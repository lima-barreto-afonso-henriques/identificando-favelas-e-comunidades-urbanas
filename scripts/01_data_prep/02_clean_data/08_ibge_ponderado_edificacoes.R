# ==============================================================================
# IBGE SOCIAL — OPÇÃO 2: DASIMETRIA PONDERADA POR EDIFICAÇÕES
# Objetivo: dasimetria clássica com variável auxiliar (ancillary variable).
# Em vez de supor densidade uniforme (Opção 1), o peso de cada hexágono no
# valor do setor é proporcional à ÁREA CONSTRUÍDA REAL que existe dentro
# dele — um hexágono sem nenhuma edificação (parque, córrego, terreno
# baldio) recebe peso zero, mesmo que ocupe metade da área do setor.
#
# A camada auxiliar usada é o Open Buildings (Google), não o OpenStreetMap:
# a cobertura de edifícios no OSM é sistematicamente mais esparsa em áreas
# pobres/informais (viés documentado de VGI — voluntários mapeiam mais onde
# têm acesso/interesse/conectividade). Usar OSM como peso ancilar faria
# exatamente o oposto do pretendido: subestimaria a massa construída das
# favelas e distorceria os indicadores sociais contra elas. O Open
# Buildings (extração por deep learning de imagens de 50cm) tem cobertura
# mais homogênea entre áreas formais e informais.
#
# O dado de Open Buildings entra aqui como PONTO (centroide + área como
# atributo), como já padronizado no script de morfologia urbana. Isso
# simplifica o algoritmo: não é preciso fatiar polígonos com dupla
# st_intersection (edificação x setor x hexágono) — basta um st_join
# ponto-em-polígono para cada malha (setor e hexágono) e agregar por área.
# Como o hexágono médio (~15.000 m²) é muito maior que uma edificação média
# (~60-90 m²), a perda de precisão por não fatiar geometrias é desprezível.
#
# LEITURA DO OPEN BUILDINGS: usa a pasta Parquet PARTICIONADA
# (94d_buildings_parquet/lat_bin=.../lon_bin=.../*.parquet), gerada uma
# única vez pelo script de extração (Seção 5, via duckdb) a partir do
# .csv.gz bruto — não um arquivo .parquet único. Por isso a leitura aqui
# usa arrow::open_dataset() (entende o particionamento Hive-style
# automaticamente), não arrow::read_parquet() (que espera um único
# arquivo e falharia contra um diretório).
#
# Duas vantagens em cima do que já ganhamos ao sair do .csv.gz:
#  1) leitura seletiva de colunas, sem materializar o arquivo inteiro em
#     disco antes de indexar (mesmo motivo do parquet de arquivo único);
#  2) PODA DE PARTIÇÃO (partition pruning): como o particionamento é por
#     lat_bin/lon_bin, filtrar por essas colunas antes do collect() faz o
#     arrow pular arquivos inteiros fora da bounding box da área de
#     estudo, sem sequer abri-los — não só menos memória, menos I/O.
#
# Se a pasta particionada ainda não existir, rode o script de extração
# (Seção 5) antes deste.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")


# ==============================================================================
# 1. FUNÇÃO PRINCIPAL
# ==============================================================================
# Interpola indicadores sociais do Censo via dasimetria ponderada por
# edificações (Open Buildings como variável ancilar).
#
# grid_proj           sf do grid H3, projetado, com coluna id_hex
# setores_proj        sf dos setores censitários, projetado, com coluna
#                     code_tract e as colunas de contagem absoluta
# edificacoes_pontos  sf de PONTOS (Open Buildings), projetado, com colunas
#                     building_area e (opcional) confidence
# colunas_contagem    vetor com os nomes das colunas de contagem absoluta
# confidence_min      filtro mínimo de confiança do Open Buildings
#                     (default 0 = sem filtro; ver limitações do artigo
#                     sobre incerteza no score de confiança do dataset)
#
# retorna um tibble com id_hex + contagens ponderadas + diagnóstico de
# fallback (setores sem nenhuma edificação mapeada)

interpolar_dasimetrico_edificacoes <- function(
  grid_proj,
  setores_proj,
  edificacoes_pontos,
  colunas_contagem,
  confidence_min = 0
) {
  faltantes <- setdiff(colunas_contagem, names(setores_proj))
  if (length(faltantes) > 0) {
    stop(
      "Colunas de contagem ausentes em 'setores_proj': ",
      paste(faltantes, collapse = ", "),
      ". Verifique se os setores censitários já têm as contagens absolutas ",
      "do Censo acopladas (script de preparação do Censo, Seção 3-4)."
    )
  }

  if (!"building_area" %in% names(edificacoes_pontos)) {
    stop(
      "Coluna 'building_area' não encontrada em 'edificacoes_pontos'. ",
      "Espera-se a saída padronizada do script de morfologia urbana ",
      "(Open Buildings), com colunas 'building_area' e, opcionalmente, ",
      "'confidence'."
    )
  }

  # filtro opcional de confiança, aplicado apenas se a coluna existir
  if (confidence_min > 0 && "confidence" %in% names(edificacoes_pontos)) {
    n_antes <- nrow(edificacoes_pontos)
    edificacoes_pontos <-
      edificacoes_pontos |>
      filter(confidence >= confidence_min)

    message(sprintf(
      "Filtro de confiança (>= %.2f) removeu %d de %d edificações (%.1f%%).",
      confidence_min,
      n_antes - nrow(edificacoes_pontos),
      n_antes,
      100 * (n_antes - nrow(edificacoes_pontos)) / n_antes
    ))
  }

  setores_proj <-
    setores_proj |>
    mutate(across(all_of(colunas_contagem), ~ coalesce(.x, 0)))

  # ----------------------------------------------------------------------
  # OTIMIZAÇÃO 1 — PRÉ-FILTRO POR BOUNDING BOX ANTES DO JOIN ESPACIAL
  # ----------------------------------------------------------------------
  # Ideia didática: um join espacial (st_join) testa, para cada edificação,
  # se ela está dentro de algum polígono — mesmo com índice espacial
  # (R-tree) por trás, quanto MAIOR o número de pontos de entrada, mais
  # candidatos o índice precisa avaliar. Se 'edificacoes_pontos' vier sem
  # filtro prévio (ex.: a UF inteira, como pode acontecer se este script
  # rodar sozinho, sem passar pelo pré-filtro do script de morfologia
  # urbana), a maior parte dos pontos está longe da área de estudo e nunca
  # vai casar com nenhum setor — mas o algoritmo só descobre isso DEPOIS de
  # testar cada um. Um filtro retangular (bounding box) é uma operação
  # muito mais barata (comparação de 4 números por ponto, sem geometria
  # complexa) e descarta a maioria dos pontos irrelevantes ANTES da parte
  # cara. Resultado: exatamente as mesmas edificações relevantes entram no
  # join seguinte — não muda o resultado, só evita trabalho desnecessário.
  bbox_area_estudo <- st_bbox(setores_proj)

  n_antes_bbox <- nrow(edificacoes_pontos)
  edificacoes_pontos <-
    edificacoes_pontos |>
    st_filter(st_as_sfc(bbox_area_estudo), .predicate = st_intersects)

  message(sprintf(
    "Pré-filtro por bounding box: %d de %d edificações permanecem (%.1f%%) antes do join espacial exato.",
    nrow(edificacoes_pontos),
    n_antes_bbox,
    100 * nrow(edificacoes_pontos) / n_antes_bbox
  ))

  # --- Etapa A: vincula cada edificação a um setor E a um hexágono ----------
  # Observação: st_intersects() e st_within() dão o mesmo resultado para
  # pontos (um ponto não tem "borda" para diferenciar dentro/tocando) mas
  # st_intersects() é o predicado mais simples de avaliar — pequena troca
  # sem custo de correção, mantida aqui por clareza de intenção.
  message("Vinculando edificações a setores censitários...")
  edif_com_setor <-
    edificacoes_pontos |>
    st_join(select(setores_proj, code_tract), join = st_intersects) |>
    filter(!is.na(code_tract))

  message("Vinculando edificações à malha H3...")
  edif_com_setor_e_hex <-
    edif_com_setor |>
    st_join(select(grid_proj, id_hex), join = st_intersects) |>
    filter(!is.na(id_hex)) |>
    st_drop_geometry()

  # ----------------------------------------------------------------------
  # OTIMIZAÇÃO 2 — UMA ÚNICA PASSAGEM DE AGREGAÇÃO EM VEZ DE DUAS
  # ----------------------------------------------------------------------
  # Ideia didática: o código original calculava "área por (setor, hexágono)"
  # e "área total por setor" com dois group_by()/summarise() separados,
  # cada um relendo do zero a tabela de edificações — que pode ter milhões
  # de linhas. Como "área total por setor" é apenas a SOMA da "área por
  # (setor, hexágono)" dentro de cada setor, dá para calcular a segunda a
  # partir da primeira (poucas linhas, uma por combinação setor×hexágono)
  # em vez de escanear a tabela grande de novo. Trocamos o segundo
  # group_by/summarise por um mutate() dentro de grupos já agregados —
  # mesmo resultado numérico, uma varredura a menos na base grande.
  area_por_setor_hex <-
    edif_com_setor_e_hex |>
    group_by(code_tract, id_hex) |>
    summarise(
      area_construida_hex = sum(building_area, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    mutate(area_construida_setor = sum(area_construida_hex)) |>
    ungroup()

  area_total_por_setor <-
    area_por_setor_hex |>
    distinct(code_tract, area_construida_setor)

  # --- Etapa C: identifica setores SEM nenhuma edificação mapeada -----------
  setores_sem_edificacao <-
    setores_proj |>
    st_drop_geometry() |>
    distinct(code_tract) |>
    anti_join(area_total_por_setor, by = "code_tract")

  n_fallback <- nrow(setores_sem_edificacao)
  pct_fallback <- 100 * n_fallback / n_distinct(setores_proj$code_tract)

  message(sprintf(
    "%d de %d setores (%.1f%%) não têm nenhuma edificação do Open Buildings mapeada dentro deles — receberão peso por ÁREA PURA como fallback.",
    n_fallback,
    n_distinct(setores_proj$code_tract),
    pct_fallback
  ))
  if (pct_fallback > 15) {
    warning(sprintf(
      "Proporção alta de setores em fallback (%.1f%%) — considere revisar a cobertura do Open Buildings na região ou reduzir 'confidence_min'.",
      pct_fallback
    ))
  }

  # --- Etapa D: peso dasimétrico para setores COM edificação -----------------
  # 'area_construida_setor' já veio embutida em 'area_por_setor_hex' pela
  # Otimização 2 acima — não precisamos mais de um left_join aqui.
  pesos_com_edificacao <-
    area_por_setor_hex |>
    mutate(
      peso_dasimetrico = area_construida_hex / area_construida_setor,
      origem_peso = "edificacao"
    ) |>
    select(
      code_tract,
      id_hex,
      peso_dasimetrico,
      origem_peso,
      area_construida_hex
    )

  # --- Etapa E: peso por área pura para setores SEM edificação (fallback) ---
  # sem essa etapa, hexágonos que só tocam um setor sem edificações mapeadas
  # ficariam zerados silenciosamente, mesmo tendo população real
  if (n_fallback > 0) {
    setores_fallback <-
      setores_proj |>
      semi_join(setores_sem_edificacao, by = "code_tract")

    grid_x_setor_fallback <-
      st_intersection(
        select(grid_proj, id_hex),
        select(setores_fallback, code_tract)
      ) |>
      mutate(area_intersecao = as.numeric(st_area(geometry))) |>
      st_drop_geometry()

    area_total_setor_fallback <-
      grid_x_setor_fallback |>
      group_by(code_tract) |>
      summarise(area_total_setor = sum(area_intersecao), .groups = "drop")

    pesos_fallback <-
      grid_x_setor_fallback |>
      left_join(area_total_setor_fallback, by = "code_tract") |>
      mutate(
        peso_dasimetrico = area_intersecao / area_total_setor,
        origem_peso = "fallback_area",
        area_construida_hex = NA_real_
      ) |>
      select(
        code_tract,
        id_hex,
        peso_dasimetrico,
        origem_peso,
        area_construida_hex
      )

    pesos_finais <- bind_rows(pesos_com_edificacao, pesos_fallback)
  } else {
    pesos_finais <- pesos_com_edificacao
  }

  # --- Etapa E-bis: hexágonos com LACUNA DE DETECÇÃO (cenário b) ------------
  # Cenário (b), descrito no comentário da Etapa E.1 abaixo, tinha uma
  # consequência não tratada até aqui: um hexágono que toca um setor COM
  # edificação mapeada, mas onde nenhuma dessas edificações cai dentro dele
  # especificamente, nunca aparece em 'area_por_setor_hex' (que só existe
  # para combinações setor×hexágono com pelo menos 1 edificação) — e também
  # não entra no fallback da Etapa E, porque esse fallback é por SETOR
  # inteiro sem nenhuma edificação, não por hexágono. Esse hexágono ficava
  # simplesmente ausente de 'pesos_finais': sem diagnóstico, e silenciosamente
  # zerado no left_join da Etapa F (mutate(coalesce(0))). Isso é uma
  # DIFERENÇA de tratamento em relação à Opção 1 (área pura), que atribui
  # peso a todo hexágono que intersecta o setor, sem depender de detecção de
  # edificação — daí a divergência ficar concentrada exatamente nesses casos.
  #
  # Aqui identificamos esses pares (setor, hexágono) via st_join/st_intersects
  # (mais barato que st_intersection, já que não precisamos da área exata:
  # o peso desses pares é zero por definição, então só a existência do par
  # importa) e os adicionamos a 'pesos_finais' com peso_dasimetrico = 0 e uma
  # origem própria — isso não muda nenhum valor interpolado (peso 0 não
  # contribui na soma ponderada da Etapa F), mas torna o cenário visível no
  # diagnóstico por hexágono (Etapa E.1), em vez de ficar misturado,
  # indistinguível, dentro do resultado final "==0 sem explicação".
  setores_com_edificacao <-
    setores_proj |>
    anti_join(setores_sem_edificacao, by = "code_tract")

  pares_hex_setor_com_edificacao <-
    st_join(
      select(grid_proj, id_hex),
      select(setores_com_edificacao, code_tract),
      join = st_intersects
    ) |>
    st_drop_geometry() |>
    filter(!is.na(code_tract)) |>
    distinct(id_hex, code_tract)

  pares_com_lacuna <-
    pares_hex_setor_com_edificacao |>
    anti_join(
      pesos_com_edificacao |> distinct(id_hex, code_tract),
      by = c("id_hex", "code_tract")
    )

  n_pares_lacuna <- nrow(pares_com_lacuna)
  message(sprintf(
    paste0(
      "%d combinações hexágono x setor têm LACUNA DE DETECÇÃO: o setor tem ",
      "edificação mapeada em algum lugar, mas nenhuma cai dentro deste ",
      "hexágono específico. Peso zero explícito atribuído (não afeta a ",
      "soma ponderada), rotulado 'sem_deteccao_no_hex' no diagnóstico."
    ),
    n_pares_lacuna
  ))

  if (n_pares_lacuna > 0) {
    pesos_lacuna <-
      pares_com_lacuna |>
      mutate(
        peso_dasimetrico = 0,
        origem_peso = "sem_deteccao_no_hex",
        area_construida_hex = 0
      ) |>
      select(
        code_tract,
        id_hex,
        peso_dasimetrico,
        origem_peso,
        area_construida_hex
      )

    pesos_finais <- bind_rows(pesos_finais, pesos_lacuna)
  }

  # --- Etapa E.1: diagnóstico de origem do peso, por hexágono ---------------
  # Um hexágono pode ser cortado por mais de um setor (id_hex aparece em
  # várias linhas de 'pesos_finais'). Aqui resumimos, por id_hex, quantos
  # desses setores contribuintes vieram do fallback de área pura (Etapa E)
  # vs. do peso por edificação (Etapa D) vs. da lacuna de detecção (Etapa
  # E-bis). Isso é o que faltava para distinguir, na comparação com a Opção 1
  # (área pura), três cenários que produzem sintomas superficiais parecidos
  # (peso baixo/zero por edificação) mas têm causas diferentes:
  #   (a) setor inteiro sem edificação mapeada -> fallback já ativado aqui
  #       -> a divergência esperada contra a Opção 1 é pequena (mesmo método)
  #   (b) setor com edificação mapeada, mas nenhuma cai dentro deste
  #       hexágono específico -> peso zero LEGÍTIMO (não é bug de NaN/divisão
  #       por zero, já que area_construida_setor > 0 aqui) -> divergência
  #       esperada contra a Opção 1 é grande, e reflete uma lacuna de
  #       detecção do Open Buildings nesse hexágono específico -> agora
  #       rotulado explicitamente (Etapa E-bis), em vez de aparecer como
  #       'n_setores_contribuintes = NA' no diagnóstico anterior
  #   (c) hexágono fora de qualquer setor (fora da área de estudo) -> nem
  #       aparece em 'pesos_finais', continua como NA no diagnóstico
  diagnostico_pesos_hex <-
    pesos_finais |>
    group_by(id_hex) |>
    summarise(
      n_setores_contribuintes = n(),
      n_setores_fallback = sum(origem_peso == "fallback_area"),
      n_setores_sem_deteccao = sum(origem_peso == "sem_deteccao_no_hex"),
      area_construida_hex_soma = sum(area_construida_hex, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      usou_fallback_area = n_setores_fallback > 0,
      fallback_parcial = usou_fallback_area &
        (n_setores_fallback < n_setores_contribuintes),
      tem_lacuna_deteccao = n_setores_sem_deteccao > 0,
      # hexágono onde TODOS os setores contribuintes só entraram via lacuna
      # de detecção (peso 0 em tudo) -> resultado final = 0 "genuíno", mas
      # agora explicável, em vez de indistinguível de "fora da área de
      # estudo" ou de um zero por outro motivo
      so_lacuna_deteccao = n_setores_sem_deteccao == n_setores_contribuintes
    )

  # --- Etapa F: transfere contagens absolutas usando o peso final -----------
  dados_censo_absolutos <-
    setores_proj |>
    st_drop_geometry() |>
    select(code_tract, all_of(colunas_contagem))

  censo_h3_dasimetrico <-
    pesos_finais |>
    left_join(dados_censo_absolutos, by = "code_tract") |>
    group_by(id_hex) |>
    summarise(
      across(
        all_of(colunas_contagem),
        ~ sum(.x * peso_dasimetrico, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  resultado <-
    grid_proj |>
    st_drop_geometry() |>
    select(id_hex) |>
    left_join(censo_h3_dasimetrico, by = "id_hex") |>
    mutate(across(all_of(colunas_contagem), ~ coalesce(.x, 0)))

  # checagem de preservação de massa, igual à Opção 1, para comparação
  # direta de qualidade entre os métodos
  for (coluna in colunas_contagem) {
    total_original <- sum(setores_proj[[coluna]], na.rm = TRUE)
    total_interpolado <- sum(resultado[[coluna]], na.rm = TRUE)
    erro_pct <- if (total_original > 0) {
      abs(total_interpolado - total_original) / total_original * 100
    } else {
      NA_real_
    }

    if (!is.na(erro_pct) && erro_pct > 2) {
      warning(sprintf(
        "[Dasimetria/Edificações] Preservação de massa falhou para '%s': original = %.0f, interpolado = %.0f (erro de %.1f%%).",
        coluna,
        total_original,
        total_interpolado,
        erro_pct
      ))
    } else {
      message(sprintf(
        "[Dasimetria/Edificações] OK — '%s': erro de preservação de massa = %.2f%%",
        coluna,
        ifelse(is.na(erro_pct), 0, erro_pct)
      ))
    }
  }

  attr(resultado, "n_setores_fallback_area_pura") <- n_fallback
  attr(resultado, "diagnostico_pesos") <- diagnostico_pesos_hex
  resultado
}


# caminho do grid hexagonal dasimétrico gerado no script de extração
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 11

arquivos_entrada <- c(
  "9" = "grid_dasimetrico_res9.rds",
  "10" = "grid_dasimetrico_res10.rds",
  "11" = "grid_dasimetrico_res11.rds"
)

# Puxa o nome do arquivo com base na seleção
arquivo_entrada <- arquivos_entrada[[as.character(resolucao_hex)]]
sufixo_saida <- paste0("_res", resolucao_hex)

# Substitua 'pasta_saida' pela variável correta onde os
# grids ficam salvos no seu projeto
caminho_do_grid <- file.path(
  pasta_saida,
  arquivo_entrada
)

if (!file.exists(caminho_do_grid)) {
  stop(
    "Arquivo '",
    arquivo_entrada,
    "' não encontrado em ",
    caminho_do_grid,
    ". ",
    "Certifique-se de gerar o grid hexagonal para a resolução ",
    resolucao_hex,
    " antes de prosseguir."
  )
}


# ==============================================================================
# 2. EXECUÇÃO (SE RODADO DIRETAMENTE, NÃO VIA source())
# ==============================================================================

if (sys.nframe() == 0) {
  # ajuste fino de sensibilidade do filtro de confiança do Open Buildings
  confidence_min_open_buildings <- 0

  arquivo_grid <- caminho_do_grid
  arquivo_setores <- file.path(pasta_saida, "ibge_censo.rds")

  # arquivo .parquet (colunar) em vez do .csv.gz bruto — ver nota no
  # cabeçalho do script sobre por que e como gerar esse .parquet
  arquivo_open_buildings <- here(
    pasta_entrada,
    "open_buildings",
    "94d_buildings_parquet"
  )

  # falha explícita se as dependências não existirem — sem fallback
  # silencioso que geraria NA nos joins seguintes
  if (!file.exists(arquivo_setores)) {
    stop(
      "'",
      arquivo_setores,
      "' não encontrado. Este script exige os ",
      "setores censitários com as contagens absolutas do Censo já ",
      "acopladas — rode o script de preparação do Censo primeiro."
    )
  }
  if (!file.exists(arquivo_open_buildings)) {
    stop(
      "Arquivo/Diretório '",
      arquivo_open_buildings,
      "' não encontrado. Este script espera o Open Buildings já ",
      "convertido para .parquet particionado. Se você só tem o ",
      ".csv.gz bruto, converta uma única vez usando arrow::write_dataset()\n"
    )
  }

  grid_base <- readRDS(arquivo_grid)
  setores_ibge <- readRDS(arquivo_setores)

  # Captura a bounding box da área de estudo em WGS84 (lat/lon)
  # para usar como filtro no Arrow (partition pruning / predicate pushdown)
  bbox_estudo <- st_bbox(st_transform(setores_ibge, crs = 4326))

  # importa as edificações como PONTOS individuais (não a agregação por
  # hexágono já salva pelo script de morfologia urbana) — este método
  # precisa da localização exata de cada edificação.
  # arrow::open_dataset() entende a pasta particionada. O filter() antes
  # do collect() faz a poda de partição/arquivos, lendo só o necessário
  # sem descomprimir/materializar o arquivo inteiro em um temporário.
  edificacoes_raw <-
    arrow::open_dataset(arquivo_open_buildings) |>
    select(longitude, latitude, area_in_meters, confidence) |>
    filter(
      latitude >= bbox_estudo["ymin"],
      latitude <= bbox_estudo["ymax"],
      longitude >= bbox_estudo["xmin"],
      longitude <= bbox_estudo["xmax"]
    ) |>
    collect() |>
    rename(building_area = area_in_meters) |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  grid_proj <-
    grid_base |>
    st_as_sf() |>
    st_transform(crs = 31983) |>
    st_make_valid()

  setores_proj <-
    setores_ibge |>
    st_transform(crs = 31983) |>
    st_make_valid()

  edificacoes_proj <-
    edificacoes_raw |>
    st_transform(crs = 31983)

  colunas_contagem <- c(
    "water_rede",
    "water_total",
    "sewer_rede",
    "sewer_total",
    "waste_rede",
    "waste_total",
    "sidewalk_rede",
    "sidewalk_total",
    "trees_rede",
    "trees_total"
  )

  resultado_bruto <-
    interpolar_dasimetrico_edificacoes(
      grid_proj,
      setores_proj,
      edificacoes_proj,
      colunas_contagem,
      confidence_min = confidence_min_open_buildings
    )

  # extrai o diagnóstico ANTES do mutate/select seguinte — atributos
  # personalizados não sobrevivem de forma garantida a verbos do dplyr
  diagnostico_pesos_hex <- attr(resultado_bruto, "diagnostico_pesos")

  ibge_social_dasimetrico_edificacoes <-
    resultado_bruto |>
    mutate(
      households_connected_to_water_supply_network = if_else(
        water_total > 0,
        water_rede / water_total,
        0
      ),
      households_connected_to_sewerage_system = if_else(
        sewer_total > 0,
        sewer_rede / sewer_total,
        0
      ),
      households_solid_waste_collection = if_else(
        waste_total > 0,
        waste_rede / waste_total,
        0
      ),
      households_with_sidewalk = if_else(
        sidewalk_total > 0,
        sidewalk_rede / sidewalk_total,
        0
      ),
      households_with_trees = if_else(
        trees_total > 0,
        trees_rede / trees_total,
        0
      )
    ) |>
    select(
      id_hex,
      households_connected_to_water_supply_network,
      households_connected_to_sewerage_system,
      households_solid_waste_collection,
      households_with_sidewalk,
      households_with_trees
    )

  glimpse(ibge_social_dasimetrico_edificacoes)

  sufixo_saida <- paste0("_res", resolucao_hex)

  saveRDS(
    ibge_social_dasimetrico_edificacoes,
    file.path(
      pasta_saida,
      paste0("ibge_social_dasimetrico_edificacoes", sufixo_saida, ".rds")
    )
  )

  # salvo à parte (não dentro do arquivo principal) para não alterar o
  # esquema de colunas que outros scripts já esperam de
  # 'ibge_social_dasimetrico_edificacoes' — este é um arquivo de apoio,
  # consumido apenas pelo script de comparação (01)
  saveRDS(
    diagnostico_pesos_hex,
    file.path(
      pasta_saida,
      paste0("diagnostico_pesos_edificacoes", sufixo_saida, ".rds")
    )
  )

  message(
    ">>> [Opção 2] Dasimetria ponderada por edificações concluída e salva."
  )
}

# FIM ==========================================================================
