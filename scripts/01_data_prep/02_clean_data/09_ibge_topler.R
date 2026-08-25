# ==============================================================================
# IBGE SOCIAL — OPÇÃO 3: INTERPOLAÇÃO PICNOFILÁTICA DE TOBLER
# Objetivo: transformar os polígonos de setor em uma SUPERFÍCIE RASTER
# CONTÍNUA e suave via suavização iterativa (Tobler, 1979), sujeita a uma
# restrição de PRESERVAÇÃO DE VOLUME: a soma dos valores da superfície
# contínua dentro de cada setor original continua batendo com o valor
# original do Censo.
#
# Elimina descontinuidades artificiais nas bordas dos setores mas, ao
# contrário da Opção 2, NÃO usa nenhuma informação real sobre onde as
# edificações estão — pode espalhar "massa populacional" sobre parques,
# represas ou rodovias entre dois pontos de maior densidade. Para o
# objetivo deste projeto (heterogeneidade intra-assentamento), é uma
# solução mais forte que a área pura (Opção 1), mas mais fraca que a
# dasimetria por edificação (Opção 2).
#
# DEPENDÊNCIA: usa o pacote {pycno}, que depende do ecossistema espacial
# legado do R (classe 'Spatial', pacote 'sp'), com suporte reduzido desde a
# aposentadoria de rgdal/rgeos/maptools. Se não estiver disponível, o
# script falha cedo com instrução clara — considere a Opção 2 como
# alternativa sem essa dependência de risco.
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================

# carrega funções e pacotes utilizados em todos os scripts do projeto
source(here::here("scripts", "00_functions", "00_setup.R"))

# caminhos padronizados de entrada e saída
pasta_entrada <- here("data", "raw")
pasta_saida <- here("data", "processed")

# dependência legada isolada — falha cedo e com instrução clara
if (!requireNamespace("pycno", quietly = TRUE)) {
  stop(
    "O pacote 'pycno' não está disponível. Tente install.packages('pycno'); ",
    "se falhar, considere usar a Opção 2 (dasimetria por edificações), que ",
    "não depende do ecossistema espacial legado do R (classe 'Spatial')."
  )
}


# ==============================================================================
# 1. FUNÇÃO AUXILIAR — INTERPOLAÇÃO PICNOFILÁTICA DE UMA ÚNICA VARIÁVEL
# ==============================================================================

interpolar_pycno_variavel <- function(
  setores_sf,
  nome_variavel,
  res_metros = 50,
  convergencia = 3,
  relaxacao = 0.2,
  verbose = FALSE
) {
  message(sprintf(
    "  -> Aplicando algoritmo picnofilático (Tobler) em '%s' (célula = %dm)...",
    nome_variavel,
    res_metros
  ))

  # reduz o objeto ao mínimo antes de converter para 'Spatial': só a
  # geometria + a variável de interesse. Evita que colunas de metadados não
  # relacionadas a este loop (percentuais, IDs) carreguem NA que quebrariam
  # o pycno mais adiante
  sp_df <-
    setores_sf |>
    select(all_of(nome_variavel)) |>
    as("Spatial")

  # pops é o vetor NUMÉRICO de valores da variável, não o nome da coluna
  # (assinatura real do pacote instalado, CRAN pycno 1.4.1)
  vetor_valores <- sp_df[[nome_variavel]]

  # substitui contagens exatamente zero por um epsilon desprezível: impede
  # a criação de "lagos de zeros" que resultam em divisão 0/0 = NaN dentro
  # do laço iterativo interno do pycno, causa raiz de falhas de convergência
  vetor_valores <- ifelse(vetor_valores == 0, 1e-4, vetor_valores)

  t0 <- Sys.time()
  raster_pycno_sp <-
    tryCatch(
      pycno::pycno(
        x = sp_df,
        pops = vetor_valores,
        celldim = res_metros,
        r = relaxacao,
        converge = convergencia,
        verbose = verbose
      ),
      error = function(e) {
        stop(sprintf(
          "Falha ao interpolar '%s' via pycno::pycno(): %s. Verifique geometrias inválidas (st_make_valid) ou valores NA na coluna.",
          nome_variavel,
          conditionMessage(e)
        ))
      }
    )
  message(sprintf(
    "     concluído em %.1f s",
    as.numeric(Sys.time() - t0, units = "secs")
  ))

  raster_terra <- rast(raster_pycno_sp)
  crs(raster_terra) <- "EPSG:31983"

  # rede de segurança: a documentação do pycno garante saída não-negativa
  # por construção, mas mantemos o clamp como defesa contra arredondamento
  # de ponto flutuante na conversão sp -> terra
  n_negativos <- sum(values(raster_terra) < 0, na.rm = TRUE)
  if (n_negativos > 0) {
    pct_negativos <- 100 * n_negativos / sum(!is.na(values(raster_terra)))
    message(sprintf(
      "     %d células (%.2f%%) com valores negativos — zeradas.",
      n_negativos,
      pct_negativos
    ))
    raster_terra <- clamp(raster_terra, lower = 0, values = TRUE)
  }

  raster_terra
}


# ==============================================================================
# 2. FUNÇÃO PRINCIPAL
# ==============================================================================
# Interpola indicadores sociais do Censo via superfície picnofilática de
# Tobler.
#
# grid_proj         sf do grid H3, projetado, com coluna id_hex
# setores_proj      sf dos setores censitários, projetado, com as colunas
#                   de contagem absoluta
# colunas_contagem  vetor com os nomes das colunas de contagem absoluta
# res_metros        resolução do raster interno, em metros (default 50m).
#                   CUIDADO: célula maior não é sinônimo de "mais leve" — o
#                   limiar de exclusão de setores pequenos (Etapa de
#                   filtragem) cresce com res_metros². Em 100m o limiar
#                   passaria da mediana de área dos setores urbanos
#                   típicos, descartando quase metade da base. 50m é um
#                   meio-termo mais seguro; reduza para 30m para recuperar
#                   mais setores pequenos (mais lento).
#
# retorna um tibble com id_hex + contagens interpoladas

interpolar_picnofilatico <- function(
  grid_proj,
  setores_proj,
  colunas_contagem,
  res_metros = 50
) {
  faltantes <- setdiff(colunas_contagem, names(setores_proj))
  if (length(faltantes) > 0) {
    stop(
      "Colunas de contagem ausentes em 'setores_proj': ",
      paste(faltantes, collapse = ", ")
    )
  }

  # aviso de custo computacional antes do loop pesado
  n_setores <- nrow(setores_proj)
  bbox_setores <- st_bbox(setores_proj)
  n_celulas_estimado <-
    ceiling((bbox_setores["xmax"] - bbox_setores["xmin"]) / res_metros) *
    ceiling((bbox_setores["ymax"] - bbox_setores["ymin"]) / res_metros)

  message(sprintf(
    "Aviso de custo: %d setores, raster de ~%s células por variável x %d variáveis. Ajuste 'res_metros' se a RAM for insuficiente.",
    n_setores,
    format(n_celulas_estimado, big.mark = ",", decimal.mark = "."),
    length(colunas_contagem)
  ))

  setores_proj <-
    setores_proj |>
    mutate(across(all_of(colunas_contagem), ~ coalesce(.x, 0)))

  # ----------------------------------------------------------------------
  # TRIAGEM DE GEOMETRIAS INVÁLIDAS/DEGENERADAS
  # o algoritmo do Tobler opera sobre a malha de adjacência dos polígonos
  # e, internamente, divide por área do setor. Um setor com geometria
  # vazia, área zero/NA, ou tipo diferente de POLYGON/MULTIPOLYGON produz
  # NaN/Inf internamente, quebrando o pacote mais adiante. st_make_valid()
  # sozinho não garante isso: corrige topologia, mas não impede resultado
  # degenerado.
  # ----------------------------------------------------------------------
  areas_setor <- as.numeric(st_area(setores_proj))
  tipos_geom <- as.character(st_geometry_type(setores_proj))
  setores_invalidos <-
    st_is_empty(setores_proj) |
    is.na(areas_setor) |
    areas_setor <= 0 |
    !tipos_geom %in% c("POLYGON", "MULTIPOLYGON")

  n_invalidos <- sum(setores_invalidos)
  if (n_invalidos > 0) {
    contagens_excluidas <-
      setores_proj |>
      st_drop_geometry() |>
      filter(setores_invalidos) |>
      summarise(across(all_of(colunas_contagem), ~ sum(.x, na.rm = TRUE)))

    message(sprintf(
      "%d de %d setores (%.2f%%) com geometria vazia, área zero/NA, ou tipo inválido — excluídos antes do pycno. Contagens perdidas nesses setores:",
      n_invalidos,
      nrow(setores_proj),
      100 * n_invalidos / nrow(setores_proj)
    ))
    print(contagens_excluidas)
    message(
      "Essas contagens NÃO entram na checagem de preservação de massa abaixo ",
      "(o total de referência já é pós-exclusão) — o erro reportado adiante ",
      "mede só a qualidade da interpolação, não essa perda por geometria ",
      "inválida."
    )

    setores_proj <- setores_proj |> filter(!setores_invalidos)
  }

  # uniformiza tudo para MULTIPOLYGON: o 'sp' (usado internamente pelo
  # pycno via as(..., "Spatial")) pode se comportar mal com mistura de
  # POLYGON puro e MULTIPOLYGON na mesma camada
  setores_proj <- setores_proj |> st_cast("MULTIPOLYGON")

  # ----------------------------------------------------------------------
  # FILTRAR SETORES MENORES QUE A ÁREA MÍNIMA CONFIÁVEL PARA 'res_metros'
  # setores menores que a célula da grade (ou com margem insuficiente)
  # geram divisão por soma-zero durante a suavização iterativa, produzindo
  # NaN que quebra a checagem de convergência do pycno. Exigimos que o
  # setor tenha ao menos 'area_minima_multiplicador' vezes a área de uma
  # célula (celldim²) para ter folga.
  # ----------------------------------------------------------------------
  area_celula <- res_metros^2
  area_minima_multiplicador <- 2
  area_minima <- area_celula * area_minima_multiplicador

  areas_pos_filtro <- as.numeric(st_area(setores_proj))
  setores_pequenos_demais <- areas_pos_filtro < area_minima

  n_pequenos <- sum(setores_pequenos_demais)
  if (n_pequenos > 0) {
    contagens_excluidas <-
      setores_proj |>
      st_drop_geometry() |>
      filter(setores_pequenos_demais) |>
      summarise(across(all_of(colunas_contagem), ~ sum(.x, na.rm = TRUE)))

    message(sprintf(
      "%d de %d setores (%.2f%%) têm área menor que %dx a área da célula (%.0f m²) — risco de instabilidade numérica no pycno. Excluídos antes da interpolação. Contagens perdidas:",
      n_pequenos,
      nrow(setores_proj),
      100 * n_pequenos / nrow(setores_proj),
      area_minima_multiplicador,
      area_minima
    ))
    print(contagens_excluidas)
    message(
      "Se essa soma for grande, 'res_metros' está grande demais para a malha ",
      "de setores desta região — reduza a resolução em vez de aumentar. ",
      "Essas contagens não entram na checagem de preservação de massa abaixo."
    )

    setores_proj <- setores_proj |> filter(!setores_pequenos_demais)
  }

  grid_spat <- vect(grid_proj)

  # ----------------------------------------------------------------------
  # OTIMIZAÇÃO — PARALELIZAR O LOOP POR VARIÁVEL
  # ----------------------------------------------------------------------
  # Ideia didática: o algoritmo de Tobler é ITERATIVO e relativamente lento
  # — para cada variável ele suaviza um raster até convergir, célula por
  # célula, repetidas vezes. Com 10 variáveis de contagem (água, esgoto,
  # resíduos, calçadas, árvores × rede/total), rodar uma de cada vez é como
  # fazer 10 tarefas independentes em fila, uma pessoa por vez.
  #
  # A interpolação da variável "water_rede" NÃO depende em nada do
  # resultado de "sewer_total" — cada chamada de interpolar_pycno_variavel()
  # recebe os mesmos dados de entrada (setores_proj) e produz uma saída
  # própria, sem nenhum estado compartilhado entre elas. Isso é chamado de
  # problema "embaraçosamente paralelo": dá para distribuir cada variável
  # para um núcleo diferente do processador e rodar todas ao mesmo tempo,
  # sem risco de uma interferir na outra e SEM abrir mão de nenhuma
  # checagem — cada worker roda a função inteira, com as mesmas validações
  # internas, e devolve só o resultado final (uma tabela pequena), nunca o
  # raster bruto (que não viaja bem entre processos).
  #
  # Se o pacote {furrr} não estiver instalado, o script cai automaticamente
  # para o loop sequencial original — a paralelização é um ganho de
  # velocidade opcional, nunca um requisito para o script funcionar.
  paralelizar <- requireNamespace("furrr", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)

  if (paralelizar) {
    n_workers <- min(
      length(colunas_contagem),
      max(1, parallel::detectCores() - 1)
    )
    message(sprintf(
      "Paralelizando a interpolação de %d variáveis em %d processos (pacote {furrr})...",
      length(colunas_contagem),
      n_workers
    ))
    # 'multisession' abre processos R separados — funciona em qualquer
    # sistema operacional (Windows incluso), ao custo de cada processo
    # carregar sua própria cópia de 'setores_proj' na memória. Em Linux/Mac,
    # future::plan(multicore) usa fork (compartilha memória, mais leve) —
    # troque aqui se estiver rodando fora do Windows e quiser economizar RAM.
    future::plan(future::multisession, workers = n_workers)
    on.exit(future::plan(future::sequential), add = TRUE)

    mapa_variavel <- furrr::future_map(
      colunas_contagem,
      function(variavel) {
        raster_var <- interpolar_pycno_variavel(
          setores_proj,
          variavel,
          res_metros = res_metros
        )
        terra::extract(raster_var, grid_spat, fun = "sum", na.rm = TRUE)[, 2]
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
    names(mapa_variavel) <- colunas_contagem
  } else {
    message(
      "Pacotes {furrr}/{future} não encontrados — rodando a interpolação de ",
      "forma sequencial (mais lento, mas funciona igual). Para acelerar, ",
      "instale com install.packages(c('furrr', 'future'))."
    )
    mapa_variavel <- list()
    for (variavel in colunas_contagem) {
      raster_var <- interpolar_pycno_variavel(
        setores_proj,
        variavel,
        res_metros = res_metros
      )
      mapa_variavel[[variavel]] <- terra::extract(
        raster_var,
        grid_spat,
        fun = "sum",
        na.rm = TRUE
      )[, 2]
    }
  }

  # trata hexágonos fora da extensão do raster (NA na extração) como 0,
  # com a contagem reportada — igual ao comportamento original, agora
  # aplicado depois da etapa paralela (ou sequencial) de extração
  resultados_por_variavel <- list()
  for (variavel in colunas_contagem) {
    valores <- mapa_variavel[[variavel]]

    n_na <- sum(is.na(valores))
    if (n_na > 0) {
      message(sprintf(
        "     %d hexágonos fora da extensão do raster para '%s' — definidos como 0.",
        n_na,
        variavel
      ))
    }
    valores <- coalesce(valores, 0)

    resultados_por_variavel[[variavel]] <- tibble(
      id_hex = grid_proj$id_hex,
      !!variavel := valores
    )
  }

  resultado <- reduce(resultados_por_variavel, left_join, by = "id_hex")

  # checagem de preservação de massa, igual às Opções 1 e 2, para comparação
  # direta de qualidade entre os três métodos
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
        "[Picnofilático] Preservação de massa falhou para '%s': original = %.0f, interpolado = %.0f (erro de %.1f%%). Esperado: pequeno erro numérico de convergência + hexágonos fora da extensão; erro > 2%% merece investigação.",
        coluna,
        total_original,
        total_interpolado,
        erro_pct
      ))
    } else {
      message(sprintf(
        "[Picnofilático] OK — '%s': erro de preservação de massa = %.2f%%",
        coluna,
        ifelse(is.na(erro_pct), 0, erro_pct)
      ))
    }
  }

  resultado
}


# ==============================================================================
# 3. EXECUÇÃO (SE RODADO DIRETAMENTE, NÃO VIA source())
# ==============================================================================

if (sys.nframe() == 0) {
  # código do município (ou lista de códigos, para regiões metropolitanas)
  # ex.: 3550308 = São Paulo (SP)
  codigo_municipio <- "3550308"

  # resolução do raster interno do pycno, em metros (ver documentação da
  # função interpolar_picnofilatico() acima para os limites de segurança)
  resolucao_pycno_metros <- 50

  arquivo_grid <- file.path(pasta_saida, "grid_dasimetrico.rds")
  arquivo_setores <- file.path(pasta_saida, "ibge_censo.rds")

  # falha explícita se a base de setores não existir — sem fallback
  # silencioso via geobr puro, que não traria as colunas de contagem
  if (!file.exists(arquivo_setores)) {
    stop(
      "'",
      arquivo_setores,
      "' não encontrado. Este script exige os ",
      "setores censitários com as contagens absolutas do Censo já ",
      "acopladas — rode o script de preparação do Censo primeiro."
    )
  }

  grid_base <- readRDS(arquivo_grid)
  setores_ibge <- readRDS(arquivo_setores)

  grid_proj <-
    grid_base |>
    st_as_sf() |>
    st_transform(crs = 31983) |>
    st_make_valid()

  setores_proj <-
    setores_ibge |>
    st_transform(crs = 31983) |>
    st_make_valid()

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
    interpolar_picnofilatico(
      grid_proj,
      setores_proj,
      colunas_contagem,
      res_metros = resolucao_pycno_metros
    )

  ibge_social_picnofilatico_tobler <-
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

  glimpse(ibge_social_picnofilatico_tobler)

  saveRDS(
    ibge_social_picnofilatico_tobler,
    file.path(pasta_saida, "ibge_social_picnofilatico_tobler.rds")
  )

  message(
    ">>> [Opção 3] Interpolação picnofilática de Tobler concluída e salva."
  )
}

# FIM ==========================================================================
