# ==============================================================================
# COMPARAÇÃO — DASIMETRIA POR ÁREA PURA x POR EDIFICAÇÕES
# ==============================================================================

# ==============================================================================
# 0. CARREGAR PACOTES E DEFINIR CAMINHOS
# ==============================================================================
source(here::here("scripts", "00_functions", "00_setup.R"))
source(here::here("scripts", "00_functions", "01_utils.R"))

# ----------------------------------------------------------------------
# SELETOR DE RESOLUÇÃO
# ----------------------------------------------------------------------
# <- mude aqui (9, 10 ou 11) e rode de novo a cada vez
resolucao_hex <- 11

pasta_saida <- here("data", "processed")
pasta_outputs <- here("outputs", "comparacao_dasimetria")

if (!dir.exists(pasta_outputs)) {
  dir.create(pasta_outputs, recursive = TRUE)
}

sufixo <- paste0("_res", resolucao_hex)

arquivo_area_pura <- file.path(
  pasta_saida,
  paste0("ibge_social_area_pura", sufixo, ".rds")
)

arquivo_edificacoes <- file.path(
  pasta_saida,
  paste0("ibge_social_dasimetrico_edificacoes", sufixo, ".rds")
)

arquivo_grid_hexagonal_area_pura <- file.path(
  pasta_saida,
  paste0("grid_hexagonal_area_pura", sufixo, ".rds")
)

arquivo_grid_hexagonal_ponderado_edificacoes <- file.path(
  pasta_saida,
  paste0("grid_hexagonal_ponderado_edificacoes", sufixo, ".rds")
)

# gerado pela versão atualizada do script 08 (Etapa E.1) — opcional: se
# não existir ainda (script 08 não foi rerodado), seguimos sem a
# estratificação por fallback, com aviso, em vez de travar o script
arquivo_diagnostico_fallback <- file.path(
  pasta_saida,
  paste0("diagnostico_pesos_edificacoes", sufixo, ".rds")
)

if (!file.exists(arquivo_area_pura)) {
  stop(
    "Arquivo '",
    basename(arquivo_area_pura),
    "' não encontrado em ",
    pasta_saida,
    ". Rode o script de dasimetria por área pura (Opção 1) ",
    "para a resolução ",
    resolucao_hex,
    " antes de prosseguir."
  )
}

if (!file.exists(arquivo_edificacoes)) {
  stop(
    "Arquivo '",
    basename(arquivo_edificacoes),
    "' não encontrado em ",
    pasta_saida,
    ". Rode o script de dasimetria por edificações (Opção 2) ",
    "para a resolução ",
    resolucao_hex,
    " antes de prosseguir."
  )
}

if (!file.exists(arquivo_grid_hexagonal_area_pura)) {
  stop(
    "Arquivo '",
    basename(arquivo_grid_hexagonal_area_pura),
    "' não encontrado em ",
    pasta_saida,
    ". Necessário para os pesos (number_of_buildings/",
    "number_private_residences) usados no diagnóstico de divergência."
  )
}

if (!file.exists(arquivo_grid_hexagonal_ponderado_edificacoes)) {
  stop(
    "Arquivo '",
    basename(arquivo_grid_hexagonal_ponderado_edificacoes),
    "' não encontrado em ",
    pasta_saida,
    ". Necessário para os pesos (number_of_buildings/",
    "number_private_residences) usados no diagnóstico de divergência."
  )
}

# ----------------------------------------------------------------------
# CARREGAMENTO
# ----------------------------------------------------------------------
dasim_area <-
  readRDS(arquivo_area_pura) |>
  as_tibble()
glimpse(dasim_area)

dasim_edif <-
  readRDS(arquivo_edificacoes) |>
  as_tibble()
glimpse(dasim_edif)

colunas_peso <- c("number_of_buildings", "number_private_residences")

# CORREÇÃO: 'number_of_buildings'/'number_private_residences' NÃO estão em
# dasim_area/dasim_edif (confirmado por glimpse(): só as 5 variáveis de
# infraestrutura + id_hex). Vivem nos grids hexagonais de cada método —
# que o script já apontava (arquivo_grid_hexagonal_*) mas nunca lia. O
# bloco anterior usava 'arquivo_open_buildings', uma variável nunca
# definida em lugar nenhum — removido.
#
# Lemos os pesos dos DOIS grids (não só um): além de servir de diagnóstico
# para o top_divergentes, isso testa de graça se os dois métodos sequer
# concordam sobre quantas edificações/residências cada hexágono tem — se
# não concordarem, é outro sintoma de inconsistência entre os pipelines,
# igual ao 'full_join' + aviso de hexágonos sem par que o script já faz
# para as variáveis de infraestrutura.
pesos_area <-
  readRDS(arquivo_grid_hexagonal_area_pura) |>
  sf::st_drop_geometry() |>
  as_tibble()
glimpse(pesos_area)

pesos_edif <-
  readRDS(arquivo_grid_hexagonal_ponderado_edificacoes) |>
  sf::st_drop_geometry() |>
  as_tibble()
glimpse(pesos_edif)

faltantes_peso_area <- setdiff(colunas_peso, names(pesos_area))
faltantes_peso_edif <- setdiff(colunas_peso, names(pesos_edif))

if (length(faltantes_peso_area) > 0 || length(faltantes_peso_edif) > 0) {
  stop(
    "Colunas de peso esperadas ausentes nos grids hexagonais. ",
    "Área pura: ",
    paste(faltantes_peso_area, collapse = ", "),
    " | Edificações: ",
    paste(faltantes_peso_edif, collapse = ", ")
  )
}

pesos_area <- pesos_area |> select(id_hex, all_of(colunas_peso))
pesos_edif <- pesos_edif |> select(id_hex, all_of(colunas_peso))

# Diagnóstico: os dois grids concordam sobre os pesos? Se 'diferenca_pesos'
# não for zero em algum hexágono, os dois métodos de dasimetria estão
# partindo de contagens de edificação/residência DIFERENTES para o mesmo
# hexágono — o que por si só já explicaria parte da divergência nas
# variáveis de infraestrutura, sem precisar invocar diferença metodológica
# na ponderação em si.
checagem_pesos <-
  pesos_area |>
  rename_with(~ paste0(.x, "_area"), all_of(colunas_peso)) |>
  full_join(
    pesos_edif |> rename_with(~ paste0(.x, "_edif"), all_of(colunas_peso)),
    by = "id_hex"
  ) |>
  mutate(
    diff_number_of_buildings = number_of_buildings_edif -
      number_of_buildings_area,
    diff_number_private_residences = number_private_residences_edif -
      number_private_residences_area
  )

n_pesos_divergentes <-
  sum(checagem_pesos$diff_number_of_buildings != 0, na.rm = TRUE) +
  sum(checagem_pesos$diff_number_private_residences != 0, na.rm = TRUE)

if (n_pesos_divergentes > 0) {
  message(sprintf(
    paste0(
      "[aviso] Os grids hexagonais dos dois métodos divergem em %d ",
      "combinações hexágono x variável de peso (number_of_buildings / ",
      "number_private_residences). Isso sozinho já pode explicar parte ",
      "da divergência nas variáveis de infraestrutura abaixo — ver ",
      "'checagem_pesos' para os casos específicos."
    ),
    n_pesos_divergentes
  ))
} else {
  message(
    "[ok] Os dois grids concordam integralmente sobre number_of_buildings ",
    "e number_private_residences — a divergência nas variáveis de ",
    "infraestrutura (se houver) não vem de contagens de peso diferentes."
  )
}

# Peso único usado no diagnóstico do top_divergentes: preferimos o do
# grid 'ponderado_edificacoes', por ser o método diretamente associado à
# hipótese de instabilidade por denominador pequeno (poucas edificações
# detectadas). Se os dois grids concordarem (caso 'ok' acima), tanto faz
# qual dos dois usar aqui.
grid_pesos <- pesos_edif

# ----------------------------------------------------------------------
# DIAGNÓSTICO DE FALLBACK (Etapa E.1 do script 08)
# ----------------------------------------------------------------------
# 'number_of_buildings = 0' num hexágono pode significar duas coisas bem
# diferentes, que o script 08 agora distingue:
#   (a) o SETOR inteiro não tinha edificação mapeada -> o 08 já usou área
#       pura ali (fallback) -> divergência esperada contra a Opção 1 é
#       PEQUENA, os dois métodos deveriam quase coincidir
#   (b) o setor TEM edificação mapeada, só que nenhuma cai dentro deste
#       hexágono específico -> peso zero LEGÍTIMO, não é fallback -> essa
#       é a divergência genuína, e reflete uma lacuna de detecção do Open
#       Buildings específica daquele hexágono
# Sem essa distinção, os dois cenários ficam misturados sob o mesmo
# rótulo ("number_of_buildings baixo -> instabilidade"), o que mistura um
# caso onde o método já se protegeu com outro onde a divergência é real.
if (file.exists(arquivo_diagnostico_fallback)) {
  diagnostico_fallback <-
    readRDS(arquivo_diagnostico_fallback) |>
    as_tibble() |>
    select(
      id_hex,
      n_setores_contribuintes,
      n_setores_fallback,
      usou_fallback_area,
      fallback_parcial,
      # novos campos da Etapa E-bis do script 08 (opcional, para
      # retrocompatibilidade com diagnósticos gerados antes dessa etapa
      # existir): se ausentes, ficam como NA em vez de travar o script
      any_of(c(
        "n_setores_sem_deteccao",
        "tem_lacuna_deteccao",
        "so_lacuna_deteccao"
      ))
    )
  glimpse(diagnostico_fallback)
} else {
  warning(sprintf(
    paste0(
      "'%s' não encontrado — rode a versão atualizada do script 08 para ",
      "gerá-lo. Prosseguindo SEM a estratificação por fallback: a leitura ",
      "de 'top_divergentes' abaixo não vai distinguir hexágonos onde o ",
      "fallback já explica a baixa divergência esperada de hexágonos ",
      "onde a divergência é genuína."
    ),
    basename(arquivo_diagnostico_fallback)
  ))
  diagnostico_fallback <- NULL
}

colunas_comparar <- c(
  "households_connected_to_water_supply_network",
  "households_connected_to_sewerage_system",
  "households_solid_waste_collection",
  "households_with_sidewalk",
  "households_with_trees"
)

faltantes_area <- setdiff(colunas_comparar, names(dasim_area))
faltantes_edif <- setdiff(colunas_comparar, names(dasim_edif))

if (length(faltantes_area) > 0 || length(faltantes_edif) > 0) {
  stop(
    "Colunas esperadas ausentes. Área pura: ",
    paste(faltantes_area, collapse = ", "),
    " | Edificações: ",
    paste(faltantes_edif, collapse = ", ")
  )
}

# ----------------------------------------------------------------------
# JOIN E DIFERENÇAS
# ----------------------------------------------------------------------
# full_join (não inner) para não esconder hexágonos que só existem em
# um dos dois métodos — isso por si só já é um sinal de inconsistência
# entre os pipelines que vale a pena checar
comparacao <-
  dasim_area |>
  select(id_hex, all_of(colunas_comparar)) |>
  rename_with(~ paste0(.x, "_area"), all_of(colunas_comparar)) |>
  full_join(
    dasim_edif |>
      select(id_hex, all_of(colunas_comparar)) |>
      rename_with(~ paste0(.x, "_edif"), all_of(colunas_comparar)),
    by = "id_hex"
  )

n_so_area <- sum(is.na(comparacao[[paste0(colunas_comparar[1], "_edif")]]))
n_so_edif <- sum(is.na(comparacao[[paste0(colunas_comparar[1], "_area")]]))

if (n_so_area > 0 || n_so_edif > 0) {
  warning(sprintf(
    "%d hexágonos só existem na versão 'área pura' e %d só na versão 'edificações' (id_hex sem par) — excluídos das estatísticas de diferença abaixo, mas mantidos no CSV completo.",
    n_so_area,
    n_so_edif
  ))
}

for (col in colunas_comparar) {
  col_area <- paste0(col, "_area")
  col_edif <- paste0(col, "_edif")
  col_diff <- paste0(col, "_diff") # edif - area (sinal importa)
  col_absdiff <- paste0(col, "_absdiff")
  comparacao[[col_diff]] <- comparacao[[col_edif]] - comparacao[[col_area]]
  comparacao[[col_absdiff]] <- abs(comparacao[[col_diff]])
}

if (!is.null(diagnostico_fallback)) {
  comparacao <- comparacao |> left_join(diagnostico_fallback, by = "id_hex")
  # hexágonos sem nenhum setor contribuinte (fora de 'pesos_finais' no 08,
  # ex.: hexágono totalmente fora da área de estudo) não são fallback nem
  # não-fallback — ficam como NA, tratados à parte no resumo estratificado
}

# ----------------------------------------------------------------------
# RESUMO POR VARIÁVEL
# ----------------------------------------------------------------------
resumo_diferencas <-
  purrr::map_dfr(colunas_comparar, function(col) {
    diffs <- comparacao[[paste0(col, "_diff")]]
    diffs <- diffs[!is.na(diffs)]
    absdiffs <- abs(diffs)
    tibble(
      variavel = col,
      n_hexagonos_comparados = length(diffs),
      diferenca_media = mean(diffs),
      diferenca_media_absoluta = mean(absdiffs),
      diferenca_mediana_absoluta = median(absdiffs),
      diferenca_max_absoluta = max(absdiffs),
      pct_hex_diferenca_acima_10pp = mean(absdiffs > 0.10) * 100,
      correlacao_pearson = cor(
        comparacao[[paste0(col, "_area")]],
        comparacao[[paste0(col, "_edif")]],
        use = "complete.obs"
      )
    )
  })

message(
  "=== Resumo das diferenças (edificações - área pura), resolução ",
  resolucao_hex,
  " ==="
)
print(resumo_diferencas, n = Inf)

# ----------------------------------------------------------------------
# RESUMO ESTRATIFICADO POR FALLBACK (hipótese (a) vs (b) acima)
# ----------------------------------------------------------------------
resumo_diferencas_por_fallback <- NULL
tem_diag_lacuna <-
  !is.null(diagnostico_fallback) &&
  "tem_lacuna_deteccao" %in% names(diagnostico_fallback)

if (!is.null(diagnostico_fallback)) {
  # categoria de 3 níveis em vez do TRUE/FALSE anterior — separa
  # explicitamente o cenário (b) (lacuna de detecção) do cenário (a)
  # (fallback de setor inteiro), que antes ficavam ambos sob 'FALSE' ou
  # then indistinguíveis dentro do grupo não-fallback
  comparacao_categorizada <-
    comparacao |>
    # garante a coluna mesmo com diagnóstico de versão antiga do script 08
    # (sem a Etapa E-bis) — evita erro de "objeto não encontrado" no
    # case_when abaixo; nesse caso, tudo cai em 'edificacao_normal' por
    # omissão, igual ao comportamento anterior a esta mudança
    (\(.) {
      if (!tem_diag_lacuna) mutate(., tem_lacuna_deteccao = FALSE) else .
    })() |>
    mutate(
      categoria_peso = case_when(
        is.na(usou_fallback_area) ~ NA_character_,
        usou_fallback_area ~ "fallback_setor_inteiro",
        tem_lacuna_deteccao ~ "lacuna_deteccao_no_hex",
        TRUE ~ "edificacao_normal"
      )
    )

  resumo_diferencas_por_fallback <-
    purrr::map_dfr(colunas_comparar, function(col) {
      dados_col <-
        comparacao_categorizada |>
        filter(
          !is.na(.data[[paste0(col, "_diff")]]),
          !is.na(categoria_peso)
        )

      dados_col |>
        group_by(categoria_peso) |>
        summarise(
          n_hexagonos = n(),
          diferenca_media_absoluta = mean(abs(.data[[paste0(col, "_diff")]])),
          diferenca_mediana_absoluta = median(abs(.data[[paste0(
            col,
            "_diff"
          )]])),
          .groups = "drop"
        ) |>
        mutate(variavel = col, .before = 1)
    })

  # hexágonos fora de qualquer setor (categoria_peso == NA) — cenário (c),
  # continuam fora do resumo estratificado, mas contamos aqui para não
  # ficarem invisíveis
  n_fora_da_area <- sum(is.na(comparacao_categorizada$categoria_peso))

  message(
    "\n=== Diferença média absoluta por categoria de origem do peso ",
    "(fallback_setor_inteiro / lacuna_deteccao_no_hex / edificacao_normal) ==="
  )
  print(resumo_diferencas_por_fallback, n = Inf)

  message(sprintf(
    paste0(
      "\n%d hexágonos não têm nenhum setor contribuinte (fora da área de ",
      "estudo coberta pelo Censo) e ficam fora deste resumo estratificado, ",
      "mas continuam no resumo geral e no CSV completo."
    ),
    n_fora_da_area
  ))

  message(
    "\nLEITURA: 'fallback_setor_inteiro' deveria ter diferença média ",
    "próxima de zero (mesmo método que a Opção 1 nesses setores). ",
    "'lacuna_deteccao_no_hex' concentra a divergência genuína — hexágonos ",
    "onde o setor tem edificação mapeada, mas nenhuma cai especificamente ",
    "ali, então a Opção 2 zera o peso enquanto a Opção 1 (área pura) não. ",
    "'edificacao_normal' é o caso comum, com edificação detectada dentro ",
    "do próprio hexágono."
  )
}

# ----------------------------------------------------------------------
# HEXÁGONOS DE MAIOR DIVERGÊNCIA (para inspeção qualitativa/mapa)
# ----------------------------------------------------------------------
# soma das diferenças absolutas das 5 variáveis, para achar de forma
# simples os hexágonos onde os dois métodos mais discordam no conjunto
comparacao <-
  comparacao |>
  mutate(
    divergencia_total = rowSums(
      across(ends_with("_absdiff")),
      na.rm = TRUE
    )
  )

# CORREÇÃO: o left_join anterior buscava number_of_buildings/
# number_private_residences em 'dasim_edif', que nunca teve essas
# colunas — agora usa 'grid_pesos' (Seção de carregamento acima).
colunas_diagnostico_extra <- if (!is.null(diagnostico_fallback)) {
  c(
    "n_setores_contribuintes",
    "n_setores_fallback",
    "usou_fallback_area",
    "fallback_parcial"
  )
} else {
  character(0)
}

top_divergentes <-
  comparacao |>
  filter(!is.na(divergencia_total)) |>
  left_join(grid_pesos, by = "id_hex") |>
  arrange(desc(divergencia_total)) |>
  slice_head(n = 20) |>
  select(
    id_hex,
    divergencia_total,
    number_of_buildings,
    number_private_residences,
    all_of(colunas_diagnostico_extra),
    ends_with("_diff")
  )

message("\n=== Top 20 hexágonos mais divergentes entre os dois métodos ===")
print(top_divergentes, n = Inf, width = Inf)

if (!is.null(diagnostico_fallback)) {
  msg_lacuna <- if (tem_diag_lacuna) {
    paste0(
      "'tem_lacuna_deteccao == TRUE' aponta o cenário (b) diretamente — ",
      "divergência genuína por lacuna de detecção do Open Buildings NESTE ",
      "hexágono específico, mesmo o setor tendo edificação mapeada em ",
      "outro lugar. Se um hexágono aparecer aqui com 'n_setores_contribuintes ",
      "= NA', ele está fora de qualquer setor (cenário (c), fora da área ",
      "de estudo) — não confundir com (b)."
    )
  } else {
    paste0(
      "Este diagnóstico ainda não tem a Etapa E-bis do script 08 (rode a ",
      "versão atualizada para regenerar 'diagnostico_pesos_edificacoes'): ",
      "'usou_fallback_area == FALSE' com 'n_setores_contribuintes = NA' pode ",
      "ser tanto o cenário (b) (lacuna de detecção) quanto (c) (fora da ",
      "área de estudo) — indistinguíveis sem o campo 'tem_lacuna_deteccao'."
    )
  }
  message(
    "\nLEITURA: para cada hexágono do top 20, 'usou_fallback_area == TRUE' ",
    "aponta o cenário (a) — pelo menos um setor contribuinte já usou área ",
    "pura como fallback, então a divergência ali NÃO deveria ser grande; ",
    "se aparecer aqui mesmo assim, vale investigar como um bug/mistura ",
    "inesperada. ",
    msg_lacuna
  )
} else {
  message(
    "\nLEITURA (parcial — sem 'diagnostico_pesos_edificacoes'): se ",
    "'number_of_buildings' e/ou 'number_private_residences' estiverem ",
    "sistematicamente baixos nesses 20 hexágonos, a divergência é ",
    "consistente com peso zero por ausência de edificação detectada, mas ",
    "sem o diagnóstico de fallback não dá para saber se isso já era ",
    "esperado (setor inteiro em fallback) ou se é uma lacuna de detecção ",
    "pontual. Rode o script 08 atualizado para obter essa distinção."
  )
}

# ----------------------------------------------------------------------
# SALVAR RESULTADOS
# ----------------------------------------------------------------------

glimpse(comparacao)
skimr::skim(comparacao)
saveRDS(
  comparacao,
  file.path(
    pasta_outputs,
    paste0("comparacao_area_x_edificacoes", sufixo, ".rds")
  )
)

glimpse(resumo_diferencas)
skimr::skim(resumo_diferencas)
readr::write_csv(
  resumo_diferencas,
  file.path(pasta_outputs, paste0("resumo_diferencas", sufixo, ".csv"))
)

if (!is.null(resumo_diferencas_por_fallback)) {
  glimpse(resumo_diferencas_por_fallback)
  readr::write_csv(
    resumo_diferencas_por_fallback,
    file.path(
      pasta_outputs,
      paste0("resumo_diferencas_por_fallback", sufixo, ".csv")
    )
  )
}

glimpse(top_divergentes)
skimr::skim(top_divergentes)
readr::write_csv(
  top_divergentes,
  file.path(pasta_outputs, paste0("top_divergentes", sufixo, ".csv"))
)

glimpse(checagem_pesos)
skimr::skim(checagem_pesos)
readr::write_csv(
  checagem_pesos,
  file.path(pasta_outputs, paste0("checagem_pesos", sufixo, ".csv"))
)

# FIM ===================================================================
