# ============================================================
# EXPORTAR DIAGRAMA grViz - VERSÃO AJUSTADA PARA O ARTIGO (2 colunas, 17 cm)
# Mudanças em relação à versão anterior:
#  - caixas com TAMANHO FIXO (fixedsize = true), calculado com a fonte mais larga
#    possível (DejaVu Sans Bold): o texto cabe mesmo que o R/rsvg troque a fonte
#  - largura uniforme por coluna e mais espaço entre colunas (nodesep = 0.55)
#  - fontes/espaçamentos compactados (texto ~6 pt na página; antes ~4,4 pt)
#  - paralelogramos com inclinação menor (skew = 0.25)
#  - exportação só com a LARGURA: a altura segue a proporção do desenho,
#    sem faixas brancas em cima/embaixo
# OBS.: o PNG/PDF entregues foram gerados com o Graphviz (dot); este script não
# foi executado no R, mas o DOT é o mesmo (testado também com DejaVu Sans Bold).
# ============================================================

source(here::here("scripts", "00_functions", "00_setup.R"))

gr <- grViz(
      "

digraph flowchart {

  # --- CONFIGURAÇÕES GERAIS DE ESPAÇAMENTO E COMPRESSÃO ---
  graph [layout = dot, rankdir = TB,
         nodesep = 0.55,        
         ranksep = 0.3,        
         compound = true,
         newrank = true,
         splines = polyline,
         ratio = auto, pad = 0.03]

  # --- PROPRIEDADES PADRÃO (FONTES GRANDES E LEGÍVEIS) ---
  node [fontname = 'Helvetica-Bold', fontsize = 24, margin = '0.12,0.06', width = 1.9]
  edge [fontname = 'Helvetica-Bold', fontsize = 14, penwidth = 2.5,
        arrowsize = 1.0, color = '#000000']

  # ====================================================
  # 1. UNIDADE DE ANÁLISE
  # ====================================================
  subgraph cluster_unit {
    label = 'Unidade de Análise'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = polygon, sides = 4, skew = 0.25, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold', width = 1.9]

    h3 [label = 'Grades Hexagonais H3', group = 'col_top', width = 5.247, height = 0.639, fixedsize = true]
  }

  node [shape = box, style = filled, fillcolor = '#C9DAF8',
        color = '#4A86E8', fontname = 'Helvetica-Bold', width = 1.9]
  spatial_int [label = 'Interseção Espacial', group = 'col_top', width = 4.144, height = 0.639, fixedsize = true]

  # ====================================================
  # 2. DADOS DE ENTRADA (Coluna 1)
  # ====================================================
  subgraph cluster_input {
    label = 'Dados de Entrada'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = polygon, sides = 4, skew = 0.25, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold', width = 1.9]

    cnefe [label = 'Pontos CNEFE', group = 'col1', width = 4.661, height = 0.639, fixedsize = true]
    census [label = 'Setores Censitários', group = 'col1', width = 4.661, height = 0.639, fixedsize = true]
    building [label = 'Polígonos de\\nEdificações', group = 'col1', width = 4.661, height = 1.056, fixedsize = true]
    slum [label = 'Polígonos de\\nFavelas', group = 'col1', width = 4.661, height = 1.056, fixedsize = true]
    osm [label = 'Linhas Vetoriais\\n(OSM)', group = 'col1', width = 4.661, height = 1.056, fixedsize = true]
    dsm [label = 'MDS', group = 'col1', width = 1.839, height = 0.639, fixedsize = true]
    slope [label = 'Declividade', group = 'col1', width = 3.179, height = 0.639, fixedsize = true]
  }

  # ====================================================
  # 3. PRÉ-PROCESSAMENTO (Coluna 2)
  # ====================================================
  subgraph cluster_preprocessing {
    label = 'Pré-processamento'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = box, style = filled, fillcolor = '#C9DAF8',
          color = '#4A86E8', fontname = 'Helvetica-Bold', width = 1.9]

    dasymetric [label = 'Mapeamento\\nDasimétrico', group = 'col2', width = 4.343, height = 1.056, fixedsize = true]
    density [label = 'Densidade de Área', group = 'col2', width = 4.343, height = 0.639, fixedsize = true]
    poly_count [label = 'Contagem, média e\\ndesvio padrão de\\npolígonos', group = 'col2', width = 4.343, height = 1.472, fixedsize = true]
    occupies [label = 'Se ocupa 70%\\nda área', group = 'col2', width = 4.343, height = 1.056, fixedsize = true]
    buffer [label = 'Buffer de 100m', group = 'col2', width = 4.343, height = 0.639, fixedsize = true]
    zonal [label = 'Estatística Zonal\\n(Média)', group = 'col2', width = 4.343, height = 1.056, fixedsize = true]
  }

  # ====================================================
  # 4. CONJUNTO DE DADOS (Centro)
  # ====================================================
  node [shape = polygon, sides = 4, skew = 0.25, style = filled, fillcolor = '#FFF2CC',
        color = '#D6B656', fontname = 'Helvetica-Bold', width = 1.9]
  dataset [label = 'Conjunto de Dados\\nde Feições\\nHexagonais', group = 'col_mid', width = 5.001, height = 1.472, fixedsize = true]

  # ====================================================
  # 5. ANÁLISE DESCRITIVA
  # ====================================================
  node [shape = box, style = filled, fillcolor = '#FADCD9',
        color = '#D9534F', fontname = 'Helvetica-Bold']
  desc_analysis [label = 'Análise Descritiva', group = 'col3', width = 3.883, height = 0.639, fixedsize = true]

  # ====================================================
  # 6. CLASSIFICAÇÃO (Coluna 3)
  # ====================================================
  subgraph cluster_classification {
    label = 'Classificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold', width = 1.9]

    model_sel [label = 'Seleção do\\nModelo', group = 'col3', width = 3.006, height = 1.056, fixedsize = true]
    model_run [label = 'Execução do\\nModelo', group = 'col3', width = 3.006, height = 1.056, fixedsize = true]
    feat_impact [label = 'Impacto das\\nVariáveis', group = 'col3', width = 3.006, height = 1.056, fixedsize = true]
  }

  # ====================================================
  # 7. AVALIAÇÃO DA CLASSIFICAÇÃO (Coluna 4 - Topo)
  # ====================================================
  subgraph cluster_evaluation {
    label = 'Avaliação da\\nClassificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold', width = 1.9]

    qual_quant [label = 'Qualitativa e\\nQuantitativa', group = 'col4', width = 3.883, height = 1.056, fixedsize = true]
  }

  # ====================================================
  # 8. ANÁLISE DE RESULTADOS (Coluna 4 - Baixo)
  # ====================================================
  subgraph cluster_results {
    label = 'Análise de\\nResultados'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2.5; fontname = 'Helvetica-Bold'; fontsize = 26; margin = 10

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold', width = 1.9]

    kmeans [label = 'k-means', group = 'col4', width = 3.883, height = 0.639, fixedsize = true]
    desc_results [label = 'Análise Descritiva', group = 'col4', width = 3.883, height = 0.639, fixedsize = true]
  }

  # --- ALINHAMENTOS INVISÍVEIS ---
  cnefe -> census -> building -> slum -> osm -> dsm [style = invis]
  dasymetric -> density -> poly_count -> occupies -> buffer -> zonal [style = invis]
  desc_analysis -> model_sel -> model_run -> feat_impact [style = invis]
  qual_quant -> kmeans -> desc_results [style = invis]

  { rank = same; cnefe; dasymetric; }
  { rank = same; census; density; desc_analysis; qual_quant; }
  { rank = same; building; poly_count; dataset; model_sel; kmeans; }
  { rank = same; slum; occupies; model_run; desc_results; }
  { rank = same; osm; buffer; feat_impact; } 
  { rank = same; dsm; slope; zonal; }

  # --- CONEXÕES PRINCIPAIS ---
  edge [color = '#000000', style = solid, arrowhead = vee, penwidth = 2.5, arrowsize = 1.0]
  
  h3:s -> spatial_int:n
  cnefe:ne -> spatial_int:sw      
  spatial_int:s -> dasymetric:n
  dsm:e -> slope:w
  slope:e -> zonal:w [style = dashed, color = '#888888'] 
  poly_count -> dataset
  
  dataset -> desc_analysis
  dataset -> model_sel
  
  model_run:e -> qual_quant:w
  model_run:e -> kmeans:w

  # --- CONEXÕES TRACEJADAS DE ENTRADA ---
  edge [color = '#888888', style = dashed, arrowhead = vee, penwidth = 2.5, arrowsize = 1.0]
  cnefe:e -> dasymetric:w
  census:e -> density:w
  building:e -> poly_count:w
  slum:e -> occupies:w
  osm:e -> buffer:w
}
"
)

# ============================================================
# EXPORTAÇÃO: 17 cm de largura (duas colunas), 600 dpi, altura proporcional
# ============================================================
svg_code <- DiagrammeRsvg::export_svg(gr)

largura_cm <- 17.0
dpi <- 600

# 1. PDF vetorial (altura proporcional)
rsvg::rsvg_pdf(
      charToRaw(svg_code),
      file = "figura_fluxograma.pdf",
      width = largura_cm / 2.54 * 72
)

# 2. PNG de alta resolução (altura proporcional)
rsvg::rsvg_png(
      charToRaw(svg_code),
      file = "figura_fluxograma_alta_res.png",
      width = round(largura_cm / 2.54 * dpi)
)
