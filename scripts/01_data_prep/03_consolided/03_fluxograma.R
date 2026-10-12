library(DiagrammeR)

grViz(
  "
digraph flowchart {

  // ----------------------------------------------------
  // ESPAÇAMENTO AJUSTADO 
  // ----------------------------------------------------
  graph [layout = dot, rankdir = TB,
         nodesep = 5.0,        // era 2.5 → mais respiro horizontal
         ranksep = 3,        // era 1.5 → mais respiro vertical
         compound = true,
         newrank = true,
         splines = polyline]

  // ----------------------------------------------------
  // FONTES MANTIDAS EM 55
  // ----------------------------------------------------
  node [fontname = 'Helvetica', fontsize = 105, margin = '0.7,0.5']
  edge [fontname = 'Helvetica', fontsize = 105, penwidth = 6.0,
        arrowsize = 5.0, color = '#000000']

  // ====================================================
  // 1. UNIDADE DE ANÁLISE
  // ====================================================
  subgraph cluster_unit {
    label = 'Unidade de Análise'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60

    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', width = 20.0, fontsize = 105]

    h3 [label = 'Grades Hexagonais H3', group = 'center', fontsize = 105]
  }

  node [shape = box, style = filled, fillcolor = '#C9DAF8',
        color = '#4A86E8', fontname = 'Helvetica-Bold', width = 20.0]
  spatial_int [label = 'Interseção Espacial', group = 'center']

  // ====================================================
  // 2. DADOS DE ENTRADA (Coluna 1)
  // ====================================================
  subgraph cluster_input {
    label = 'Dados de Entrada'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 75; margin = 60

    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', width = 20]

    cnefe    [label = 'Pontos CNEFE',                  group = 'col1', fontsize = 105]
    census   [label = 'Setores Censitários',           group = 'col1', fontsize = 105]
    building [label = 'Polígonos de\\nEdificações',    group = 'col1', fontsize = 105]
    slum     [label = 'Polígonos de Favelas',          group = 'col1', fontsize = 105]
    osm      [label = 'Linhas Vetoriais (OSM)',        group = 'col1', fontsize = 105]
    dsm      [label = 'MDS', width = 5.0,              group = 'col1', fontsize = 105]
    slope    [label = 'Declividade', width = 5.0,      group = 'col1', fontsize = 105]
  }

  // ====================================================
  // 3. PRÉ-PROCESSAMENTO (Coluna 2)
  // ====================================================
  subgraph cluster_preprocessing {
    label = 'Pré-processamento'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60

    node [shape = box, style = filled, fillcolor = '#C9DAF8',
          color = '#4A86E8', fontname = 'Helvetica', width = 20.0]

    dasymetric [label = 'Mapeamento\\nDasimétrico',                       group = 'col2', fontsize = 105]
    density    [label = 'Densidade de Área',                              group = 'col2', fontsize = 105]
    poly_count [label = 'Contagem, média e desvio\\npadrão de polígonos', group = 'col2', fontsize = 105]
    occupies   [label = 'Se ocupa 70%\\nda área',                         group = 'col2', fontsize = 105]
    buffer     [label = 'Buffer de 100m',                                 group = 'col2', fontsize = 105]
    zonal      [label = 'Estatística Zonal\\n(Média)',                    group = 'col2', fontsize = 105]
  }

  // ====================================================
  // 4. CONJUNTO DE DADOS (Centro)
  // ====================================================
  node [shape = parallelogram, style = filled, fillcolor = '#FFF2CC',
        color = '#D6B656', fontname = 'Helvetica-Bold', width = 12.0]
  dataset [label = 'Conjunto de Dados\\nde Feições Hexagonais',
           group = 'center', fontsize = 85]

  // ====================================================
  // 5. ANÁLISE DESCRITIVA (Coluna 3 - Topo)
  // ====================================================
  node [shape = box, style = filled, fillcolor = '#FADCD9',
        color = '#D9534F', fontname = 'Helvetica-Bold', width = 12.0]
  desc_analysis [label = 'Análise Descritiva', group = 'col3', fontsize = 105]

  // ====================================================
  // 6. AVALIAÇÃO DA CLASSIFICAÇÃO (Coluna 4 - Topo)
  // ====================================================
  subgraph cluster_evaluation {
    label = 'Avaliação da\\nClassificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]

    qual_quant [label = 'Qualitativa e\\nQuantitativa', group = 'col4', fontsize = 105]
  }

  // ====================================================
  // 7. CLASSIFICAÇÃO (Coluna 3 - Meio)
  // ====================================================
  subgraph cluster_classification {
    label = 'Classificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 50

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]

    model_sel   [label = 'Seleção do\\nModelo',     group = 'col3', fontsize = 105]
    model_run   [label = 'Execução do\\nModelo',    group = 'col3', fontsize = 105]
    feat_impact [label = 'Impacto das\\nVariáveis', group = 'col3', fontsize = 105]
  }

  // ====================================================
  // 8. ANÁLISE DE RESULTADOS (Coluna 4 - Baixo)
  // ====================================================
  subgraph cluster_results {
    label = 'Análise de\\nResultados'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 50

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]

    kmeans       [label = 'k-means',            group = 'col4', fontsize = 105]
    desc_results [label = 'Análise Descritiva', group = 'col4', fontsize = 105]
  }

  // ----------------------------------------------------
  // ALINHAMENTOS VERTICAIS INTERNOS
  // ----------------------------------------------------
  cnefe -> census -> building -> slum -> osm -> dsm [style = invis]
  dasymetric -> density -> poly_count -> occupies -> buffer -> zonal [style = invis]
  desc_analysis -> model_sel [style = invis]
  qual_quant -> kmeans -> desc_results [style = invis]

  // ----------------------------------------------------
  // GRADE RÍGIDA DE ALINHAMENTO HORIZONTAL
  // ----------------------------------------------------
  { rank = same; cnefe; dasymetric; }
  { rank = same; census; density; desc_analysis; qual_quant; }
  { rank = same; building; poly_count; dataset; model_sel; }
  { rank = same; slum; occupies; model_run; kmeans; }
  { rank = same; osm; buffer; feat_impact; desc_results; }
  { rank = same; dsm; slope; zonal; }

  // ----------------------------------------------------
  // CONEXÕES COM SETAS SÓLIDAS
  // ----------------------------------------------------
  edge [color = '#000000', style = solid, arrowhead = vee,
        penwidth = 6.0, arrowsize = 5.0]

  // Eixos verticais principais
  h3:s -> spatial_int:n
  spatial_int:s -> dasymetric:n
  dsm:e -> slope:w
  poly_count:e -> dataset:w

  // Conexões horizontais e cruzadas
  cnefe:e -> spatial_int:w
  dataset:e -> desc_analysis:w
  dataset:e -> model_sel:w

  // Fluxo para avaliação e resultados
  model_run:e -> qual_quant:w
  model_run:e -> kmeans:w

  // ----------------------------------------------------
  // CONEXÕES TRACEJADAS (DADOS DE ENTRADA -> PRÉ-PROCESSAMENTO)
  // ----------------------------------------------------
  edge [color = '#888888', style = dashed, arrowhead = vee,
        penwidth = 6.0, arrowsize = 5.0]

  cnefe:e -> dasymetric:w
  census:e -> density:w
  building:e -> poly_count:w
  slum:e -> occupies:w
  osm:e -> buffer:w
  slope:e -> zonal:w

}
"
)

#
#
#
#

# Instale uma vez, se necessário:
install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# 1. Seu diagrama
gr <- grViz(
  "
digraph flowchart {
  graph [layout = dot, rankdir = TB,
         nodesep = 5.0, ranksep = 3,
         compound = true, newrank = true, splines = polyline]
  node [fontname = 'Helvetica', fontsize = 105, margin = '0.7,0.5']
  edge [fontname = 'Helvetica', fontsize = 105, penwidth = 6.0,
        arrowsize = 5.0, color = '#000000']

  subgraph cluster_unit {
    label = 'Unidade de Análise'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60
    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', width = 20.0, fontsize = 105]
    h3 [label = 'Grades Hexagonais H3', group = 'center', fontsize = 105]
  }
  node [shape = box, style = filled, fillcolor = '#C9DAF8',
        color = '#4A86E8', fontname = 'Helvetica-Bold', width = 20.0]
  spatial_int [label = 'Interseção Espacial', group = 'center']

  subgraph cluster_input {
    label = 'Dados de Entrada'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 75; margin = 60
    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', width = 20]
    cnefe    [label = 'Pontos CNEFE',               group = 'col1', fontsize = 105]
    census   [label = 'Setores Censitários',        group = 'col1', fontsize = 105]
    building [label = 'Polígonos de\\nEdificações', group = 'col1', fontsize = 105]
    slum     [label = 'Polígonos de Favelas',       group = 'col1', fontsize = 105]
    osm      [label = 'Linhas Vetoriais (OSM)',     group = 'col1', fontsize = 105]
    dsm      [label = 'MDS', width = 5.0,           group = 'col1', fontsize = 105]
    slope    [label = 'Declividade', width = 5.0,   group = 'col1', fontsize = 105]
  }

  subgraph cluster_preprocessing {
    label = 'Pré-processamento'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60
    node [shape = box, style = filled, fillcolor = '#C9DAF8',
          color = '#4A86E8', fontname = 'Helvetica', width = 20.0]
    dasymetric [label = 'Mapeamento\\nDasimétrico',                       group = 'col2', fontsize = 105]
    density    [label = 'Densidade de Área',                              group = 'col2', fontsize = 105]
    poly_count [label = 'Contagem, média e desvio\\npadrão de polígonos', group = 'col2', fontsize = 105]
    occupies   [label = 'Se ocupa 70%\\nda área',                         group = 'col2', fontsize = 105]
    buffer     [label = 'Buffer de 100m',                                 group = 'col2', fontsize = 105]
    zonal      [label = 'Estatística Zonal\\n(Média)',                    group = 'col2', fontsize = 105]
  }

  node [shape = parallelogram, style = filled, fillcolor = '#FFF2CC',
        color = '#D6B656', fontname = 'Helvetica-Bold', width = 12.0]
  dataset [label = 'Conjunto de Dados\\nde Feições Hexagonais',
           group = 'center', fontsize = 85]

  node [shape = box, style = filled, fillcolor = '#FADCD9',
        color = '#D9534F', fontname = 'Helvetica-Bold', width = 12.0]
  desc_analysis [label = 'Análise Descritiva', group = 'col3', fontsize = 105]

  subgraph cluster_evaluation {
    label = 'Avaliação da\\nClassificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 60
    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]
    qual_quant [label = 'Qualitativa e\\nQuantitativa', group = 'col4', fontsize = 105]
  }

  subgraph cluster_classification {
    label = 'Classificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 50
    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]
    model_sel   [label = 'Seleção do\\nModelo',     group = 'col3', fontsize = 105]
    model_run   [label = 'Execução do\\nModelo',    group = 'col3', fontsize = 105]
    feat_impact [label = 'Impacto das\\nVariáveis', group = 'col3', fontsize = 105]
  }

  subgraph cluster_results {
    label = 'Análise de\\nResultados'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 4; fontname = 'Helvetica-Bold'; fontsize = 105; margin = 50
    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica', width = 20.0]
    kmeans       [label = 'k-means',            group = 'col4', fontsize = 105]
    desc_results [label = 'Análise Descritiva', group = 'col4', fontsize = 105]
  }

  cnefe -> census -> building -> slum -> osm -> dsm [style = invis]
  dasymetric -> density -> poly_count -> occupies -> buffer -> zonal [style = invis]
  desc_analysis -> model_sel [style = invis]
  qual_quant -> kmeans -> desc_results [style = invis]

  { rank = same; cnefe; dasymetric; }
  { rank = same; census; density; desc_analysis; qual_quant; }
  { rank = same; building; poly_count; dataset; model_sel; }
  { rank = same; slum; occupies; model_run; kmeans; }
  { rank = same; osm; buffer; feat_impact; desc_results; }
  { rank = same; dsm; slope; zonal; }

  edge [color = '#000000', style = solid, arrowhead = vee,
        penwidth = 6.0, arrowsize = 5.0]
  h3:s -> spatial_int:n
  spatial_int:s -> dasymetric:n
  dsm:e -> slope:w
  poly_count:e -> dataset:w
  cnefe:e -> spatial_int:w
  dataset:e -> desc_analysis:w
  dataset:e -> model_sel:w
  model_run:e -> qual_quant:w
  model_run:e -> kmeans:w

  edge [color = '#888888', style = dashed, arrowhead = vee,
        penwidth = 6.0, arrowsize = 5.0]
  cnefe:e -> dasymetric:w
  census:e -> density:w
  building:e -> poly_count:w
  slum:e -> occupies:w
  osm:e -> buffer:w
  slope:e -> zonal:w
}
"
)

# 2. Exportar para SVG (vetorial, sem perda)
svg_code <- export_svg(gr)

# 3. Converter SVG -> PNG com tamanho controlado
#    Tamanho SBSR: 8,6 cm de largura (= 1 coluna)
#    Fórmula pixels = cm / 2.54 * dpi
largura_cm <- 8.6
altura_cm <- 14 # ajuste conforme a proporção do diagrama
dpi <- 600 # 300 para submissão, 600 para impressão

rsvg_png(
  charToRaw(svg_code),
  file = "figura_fluxograma.png",
  width = round(largura_cm / 2.54 * dpi),
  height = round(altura_cm / 2.54 * dpi)
)
