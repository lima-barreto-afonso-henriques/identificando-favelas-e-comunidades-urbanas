# ============================================================
# EXPORTAR DIAGRAMA grViz EM ALTA RESOLUÇÃO (Ajustado)
# ============================================================

source(here::here("scripts", "00_functions", "00_setup.R"))

gr <- grViz(
      "
digraph flowchart {

  # --- CONFIGURAÇÕES GERAIS ---
  graph [layout = dot, rankdir = TB,
         nodesep = 0.6,        # Aumentado para espalhar mais na horizontal e preencher o vazio
         ranksep = 0.5,        
         compound = true,
         newrank = true,
         splines = ortho]

  # --- PROPRIEDADES PADRÃO ---
  node [fontname = 'Helvetica', fontsize = 20, margin = '0.15,0.1', width = 3.0]
  edge [fontname = 'Helvetica', fontsize = 16, penwidth = 1.5,
        arrowsize = 1.0, color = '#000000']

  # ====================================================
  # 1. UNIDADE DE ANÁLISE
  # ====================================================
  subgraph cluster_unit {
    label = 'Unidade de Análise'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica']

    h3 [label = 'Grades Hexagonais H3']
  }

  node [shape = box, style = filled, fillcolor = '#C9DAF8',
        color = '#4A86E8', fontname = 'Helvetica-Bold']
  spatial_int [label = 'Interseção Espacial']

  # ====================================================
  # 2. DADOS DE ENTRADA (Coluna 1)
  # ====================================================
  subgraph cluster_input {
    label = 'Dados de Entrada'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = parallelogram, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica-Bold']

    cnefe    [label = 'Pontos CNEFE']
    census   [label = 'Setores Censitários']
    building [label = 'Polígonos de\\nEdificações']
    slum     [label = 'Polígonos de Favelas']
    osm      [label = 'Linhas Vetoriais (OSM)']
    dsm      [label = 'MDS', width = 1.5]
    slope    [label = 'Declividade', width = 2.0]
  }

  # ====================================================
  # 3. PRÉ-PROCESSAMENTO (Coluna 2)
  # ====================================================
  subgraph cluster_preprocessing {
    label = 'Pré-processamento'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = box, style = filled, fillcolor = '#C9DAF8',
          color = '#4A86E8', fontname = 'Helvetica']

    dasymetric [label = 'Mapeamento\\nDasimétrico']
    density    [label = 'Densidade de Área']
    poly_count [label = 'Contagem, média e desvio\\npadrão de polígonos']
    occupies   [label = 'Se ocupa 70%\\nda área']
    buffer     [label = 'Buffer de 100m']
    zonal      [label = 'Estatística Zonal\\n(Média)']
  }

  # ====================================================
  # 4. CONJUNTO DE DADOS (Centro)
  # ====================================================
  node [shape = parallelogram, style = filled, fillcolor = '#FFF2CC',
        color = '#D6B656', fontname = 'Helvetica-Bold', width = 2.5]
  dataset [label = 'Conjunto de Dados\\nde Feições Hexagonais']

  # ====================================================
  # 5. ANÁLISE DESCRITIVA (Coluna 3)
  # ====================================================
  node [shape = box, style = filled, fillcolor = '#FADCD9',
        color = '#D9534F', fontname = 'Helvetica-Bold']
  desc_analysis [label = 'Análise Descritiva']

  # ====================================================
  # 6. AVALIAÇÃO DA CLASSIFICAÇÃO (Coluna 4)
  # ====================================================
  subgraph cluster_evaluation {
    label = 'Avaliação da\\nClassificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica']

    qual_quant [label = 'Qualitativa e\\nQuantitativa']
  }

  # ====================================================
  # 7. CLASSIFICAÇÃO (Coluna 3)
  # ====================================================
  subgraph cluster_classification {
    label = 'Classificação'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica']

    model_sel   [label = 'Seleção do\\nModelo']
    model_run   [label = 'Execução do\\nModelo']
    feat_impact [label = 'Impacto das\\nVariáveis']
  }

  # ====================================================
  # 8. ANÁLISE DE RESULTADOS (Coluna 4)
  # ====================================================
  subgraph cluster_results {
    label = 'Análise de\\nResultados'
    style = filled; fillcolor = '#FADCD9'; color = '#D9534F'
    penwidth = 2; fontname = 'Helvetica-Bold'; fontsize = 22; margin = 15

    node [shape = box, style = filled, fillcolor = '#FFFFFF',
          color = '#333333', fontname = 'Helvetica']

    kmeans       [label = 'k-means']
    desc_results [label = 'Análise Descritiva']
  }

  # --- ESTRUTURA E ALINHAMENTOS (Ajustados conforme seus desenhos) ---
  cnefe -> census -> building -> slum -> osm -> dsm [style = invis]
  dasymetric -> density -> poly_count -> occupies -> buffer -> zonal [style = invis]
  desc_analysis -> model_sel [style = invis]
  qual_quant -> kmeans -> desc_results [style = invis]

  { rank = same; cnefe; dasymetric; }
  { rank = same; census; density; }
  { rank = same; building; poly_count; dataset; model_sel; desc_analysis; }
  { rank = same; slum; occupies; model_run; }
  # AQUI: Avaliação da Classificação desceu para alinhar com k-means e buffer
  { rank = same; osm; buffer; feat_impact; kmeans; qual_quant; } 
  { rank = same; dsm; slope; zonal; desc_results; }

  # --- CONEXÕES ---
  edge [color = '#000000', style = solid, arrowhead = vee, penwidth = 2.0, arrowsize = 1.0]
  
  h3:s -> spatial_int:n
  cnefe:ne -> spatial_int:sw      
  spatial_int:s -> dasymetric:n
  dsm:e -> slope:w
  slope:e -> zonal:w [style = dashed, color = '#888888'] 
  poly_count:e -> dataset:w
  
  # Ajustei as portas de saída para as setas não se cruzarem agora que desceram
  dataset:e -> desc_analysis:sw
  dataset:e -> model_sel:nw
  
  model_run:e -> qual_quant:nw
  model_run:e -> kmeans:w

  edge [color = '#888888', style = dashed, arrowhead = vee, penwidth = 2.0, arrowsize = 1.0]
  cnefe:e -> dasymetric:w
  census:e -> density:w
  building:e -> poly_count:w
  slum:e -> occupies:w
  osm:e -> buffer:w
}
"
)

# ============================================================
# EXPORTAÇÃO EM ALTA RESOLUÇÃO
# ============================================================
svg_code <- DiagrammeRsvg::export_svg(gr)

# 1. Exportar PDF (Vetorial)
rsvg::rsvg_pdf(
      charToRaw(svg_code),
      file = "figura_fluxograma.pdf",
      width = 17.5 / 2.54 * 72
)

# 2. Exportar PNG de altíssima resolução (600 DPI)
largura_cm <- 17.5
dpi <- 600

rsvg::rsvg_png(
      charToRaw(svg_code),
      file = "figura_fluxograma_alta_res.png",
      width = round(largura_cm / 2.54 * dpi)
)
