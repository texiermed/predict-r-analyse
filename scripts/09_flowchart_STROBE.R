# ============================================================================
# 09_flowchart_STROBE.R - Diagramme de flux STROBE
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Generer le diagramme de flux STROBE obligatoire pour les etudes
#   observationnelles. Affiche le cheminement : patients consultants ->
#   sollicites -> incluses -> analyses.
#
# DONNEES SOURCE :
#   Excel "Synthese_Flux_STROBE_PredictR.xlsx" (rempli pendant la semaine
#   d'etude). Pour l'instant, donnees fictives pour tester le script.
#
# REFERENCES :
#   - PROTOCOLE_PREDICT-R.md §5.7 (STROBE)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §6 (faisabilite)
# ============================================================================


library(here)
source(here("scripts", "01_codebook.R"))
library(DiagrammeR)


# ============================================================================
# 1. DONNEES A INTEGRER (a remplir avec les vraies donnees terrain)
# ============================================================================

# Donnees fictives pour le test du script. Remplacer par les valeurs reelles
# issues du Tableau_Recueil_Terrain_PredictR_v2.xlsx apres la semaine.

n_patientele_msp   <- 8500   # Total patientele MSP (Alma Pro + Doctolib)
n_consultations    <- 350    # Patients passes en MSP pendant la semaine
n_sollicites       <- 180    # Patients explicitement sollicites
n_eligibles        <- 165    # Eligibles (apres criteres inclusion)
n_refus            <- 65     # Refus apres information
n_acceptes         <- 100    # Acceptes
n_completes        <- 80     # Questionnaires complets (donnees analysables)
n_analyses         <- nrow(predict_r)  # Effectivement inclus dans l'analyse


# ============================================================================
# 2. GENERATION DU FLOWCHART (graphique GraphViz)
# ============================================================================

flux_strobe <- grViz(
  paste0("
digraph STROBE {

  graph [layout = dot, rankdir = TB, fontsize = 12]
  node [shape = box, style = 'filled,rounded', fillcolor = '#e8f5ed',
        fontname = Helvetica, fontsize = 11, width = 4]
  edge [arrowhead = vee]

  A1 [label = 'Patientele MSP Labarthe-sur-Leze\\nn = ", n_patientele_msp, "', fillcolor = '#f0f0f0']
  A2 [label = 'Patients consultants pendant\\nla semaine d etude\\nn = ", n_consultations, "']
  A3 [label = 'Patients sollicites\\nn = ", n_sollicites, "']
  A4 [label = 'Patients eligibles\\n(criteres d inclusion respectes)\\nn = ", n_eligibles, "']
  A5 [label = 'Patients ayant accepte\\nde participer\\nn = ", n_acceptes, "']
  A6 [label = 'Questionnaires complets\\nn = ", n_completes, "']
  A7 [label = 'Patients analyses\\nn = ", n_analyses, "', fillcolor = '#4CAF50', fontcolor = white]

  # Exclusions (a droite, en rouge clair)
  B1 [label = 'Non sollicites\\nn = ", n_consultations - n_sollicites, "', fillcolor = '#ffebee', width = 2.5]
  B2 [label = 'Non eligibles\\nn = ", n_sollicites - n_eligibles, "', fillcolor = '#ffebee', width = 2.5]
  B3 [label = 'Refus apres information\\nn = ", n_refus, "', fillcolor = '#ffebee', width = 2.5]
  B4 [label = 'Questionnaires incomplets\\nn = ", n_acceptes - n_completes, "', fillcolor = '#ffebee', width = 2.5]

  A1 -> A2
  A2 -> A3
  A3 -> A4
  A4 -> A5
  A5 -> A6
  A6 -> A7

  A2 -> B1 [style = dashed]
  A3 -> B2 [style = dashed]
  A4 -> B3 [style = dashed]
  A5 -> B4 [style = dashed]

  { rank = same; A2; B1 }
  { rank = same; A3; B2 }
  { rank = same; A4; B3 }
  { rank = same; A5; B4 }

  label = 'Diagramme de flux STROBE - etude PREDICT-R'
  labelloc = b
  fontsize = 14
}
"))


# ============================================================================
# 3. EXPORT PNG + SVG
# ============================================================================

# Necessite le package DiagrammeRsvg + rsvg
if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
    !requireNamespace("rsvg", quietly = TRUE)) {
  message("Pour exporter en PNG/SVG, installer :")
  message('  install.packages(c("DiagrammeRsvg", "rsvg"))')
  message("Pour l'instant, le flowchart s'affiche dans le panneau Viewer de RStudio.")
  print(flux_strobe)
} else {
  svg_str <- DiagrammeRsvg::export_svg(flux_strobe)
  # PNG
  rsvg::rsvg_png(charToRaw(svg_str),
                 file = here("output", "figures", "fig00_flowchart_STROBE.png"),
                 width = 900)
  # SVG (vectoriel, ideal pour la these)
  writeLines(svg_str,
             here("output", "figures", "fig00_flowchart_STROBE.svg"))
  cat("Diagramme de flux STROBE sauvegarde :\n")
  cat("  - output/figures/fig00_flowchart_STROBE.png\n")
  cat("  - output/figures/fig00_flowchart_STROBE.svg\n")
}


# ============================================================================
# 4. RECAPITULATIF NUMERIQUE
# ============================================================================

cat("\n---------------------------------\n")
cat("DIAGRAMME DE FLUX STROBE - SYNTHESE\n")
cat("---------------------------------\n")
cat(sprintf("  Patientele MSP totale      : %d\n", n_patientele_msp))
cat(sprintf("  Consultations / semaine    : %d\n", n_consultations))
cat(sprintf("  Sollicites                 : %d\n", n_sollicites))
cat(sprintf("  Eligibles                  : %d\n", n_eligibles))
cat(sprintf("  Acceptes                   : %d (refus = %d)\n",
            n_acceptes, n_refus))
cat(sprintf("  Questionnaires complets    : %d\n", n_completes))
cat(sprintf("  Patients analyses          : %d\n\n", n_analyses))

cat("--- 09_flowchart_STROBE.R termine ---\n\n")
