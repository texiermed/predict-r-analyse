# ============================================================================
# 10_faisabilite.R - Indicateurs de faisabilite (OS3)
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Calculer les indicateurs de faisabilite pre-specifies au protocole :
#     - Taux de participation strict et large
#     - Taux de completion
#     - Taux de refus (avec motifs)
#     - Taux de realisation de la bandelette urinaire
#     - Duree de remplissage (NON DISPONIBLE via HEKO - voir note)
#
# DONNEES SOURCE :
#   - data/data_simulees.csv (pour les patients inclus)
#   - Variables externes a saisir : effectifs MSP, refus, motifs
#     (issus du Tableau_Recueil_Terrain_PredictR_v2.xlsx)
#
# REFERENCES :
#   - PROTOCOLE_PREDICT-R.md §8.1 (faisabilite)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §6 (faisabilite - definitions)
# ============================================================================


library(here)
source(here("scripts", "01_codebook.R"))
library(flextable)


# ============================================================================
# 1. DONNEES TERRAIN (a remplir avec les vraies valeurs apres recueil)
# ============================================================================
# Pour l'instant, valeurs fictives pour tester le script.
# Remplacer ces valeurs apres la semaine d'etude.

n_msp_total       <- 8500   # Total patientele MSP
n_consultations   <- 350    # Patients passes en consultation pendant la semaine
n_sollicites      <- 180    # Patients explicitement sollicites
n_refus           <- 65     # Refus apres information

# Repartition des motifs de refus (5 categories pre-specifiees)
motifs_refus <- tibble(
  motif = c("Pas le temps",
            "Pas interesse",
            "Deja suivi en nephrologie",
            "Probleme technique",
            "Autre"),
  n = c(25, 18, 8, 9, 5)
)


# ============================================================================
# 2. INDICATEURS CALCULES (a partir des donnees CRD)
# ============================================================================

n_inclus   <- nrow(predict_r)
n_bu_realisees <- sum(predict_r$PR_BU_realisee == 1, na.rm = TRUE)

# Taux de participation strict = inclus / sollicites
taux_part_strict <- n_inclus / n_sollicites
ic_strict <- prop.test(n_inclus, n_sollicites, correct = FALSE)$conf.int

# Taux de participation large = inclus / total patients MSP semaine
taux_part_large <- n_inclus / n_consultations
ic_large <- prop.test(n_inclus, n_consultations, correct = FALSE)$conf.int

# Taux de refus = refus / sollicites
taux_refus <- n_refus / n_sollicites
ic_refus <- prop.test(n_refus, n_sollicites, correct = FALSE)$conf.int

# Taux de realisation BU = BU realisees / inclus
taux_bu <- n_bu_realisees / n_inclus
ic_bu <- prop.test(n_bu_realisees, n_inclus, correct = FALSE)$conf.int


# ============================================================================
# 3. AFFICHAGE CONSOLE
# ============================================================================

cat("---------------------------------\n")
cat("FAISABILITE - INDICATEURS PRE-SPECIFIES\n")
cat("---------------------------------\n\n")

cat(sprintf("Patientele totale MSP         : %d patients\n", n_msp_total))
cat(sprintf("Consultations dans la semaine : %d\n", n_consultations))
cat(sprintf("Patients sollicites           : %d\n", n_sollicites))
cat(sprintf("Patients inclus (analyses)    : %d\n\n", n_inclus))

cat(sprintf("Taux de participation strict : %.1f %% [IC 95 %% : %.1f %% - %.1f %%]\n",
            taux_part_strict * 100, ic_strict[1] * 100, ic_strict[2] * 100))
cat(sprintf("Taux de participation large  : %.1f %% [IC 95 %% : %.1f %% - %.1f %%]\n",
            taux_part_large * 100, ic_large[1] * 100, ic_large[2] * 100))
cat(sprintf("Taux de refus                : %.1f %% [IC 95 %% : %.1f %% - %.1f %%]\n",
            taux_refus * 100, ic_refus[1] * 100, ic_refus[2] * 100))
cat(sprintf("Taux de realisation BU       : %.1f %% [IC 95 %% : %.1f %% - %.1f %%]\n\n",
            taux_bu * 100, ic_bu[1] * 100, ic_bu[2] * 100))

cat("Repartition des motifs de refus :\n")
motifs_refus_calc <- motifs_refus |>
  mutate(pct = sprintf("%.1f %%", n / n_refus * 100))
print(motifs_refus_calc)
cat("\n")

cat("Duree de remplissage du questionnaire : NON DISPONIBLE\n")
cat("  HEKO n'exporte pas la duree (confirme par Margaux le 12/05/2026)\n")
cat("  Seul l'horodatage de completion est disponible.\n\n")


# ============================================================================
# 4. TABLEAU WORD FORMATE
# ============================================================================

formate_pct <- function(p, ic1, ic2) {
  sprintf("%.1f %% [%.1f %% - %.1f %%]",
          p * 100, ic1 * 100, ic2 * 100)
}

tableau_faisabilite <- tibble(
  Indicateur = c(
    "Patientele totale MSP",
    "Consultations dans la semaine",
    "Patients sollicites",
    "Patients inclus (analyses)",
    "Taux de participation strict",
    "Taux de participation large",
    "Taux de refus",
    "Taux de realisation BU"
  ),
  `Numerateur / Denominateur` = c(
    "-",
    "-",
    "-",
    "-",
    sprintf("%d / %d", n_inclus, n_sollicites),
    sprintf("%d / %d", n_inclus, n_consultations),
    sprintf("%d / %d", n_refus, n_sollicites),
    sprintf("%d / %d", n_bu_realisees, n_inclus)
  ),
  `Valeur (IC 95 %)` = c(
    as.character(n_msp_total),
    as.character(n_consultations),
    as.character(n_sollicites),
    as.character(n_inclus),
    formate_pct(taux_part_strict, ic_strict[1], ic_strict[2]),
    formate_pct(taux_part_large, ic_large[1], ic_large[2]),
    formate_pct(taux_refus, ic_refus[1], ic_refus[2]),
    formate_pct(taux_bu, ic_bu[1], ic_bu[2])
  )
)

write_csv(tableau_faisabilite,
          here("output", "tableaux", "tableau5_faisabilite.csv"))

flex_faisa <- flextable(tableau_faisabilite) |>
  set_caption("Tableau 5. Indicateurs de faisabilite de l'etude PREDICT-R") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines("IC 95 % calcules par la methode de Wilson (sans correction de continuite).") |>
  add_footer_lines("La duree mediane de remplissage n'a pas pu etre mesuree : non exportee par HEKO (confirme par BOT design, 12/05/2026). Limite a mentionner en discussion.")

save_as_docx(flex_faisa,
             path = here("output", "tableaux", "tableau5_faisabilite.docx"))


# ============================================================================
# 5. TABLEAU MOTIFS DE REFUS (Word)
# ============================================================================

flex_motifs <- flextable(motifs_refus_calc) |>
  set_caption("Tableau 5b. Repartition des motifs de refus") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all")

save_as_docx(flex_motifs,
             path = here("output", "tableaux", "tableau5b_motifs_refus.docx"))


cat("Tableaux sauvegardes :\n")
cat("  - tableau5_faisabilite (csv + docx)\n")
cat("  - tableau5b_motifs_refus (docx)\n\n")

cat("--- 10_faisabilite.R termine ---\n\n")
