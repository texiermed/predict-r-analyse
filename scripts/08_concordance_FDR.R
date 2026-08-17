# ============================================================================
# 08_concordance_FDR.R - Concordance item par item PR vs DM
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Pour chaque FDR commun aux 2 sources (Predict-R auto-declare et dossier
#   medical), evaluer la concordance :
#     - Concordance brute (% d'accord global)
#     - Discordance "PR seul" (declare PR mais non code DM)
#       -> peut traduire un sur-declaration PR ou un sous-codage DM
#     - Discordance "DM seul" (code DM mais non declare PR)
#       -> traduit une sous-declaration PR (oubli, gene, ignorance)
#     - Kappa de Cohen entre PR et DM pour ce FDR
#
# Utile pour discuter la qualite des deux sources et identifier les FDR
# pour lesquels l'autoquestionnaire est le moins fiable.
#
# REFERENCES METHODOLOGIQUES :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §5 (point 11 - comparaison des sources)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Packages ---------------------------------------------------------------
library(irr)         # Kappa
library(flextable)


# ============================================================================
# 1. PAIRES PR <-> DM A COMPARER
# ============================================================================
# Pour chaque ligne : nom du FDR, variable PR, variable DM.
# L'obesite est un cas particulier : PR_IMC numerique > 30 vs DM_obesite binaire.

# Pour la patho uro et l'expo, on construit des variables agregees cote PR
# qui couvrent les sous-questions distinctes du questionnaire officiel
# (Q15 + Q16 pour patho uro ; Q19 + Q20 pour expo).

predict_r <- predict_r |>
  mutate(
    PR_patho_uro_agg = as.integer(
      PR_patho_uro_recidivante == 1 | PR_maladie_uro_connue == 1
    ),
    PR_expo_agg = as.integer(
      PR_expo_PCI == 1 | PR_expo_radiotherapie == 1
    )
  )

paires <- tibble::tribble(
  ~FDR,               ~var_PR,                      ~var_DM,
  "HTA",              "PR_HTA",                     "DM_HTA",
  "Diabete",          "PR_diabete",                 "DM_diabete",
  "Tabagisme",        "PR_tabagisme",               "DM_tabagisme",
  "Dyslipidemie",     "PR_dyslipidemie",            "DM_dyslipidemie",
  "MCV",              "PR_MCV",                     "DM_MCV",
  "Insuf. cardiaque", "PR_insuf_cardiaque",         "DM_insuf_cardiaque",
  "Patho. urologique","PR_patho_uro_agg",           "DM_patho_uro",
  "Prematurite",      "PR_prematurite",             "DM_prematurite",
  "AINS chroniques",  "PR_AINS",                    "DM_ttt_AINS",
  "Expo. PCI/radio",  "PR_expo_agg",                "DM_expo_PCI_radiotherapie"
)


# ============================================================================
# 2. FONCTION DE CALCUL POUR UNE PAIRE
# ============================================================================
# Pour une paire de variables PR/DM binaires, calcule les indicateurs de
# concordance et de discordance.

calculer_concordance <- function(pr_var, dm_var, label) {
  v_pr <- predict_r[[pr_var]]
  v_dm <- predict_r[[dm_var]]

  # Tableau 2x2
  n_total <- length(v_pr)
  n_both <- sum(v_pr == 1 & v_dm == 1, na.rm = TRUE)
  n_pr_only <- sum(v_pr == 1 & v_dm == 0, na.rm = TRUE)
  n_dm_only <- sum(v_pr == 0 & v_dm == 1, na.rm = TRUE)
  n_neither <- sum(v_pr == 0 & v_dm == 0, na.rm = TRUE)

  n_pr_pos <- n_both + n_pr_only
  n_dm_pos <- n_both + n_dm_only

  concordance <- (n_both + n_neither) / n_total

  # Kappa de Cohen (si tableau 2x2 non degenere)
  kappa_val <- NA_real_
  if (var(v_pr, na.rm = TRUE) > 0 && var(v_dm, na.rm = TRUE) > 0) {
    kappa_obj <- tryCatch(
      kappa2(data.frame(v_pr, v_dm)),
      error = function(e) NULL
    )
    if (!is.null(kappa_obj)) kappa_val <- kappa_obj$value
  }

  tibble(
    FDR = label,
    `n PR+` = n_pr_pos,
    `n DM+` = n_dm_pos,
    `Concordants (les 2)` = n_both,
    `Discordants PR seul` = n_pr_only,
    `Discordants DM seul` = n_dm_only,
    `Concordance brute` = sprintf("%.1f %%", concordance * 100),
    `Kappa` = ifelse(is.na(kappa_val),
                     "n/a",
                     sprintf("%.3f", kappa_val))
  )
}


# ============================================================================
# 3. APPLIQUER A TOUTES LES PAIRES
# ============================================================================

tableau_discordances <- purrr::pmap_dfr(
  list(paires$var_PR, paires$var_DM, paires$FDR),
  calculer_concordance
)

cat("---------------------------------\n")
cat("CONCORDANCE PR vs DM par FDR\n")
cat("---------------------------------\n")
print(tableau_discordances)
cat("\n")


# ============================================================================
# 4. INTERPRETATION RAPIDE
# ============================================================================
# On flag les FDR les plus problematiques :
# - "sous-declaration PR" si n_DM_seul >> n_PR_seul
# - "sur-declaration PR" (ou sous-codage DM) si n_PR_seul >> n_DM_seul

cat("Lecture des discordances :\n")
cat(" - 'PR seul' = declare par le patient mais non code dans le DM\n")
cat("   (sur-declaration patient OU sous-codage MG)\n")
cat(" - 'DM seul' = code dans le DM mais non declare par le patient\n")
cat("   (sous-declaration patient : oubli, gene, ignorance)\n\n")


# ============================================================================
# 5. SAUVEGARDE - CSV + Word formate
# ============================================================================

write_csv(tableau_discordances,
          here("output", "tableaux", "tableau9_discordances_FDR.csv"))

flex_disc <- flextable(tableau_discordances) |>
  set_caption("Tableau 9. Concordance et discordances PR vs dossier medical, item par item") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:8, align = "center", part = "all") |>
  add_footer_lines(
    paste0("Effectif total : N = ", nrow(predict_r), " patients.")
  ) |>
  add_footer_lines(
    "'PR seul' : declare par le patient mais non code dans le dossier medical."
  ) |>
  add_footer_lines(
    "'DM seul' : code dans le DM mais non declare par le patient."
  ) |>
  add_footer_lines(
    "Kappa de Cohen : 0 = accord au niveau du hasard ; 1 = accord parfait."
  )

save_as_docx(flex_disc,
             path = here("output", "tableaux", "tableau9_discordances_FDR.docx"))


cat("Tableaux sauvegardes :\n")
cat("  - tableau9_discordances_FDR.csv (brut)\n")
cat("  - tableau9_discordances_FDR.docx (Word formate)\n\n")

cat("--- 08_concordance_FDR.R termine ---\n\n")
