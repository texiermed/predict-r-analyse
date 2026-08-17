# ============================================================================
# 05_pratiques_depistage.R - Objectif secondaire 5 (OS5)
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Decrire les pratiques de depistage de la MRC dans la population de
#   l'etude. Hierarchisation en 5 niveaux et taux de conformite HAS dans
#   le sous-groupe des patients avec FDR HAS >= 1.
#
# REFERENCES METHODOLOGIQUES :
#   - PROTOCOLE_PREDICT-R.md §7.4 (pratiques de depistage, OS5)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §11 (resume des analyses)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Packages ---------------------------------------------------------------
library(gtsummary)


# ============================================================================
# 1. REPARTITION DES NIVEAUX DE DEPISTAGE
# ============================================================================
# 5 categories hierarchiques (definies dans 01_codebook.R) :
#   1_aucun, 2_partiel_DFG, 3_partiel_urinaire, 4_complet, 5_nephro

cat("---------------------------------\n")
cat("NIVEAUX DE DEPISTAGE - ensemble de la population\n")
cat("---------------------------------\n")

repartition <- predict_r |>
  count(depistage_niveau) |>
  mutate(pct = round(n / sum(n) * 100, 1))

print(repartition)
cat("\n")


# ============================================================================
# 2. CONFORMITE HAS - sous-groupe FDR HAS >= 1
# ============================================================================
# Depistage conforme HAS = depistage complet (DFG + test urinaire)
#                          OU suivi nephrologique.

predict_r <- predict_r |>
  mutate(
    conforme_HAS = as.integer(
      depistage_niveau %in% c("Complet (DFG + test urinaire)",
                              "Suivi nephrologique")
    )
  )

fdr_pos <- predict_r |> filter(nb_FDR_HAS >= 1)

n_fdr <- nrow(fdr_pos)
n_conforme <- sum(fdr_pos$conforme_HAS)

cat("Sous-groupe FDR HAS >= 1 :\n")
cat("  n              :", n_fdr, "\n")
cat("  conformes HAS  :", n_conforme,
    " (", round(n_conforme / n_fdr * 100, 1), " %)\n\n", sep = "")

# IC 95 % de la proportion conforme (Wilson)
if (n_fdr > 0) {
  ic_conf <- prop.test(n_conforme, n_fdr, correct = FALSE)
  cat("  IC 95 % Wilson : [",
      round(ic_conf$conf.int[1] * 100, 1), "% - ",
      round(ic_conf$conf.int[2] * 100, 1), "%]\n\n", sep = "")
}


# ============================================================================
# 3. PRATIQUES x NIVEAU DE RISQUE PR
# ============================================================================
# Tableau croise : niveau de depistage par niveau de risque Predict-R
# Pour repondre : "les patients orange/rouge sont-ils mieux depistes ?"

cat("Niveau de depistage x niveau de risque PR :\n")
tab_dep_pr <- predict_r |>
  count(PR_couleur, depistage_niveau) |>
  pivot_wider(names_from = PR_couleur,
              values_from = n,
              values_fill = 0)

print(tab_dep_pr)
cat("\n")

write_csv(tab_dep_pr,
          here("output", "tableaux", "tableau6_pratiques_x_PR.csv"))


# ============================================================================
# 4. TABLEAU SYNTHESE (gtsummary)
# ============================================================================
# Tableau publication-ready avec gtsummary

tableau_pratiques <- predict_r |>
  select(depistage_niveau, conforme_HAS, PR_couleur) |>
  tbl_summary(
    by = PR_couleur,
    label = list(
      depistage_niveau ~ "Niveau de depistage realise",
      conforme_HAS ~ "Depistage conforme HAS"
    )
  ) |>
  add_p() |>
  add_overall()

print(tableau_pratiques)

tableau_pratiques |>
  as_flex_table() |>
  flextable::save_as_docx(
    path = here("output", "tableaux", "tableau6_pratiques.docx")
  )


# ============================================================================
# 5. SAUVEGARDE
# ============================================================================

write_csv(repartition,
          here("output", "tableaux", "tableau6_repartition_depistage.csv"))


cat("--- 05_pratiques_depistage.R termine ---\n\n")
