# ============================================================================
# 07_sous_groupes.R - Analyses en sous-groupes (exploratoires)
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Analyses exploratoires en sous-groupes pre-specifies :
#     1. Coherence interne : % MRC connue classes orange/rouge
#     2. Par nombre de FDR HAS (Chi-2 de tendance Cochran-Armitage)
#     3. Par tranche d'age
#
# Note : Analyse de sensibilite hors MRC connue est dans 03_CJP.R.
#
# REFERENCES METHODOLOGIQUES :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §7 (sous-groupes)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §8 (coherence interne MRC)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §3 (Chi-2 de tendance)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))
library(flextable)


# ============================================================================
# 1. COHERENCE INTERNE - patients MRC connue
# ============================================================================
# On verifie : les patients ayant une MRC deja connue sont-ils bien
# classes orange ou rouge par Predict-R ?
# C'est un sanity check (guide stat §8).

mrc_connue <- predict_r |> filter(DM_MRC_connue == 1)
n_mrc <- nrow(mrc_connue)

cat("---------------------------------\n")
cat("COHERENCE INTERNE - patients MRC connue\n")
cat("---------------------------------\n")
cat("  n MRC connue :", n_mrc, "\n")

if (n_mrc > 0) {
  n_correct <- sum(mrc_connue$PR_bin == 1)
  cat("  classes orange/rouge :", n_correct,
      " (", round(n_correct / n_mrc * 100, 1), " %)\n\n", sep = "")

  # Description des cas discordants (classes vert mais MRC connue)
  discordants <- mrc_connue |> filter(PR_bin == 0)
  if (nrow(discordants) > 0) {
    cat("Cas discordants (MRC connue mais classes vert) :\n")
    print(discordants |>
            select(id_patient, age, DM_sexe,
                   DM_MRC_stade, DM_MRC_critere))
    cat("\n")
  } else {
    cat("Aucun cas discordant. Coherence interne parfaite.\n\n")
  }
} else {
  cat("  Aucun patient MRC connue dans l'echantillon.\n\n")
}


# ============================================================================
# 2. PAR CATEGORIE DE FDR HAS - CHI-2 DE TENDANCE
# ============================================================================
# On teste : la proportion de rattrapes augmente-t-elle avec le nombre
# de FDR HAS ? Utilisation de prop.trend.test (guide stat §3).
# On ne considere que les patients orange/rouge (les seuls eligibles a
# etre "rattrapes").

or_rouge <- predict_r |> filter(PR_bin == 1)

cat("---------------------------------\n")
cat("RATTRAPES PAR CATEGORIE DE FDR HAS\n")
cat("---------------------------------\n")

tab_fdr <- or_rouge |>
  group_by(cat_FDR_HAS) |>
  summarise(
    n = n(),
    n_rattrapes = sum(rattrape),
    pct = round(mean(rattrape) * 100, 1),
    .groups = "drop"
  )
print(tab_fdr)
cat("\n")

# Chi-2 de tendance si effectifs suffisants
if (nrow(tab_fdr) >= 2 && all(tab_fdr$n >= 5)) {
  trend_test <- prop.trend.test(
    x = tab_fdr$n_rattrapes,
    n = tab_fdr$n
  )
  cat("Chi-2 de tendance (Cochran-Armitage) :\n")
  cat("  X-squared =", round(trend_test$statistic, 3),
      ", df =", trend_test$parameter, "\n")
  cat("  p-value   =", format(trend_test$p.value, digits = 4), "\n\n")
} else {
  cat("Effectifs insuffisants pour le Chi-2 de tendance.\n\n")
}


# ============================================================================
# 3. PAR TRANCHE D'AGE
# ============================================================================

cat("---------------------------------\n")
cat("RATTRAPES PAR TRANCHE D'AGE\n")
cat("---------------------------------\n")

tab_age <- or_rouge |>
  group_by(tranche_age) |>
  summarise(
    n = n(),
    n_rattrapes = sum(rattrape),
    pct = round(mean(rattrape) * 100, 1),
    .groups = "drop"
  )
print(tab_age)
cat("\n")


# ============================================================================
# 4a. TABLEAU 8a - COHERENCE INTERNE (MRC connue)
# ============================================================================
# Indicateur : % de patients avec MRC deja connue classes orange/rouge
# par Predict-R. C'est une verification de coherence interne (sanity check).

n_classe_or <- ifelse(n_mrc > 0, sum(mrc_connue$PR_bin == 1), 0)
n_discordants <- ifelse(n_mrc > 0, sum(mrc_connue$PR_bin == 0), 0)
pct_correct <- ifelse(n_mrc > 0,
                      sprintf("%.1f %%", n_classe_or / n_mrc * 100),
                      "n/a")

tableau_8a <- tibble(
  Indicateur = c("Patients avec MRC connue (n)",
                 "Classes orange/rouge par Predict-R",
                 "Cas discordants (classes vert)",
                 "Taux de classification correcte"),
  Valeur = c(as.character(n_mrc),
             paste0(n_classe_or, " / ", n_mrc),
             paste0(n_discordants, " / ", n_mrc),
             pct_correct)
)

print(tableau_8a)
cat("\n")

write_csv(tableau_8a,
          here("output", "tableaux", "tableau8a_coherence_MRC.csv"))

flex_8a <- flextable(tableau_8a) |>
  set_caption("Tableau 8a. Coherence interne - patients avec MRC connue") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2, align = "left", part = "all") |>
  add_footer_lines(
    "Verification de coherence interne (sanity check) : un patient avec MRC connue devrait etre classe orange ou rouge par Predict-R."
  )

save_as_docx(flex_8a,
             path = here("output", "tableaux", "tableau8a_coherence_MRC.docx"))


# ============================================================================
# 4b. TABLEAU 8b - RATTRAPES PAR SOUS-GROUPE (FDR HAS + Age)
# ============================================================================
# Indicateur : % de patients rattrapes parmi les orange/rouge dans chaque
# sous-groupe pre-specifie.

tableau_8b <- bind_rows(
  # FDR HAS
  tab_fdr |>
    rename(k = n_rattrapes) |>
    mutate(`Sous-groupe` = paste("FDR HAS :", cat_FDR_HAS),
           pct_str = sprintf("%.1f %%", pct)) |>
    select(`Sous-groupe`, n, k, pct_str),

  # Tranches d'age
  tab_age |>
    rename(k = n_rattrapes) |>
    mutate(`Sous-groupe` = paste("Age :", tranche_age),
           pct_str = sprintf("%.1f %%", pct)) |>
    select(`Sous-groupe`, n, k, pct_str)
) |>
  rename(`Effectif n (orange/rouge)` = n,
         `Rattrapes k` = k,
         `% rattrapes` = pct_str)

print(tableau_8b)
cat("\n")

write_csv(tableau_8b,
          here("output", "tableaux", "tableau8b_sousgroupes.csv"))

flex_8b <- flextable(tableau_8b) |>
  set_caption("Tableau 8b. Proportion de rattrapes par sous-groupe pre-specifie (exploratoire)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines(
    "Sous-groupes pre-specifies. Effectifs limites - analyses exploratoires non dimensionnees pour test statistique."
  )

save_as_docx(flex_8b,
             path = here("output", "tableaux", "tableau8b_sousgroupes.docx"))


cat("\nTableaux sauvegardes :\n")
cat("  - tableau8a_coherence_MRC (csv + docx)\n")
cat("  - tableau8b_sousgroupes (csv + docx)\n\n")

cat("--- 07_sous_groupes.R termine ---\n\n")
