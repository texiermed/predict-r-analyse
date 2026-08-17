# ============================================================================
# 04_concordance_kappa.R - Concordance Predict-R / dossier medical
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Mesurer la concordance entre la classification Predict-R (a risque /
#   faible risque) et le statut de reperage dans le dossier medical
#   (repere / non repere).
#
# METHODES :
#   - Tableau 3 x 2 (PR vert/orange/rouge × DM repere/non repere)
#   - Concordance brute (sur tableau 2 x 2 dichotomise)
#   - Kappa de Cohen + IC 95 % via psych::cohen.kappa()
#   - Interpretation selon Landis & Koch (1977)
#
# REFERENCES METHODOLOGIQUES :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §4 (Kappa de Cohen)
#   - PROTOCOLE_PREDICT-R.md §7.3 (concordance)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Packages specifiques ---------------------------------------------------
library(irr)        # Kappa simple
library(psych)      # Kappa avec IC 95 %
library(flextable)  # tableaux Word formates


# ============================================================================
# 1. TABLEAU 3 x 2 - PR (3 niveaux) x DM (2 statuts) avec % par ligne
# ============================================================================

# Construire le tableau croise avec n (%) par ligne
tab_3x2_brut <- predict_r |>
  count(PR_couleur, repere) |>
  pivot_wider(names_from = repere,
              values_from = n,
              values_fill = 0) |>
  rename(non_repere = `0`, repere_n = `1`) |>
  mutate(Total = non_repere + repere_n,
         pct_nonrep = non_repere / Total * 100,
         pct_rep = repere_n / Total * 100)

# Format n (% ligne) pour chaque cellule
tab_3x2_format <- tab_3x2_brut |>
  mutate(
    `Predict-R` = as.character(PR_couleur),
    `Non repere n (%)` = sprintf("%d (%.1f %%)", non_repere, pct_nonrep),
    `Repere n (%)` = sprintf("%d (%.1f %%)", repere_n, pct_rep),
    Total = as.character(Total)
  ) |>
  select(`Predict-R`, `Non repere n (%)`, `Repere n (%)`, Total)

# Ligne des totaux marginaux
total_n_rep <- sum(tab_3x2_brut$non_repere)
total_rep <- sum(tab_3x2_brut$repere_n)
total_general <- sum(tab_3x2_brut$Total)

total_row <- tibble(
  `Predict-R` = "Total",
  `Non repere n (%)` = sprintf("%d (%.1f %%)",
                                total_n_rep,
                                total_n_rep / total_general * 100),
  `Repere n (%)` = sprintf("%d (%.1f %%)",
                           total_rep,
                           total_rep / total_general * 100),
  Total = as.character(total_general)
)

tab_3x2_complet <- bind_rows(tab_3x2_format, total_row)

cat("---------------------------------\n")
cat("TABLEAU 3 x 2 - PR x DM (avec % par ligne)\n")
cat("---------------------------------\n")
print(tab_3x2_complet)
cat("\n")

write_csv(tab_3x2_complet,
          here("output", "tableaux", "tableau3_concordance_3x2.csv"))

# Version Word formatee
flex_3x2 <- flextable(tab_3x2_complet) |>
  set_caption("Tableau 3. Concordance Predict-R / dossier medical - distribution croisee") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = nrow(tab_3x2_complet)) |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines("Les pourcentages sont calcules en ligne (% au sein de chaque niveau Predict-R).")

save_as_docx(flex_3x2,
             path = here("output", "tableaux", "tableau3_concordance_3x2.docx"))


# ============================================================================
# 2. TABLEAU 2 x 2 dichotomise - pour le Kappa
# ============================================================================
# On binarise PR : vert (0) vs orange/rouge (1)

tab_2x2 <- table(PR = predict_r$PR_bin,
                 DM = predict_r$repere)

# Renommer les dimensions pour lisibilite
dimnames(tab_2x2) <- list(
  PR = c("vert", "orange/rouge"),
  DM = c("non_repere", "repere")
)

cat("Tableau 2 x 2 dichotomise :\n")
print(tab_2x2)
cat("\n")

# Version tibble avec totaux marginaux pour export
tab_2x2_df <- tibble(
  `Predict-R` = c("Faible risque (vert)",
                  "A risque (orange/rouge)",
                  "Total"),
  `Non repere` = c(tab_2x2["vert", "non_repere"],
                   tab_2x2["orange/rouge", "non_repere"],
                   sum(tab_2x2[, "non_repere"])),
  `Repere` = c(tab_2x2["vert", "repere"],
               tab_2x2["orange/rouge", "repere"],
               sum(tab_2x2[, "repere"])),
  Total = c(sum(tab_2x2["vert", ]),
            sum(tab_2x2["orange/rouge", ]),
            sum(tab_2x2))
)

flex_2x2 <- flextable(tab_2x2_df) |>
  set_caption("Tableau 3b. Concordance Predict-R / dossier medical (2 x 2 dichotomise)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = nrow(tab_2x2_df)) |>
  align(j = 2:4, align = "center", part = "all")

save_as_docx(flex_2x2,
             path = here("output", "tableaux", "tableau3b_concordance_2x2.docx"))


# ============================================================================
# 3. CONCORDANCE BRUTE
# ============================================================================
# Concordance brute = (a + d) / total
#   a = vert + non_repere (concordant : pas de risque, pas reperage)
#   d = orange/rouge + repere (concordant : risque, reperage actif)

concordance_brute <- (tab_2x2["vert", "non_repere"] +
                      tab_2x2["orange/rouge", "repere"]) / sum(tab_2x2)

cat("Concordance brute :",
    round(concordance_brute * 100, 1), "%\n\n")


# ============================================================================
# 4. KAPPA DE COHEN avec IC 95 %
# ============================================================================
# psych::cohen.kappa() donne le Kappa avec IC 95 % (recommande pour la these).

kappa_psych <- psych::cohen.kappa(
  cbind(predict_r$PR_bin, predict_r$repere)
)

kappa_value <- kappa_psych$kappa
kappa_ic <- kappa_psych$confid["unweighted kappa", ]
# kappa_ic est un vecteur : lower, estimate, upper

# Verification croisee avec irr::kappa2()
kappa_irr <- kappa2(data.frame(predict_r$PR_bin, predict_r$repere))

cat("Kappa de Cohen :\n")
cat("  Valeur          :", round(kappa_value, 3), "\n")
cat("  IC 95 %         : [",
    round(kappa_ic["lower"], 3), " - ",
    round(kappa_ic["upper"], 3), "]\n", sep = "")
cat("  p-value (irr)   :", format(kappa_irr$p.value, digits = 4), "\n\n")


# ============================================================================
# 5. INTERPRETATION (Landis & Koch 1977)
# ============================================================================

interp <- case_when(
  kappa_value < 0   ~ "negatif (pire que le hasard)",
  kappa_value < 0.20 ~ "tres faible",
  kappa_value < 0.41 ~ "faible",
  kappa_value < 0.61 ~ "modere",
  kappa_value < 0.81 ~ "bon",
  TRUE              ~ "tres bon"
)

cat("Interpretation (Landis & Koch 1977) : accord", interp, "\n\n")

cat("--> Un Kappa faible ou modere est attendu pour PREDICT-R :\n")
cat("    c'est la preuve que l'outil et le DM ne sont PAS redondants.\n")
cat("    Justification : GUIDE_STATISTIQUE_PREDICT-R.md §4.\n")
cat("    Paradoxe de Cicchetti-Feinstein : un desequilibre des marges\n")
cat("    peut abaisser artificiellement le Kappa malgre une concordance\n")
cat("    brute substantielle - a mentionner en discussion.\n\n")


# ============================================================================
# 4b. ANALYSE DE SENSIBILITE - PABAK
# ============================================================================
# PABAK = Prevalence-Adjusted Bias-Adjusted Kappa (Byrt et al. 1993)
# Corrige les 2 effets parasites du Kappa de Cohen :
#   - effet de prevalence (marges desequilibrees)
#   - effet de biais (taux de positifs differents entre les 2 evaluateurs)
# Quand le Kappa est faible par paradoxe Cicchetti-Feinstein, le PABAK
# retablit une mesure interpretable de la concordance reelle.

# Formule : PABAK = 2 * concordance_brute - 1
pabak <- 2 * concordance_brute - 1

interp_pabak <- case_when(
  pabak < 0   ~ "negatif",
  pabak < 0.20 ~ "tres faible",
  pabak < 0.41 ~ "faible",
  pabak < 0.61 ~ "modere",
  pabak < 0.81 ~ "bon",
  TRUE        ~ "tres bon"
)

cat("Analyse de sensibilite - PABAK (Byrt 1993) :\n")
cat(sprintf("  PABAK = 2 x %.3f - 1 = %.3f (accord %s)\n",
            concordance_brute, pabak, interp_pabak))
cat("  --> Le PABAK est utilise pour rapporter une concordance ajustee\n")
cat("      sur la prevalence quand le Kappa de Cohen est paradoxalement\n")
cat("      faible. A presenter en complement, pas en remplacement.\n\n")


# ============================================================================
# 6. SAUVEGARDE - tableau Kappa formate
# ============================================================================

# Formatages publication
pval_kappa_str <- ifelse(kappa_irr$p.value < 0.001,
                         "< 0,001",
                         sprintf("%.3f", kappa_irr$p.value))
ic_kappa_str <- sprintf("[%.3f ; %.3f]",
                        kappa_ic["lower"], kappa_ic["upper"])

tableau_kappa <- tibble(
  Indicateur = c("Concordance brute",
                 "Kappa de Cohen",
                 "IC 95 % du Kappa",
                 "p-value Kappa",
                 "Interpretation Kappa (Landis & Koch 1977)",
                 "PABAK (sensibilite - Byrt 1993)",
                 "Interpretation PABAK"),
  Valeur = c(sprintf("%.1f %%", concordance_brute * 100),
             sprintf("%.3f", kappa_value),
             ic_kappa_str,
             paste0("p = ", pval_kappa_str),
             paste("Accord", interp),
             sprintf("%.3f", pabak),
             paste("Accord", interp_pabak))
)

print(tableau_kappa)

write_csv(tableau_kappa,
          here("output", "tableaux", "tableau4_kappa.csv"))

# Version Word formatee
flex_kappa <- flextable(tableau_kappa) |>
  set_caption("Tableau 4. Concordance Predict-R / dossier medical - Kappa de Cohen") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2, align = "left", part = "all")

save_as_docx(flex_kappa,
             path = here("output", "tableaux", "tableau4_kappa.docx"))


cat("\nTableaux sauvegardes :\n")
cat("  - tableau3_concordance_3x2.docx (PR x DM avec totaux)\n")
cat("  - tableau3b_concordance_2x2.docx (dichotomise, pour Kappa)\n")
cat("  - tableau4_kappa.docx (resultat Kappa formate)\n\n")

cat("--- 04_concordance_kappa.R termine ---\n\n")
