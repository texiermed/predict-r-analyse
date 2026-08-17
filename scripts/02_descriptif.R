# ============================================================================
# 02_descriptif.R - Tableau 1 et figures descriptives
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Produire le Tableau 1 (caracteristiques de la population, stratifiees par
#   niveau de risque Predict-R) et quelques figures descriptives.
#
# DEPENDS :
#   - scripts/01_codebook.R (charge `predict_r` et `labels_predict_r`)
#
# REFERENCE METHODOLOGIQUE :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §5 (Analyse descriptive / Tableau 1)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Packages specifiques au descriptif -------------------------------------
library(gtsummary)
library(gt)        # pour exporter le tableau gtsummary
library(ggplot2)


# ============================================================================
# 1. TABLEAU 1 - Caracteristiques de la population (vue MIXTE PR + DM)
# ============================================================================
# Choix methodologique : pour chaque facteur de risque commun aux 2 sources
# (Predict-R et dossier medical), on prend POSITIF = PR OU DM (OR logique).
# Cela donne une description maximale de la population.
# Les variables `mix_*` sont definies dans 01_codebook.R.
#
# Variables sans equivalent : on garde la source unique (DM ou PR seul).
# Sexe : DM_sexe (a confirmer si HEKO l'exporte dans le CSV).
# Age : DM_annee_naissance (source de reference clinique).
#
# `tbl_summary()` choisit automatiquement :
#   - mediane (IQR) pour continues
#   - n (%) pour categorielles
# `add_p()` choisit automatiquement Wilcoxon/Kruskal/Chi2/Fisher
# `add_overall()` ajoute une colonne Total

tableau_1 <- predict_r |>
  select(
    # Demographiques
    age, DM_sexe, PR_IMC,
    # FDR HAS - variables mixtes (PR OU DM)
    mix_HTA, mix_diabete, mix_obesite, mix_MCV, mix_insuf_cardiaque,
    mix_patho_uro, mix_AINS, mix_expo_contraste,
    # FDR DM seul (pas d'equivalent PR)
    DM_maladie_auto_immune, DM_ATCD_fam_nephro, DM_ATCD_nephro_aigue,
    DM_nephrotoxiques, DM_expo_toxiques_pro, DM_maladie_rare_renale,
    # FDR hors HAS - mixtes
    mix_tabagisme, mix_dyslipidemie, mix_prematurite,
    # PR seul (pas d'equivalent DM)
    PR_ATCD_maladie_renale, PR_ATCD_hematurie_proteinurie,
    # BU (PR uniquement, optionnel)
    PR_BU_realisee, PR_BU_hematurie, PR_BU_proteinurie,
    # MRC connue + Score
    DM_MRC_connue, PR_score,
    # Stratification
    PR_couleur
  ) |>
  tbl_summary(
    by = PR_couleur,
    statistic = list(
      all_continuous() ~ "{median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p} %)"
    ),
    label = list(
      age ~ "Age (annees)",
      DM_sexe ~ "Sexe",
      PR_IMC ~ "IMC (kg/m2)",
      mix_HTA ~ "Hypertension arterielle",
      mix_diabete ~ "Diabete",
      mix_obesite ~ "Obesite (IMC > 30 ou ATCD code)",
      mix_MCV ~ "Maladie cardiovasculaire",
      mix_insuf_cardiaque ~ "Insuffisance cardiaque",
      mix_patho_uro ~ "Pathologie urologique",
      mix_AINS ~ "AINS chroniques",
      mix_expo_contraste ~ "Exposition PCI / radiotherapie",
      DM_maladie_auto_immune ~ "Maladie auto-immune",
      DM_ATCD_fam_nephro ~ "ATCD familiaux nephropathie",
      DM_ATCD_nephro_aigue ~ "ATCD nephropathie aigue",
      DM_nephrotoxiques ~ "Exposition nephrotoxiques",
      DM_expo_toxiques_pro ~ "Toxiques professionnels",
      DM_maladie_rare_renale ~ "Maladie rare/genetique renale",
      mix_tabagisme ~ "Tabagisme",
      mix_dyslipidemie ~ "Dyslipidemie",
      mix_prematurite ~ "Prematurite",
      PR_ATCD_maladie_renale ~ "ATCD maladie renale (declare)",
      PR_ATCD_hematurie_proteinurie ~ "ATCD hematurie/proteinurie",
      PR_BU_realisee ~ "Bandelette urinaire realisee",
      PR_BU_hematurie ~ "BU positive : hematurie",
      PR_BU_proteinurie ~ "BU positive : proteinurie",
      DM_MRC_connue ~ "MRC connue",
      PR_score ~ "Score Predict-R total"
    ),
    missing_text = "Donnees manquantes"
  ) |>
  # Tests forces : Kruskal-Wallis pour continues (3 groupes, conservateur).
  # Fisher exact pour categorielles (gere les petits effectifs et evite le warning).
  add_p(test = list(
    all_continuous() ~ "kruskal.test",
    all_categorical() ~ "fisher.test"
  ),
  pvalue_fun = ~ ifelse(.x < 0.001, "< 0,001", style_pvalue(.x, digits = 3))) |>
  add_overall() |>
  modify_header(label ~ "**Caracteristique**") |>
  modify_caption("**Tableau 1.** Caracteristiques de la population par niveau de risque Predict-R (vue mixte PR + DM)") |>
  modify_footnote(
    all_stat_cols() ~ "Mediane [Q1 ; Q3] pour les variables continues ; n (%) pour les variables categorielles.",
    p.value ~ "Test de Kruskal-Wallis (variables continues, 3 groupes) ; test exact de Fisher (variables categorielles, pour gerer les petits effectifs)."
  )

# Affichage console
print(tableau_1)

# Sauvegarde HTML (lisible dans le navigateur) et Word
tableau_1 |>
  as_gt() |>
  gtsave(filename = here("output", "tableaux", "tableau1.html"))

tableau_1 |>
  as_flex_table() |>
  flextable::save_as_docx(path = here("output", "tableaux", "tableau1.docx"))

cat("\nTableau 1 sauvegarde dans output/tableaux/ (HTML + Word)\n\n")


# ============================================================================
# 2. FIGURE 1 - Repartition des niveaux de risque Predict-R
# ============================================================================

fig_repartition <- predict_r |>
  ggplot(aes(x = PR_couleur, fill = PR_couleur)) +
  geom_bar() +
  scale_fill_manual(values = c("vert" = "#4CAF50",
                               "orange" = "#FF9800",
                               "rouge" = "#F44336")) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 4.5) +
  labs(title = "Repartition des niveaux de risque Predict-R",
       x = "Niveau de risque",
       y = "Nombre de patients",
       caption = paste("N =", nrow(predict_r))) +
  theme_classic() +
  theme(legend.position = "none")

print(fig_repartition)

ggsave(here("output", "figures", "fig01_repartition_PR.png"),
       fig_repartition, width = 6, height = 4, dpi = 200)


# ============================================================================
# 3. FIGURE 2 - Age par niveau de risque
# ============================================================================

fig_age <- predict_r |>
  ggplot(aes(x = PR_couleur, y = age, fill = PR_couleur)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("vert" = "#4CAF50",
                               "orange" = "#FF9800",
                               "rouge" = "#F44336")) +
  labs(title = "Distribution de l'age par niveau de risque",
       x = "Niveau de risque",
       y = "Age (annees)") +
  theme_classic() +
  theme(legend.position = "none")

print(fig_age)

ggsave(here("output", "figures", "fig02_age_par_groupe.png"),
       fig_age, width = 6, height = 4, dpi = 200)


# ============================================================================
# 4. FIGURE 3 - Nombre de FDR HAS par niveau de risque
# ============================================================================

fig_fdr <- predict_r |>
  ggplot(aes(x = cat_FDR_HAS, fill = PR_couleur)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("vert" = "#4CAF50",
                               "orange" = "#FF9800",
                               "rouge" = "#F44336")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Repartition des niveaux Predict-R par categorie de FDR HAS",
       x = "Nombre de FDR HAS",
       y = "Proportion",
       fill = "Predict-R") +
  theme_classic()

print(fig_fdr)

ggsave(here("output", "figures", "fig03_PR_par_FDR.png"),
       fig_fdr, width = 7, height = 4, dpi = 200)


cat("\n--- 02_descriptif.R termine ---\n")
cat("Tableau 1 et 3 figures sauvegardes dans output/.\n\n")
