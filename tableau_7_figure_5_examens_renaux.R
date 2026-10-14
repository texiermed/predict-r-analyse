# =============================================================================
#  PREDICT-R - Tableau 7 et Figure 5 (definition STRICTE du depistage complet)
# =============================================================================
#  C'est CETTE version qui alimente le Tableau 7 et la Figure 5 du manuscrit.
#
#  Deux differences volontaires avec `analyse_principale.R` :
#   1. depistage_complet = DFG + test urinaire UNIQUEMENT (le suivi
#      nephrologique n'est PAS compte comme un depistage complet) ;
#   2. DFG_evalue integre `!is.na(DM_DFG_valeur)` : une valeur de DFG datee
#      dans la fenetre vaut DFG evalue meme si la case `DM_DFG_disponible`
#      n'a pas ete cochee (erreur de saisie documentee sur 4 dossiers).
#
#  Base attendue : data/CRD_PredictRVF_data.xlsx (non diffusee, cf. README).
# =============================================================================

library(tidyverse)
library(readxl)
library(flextable)
library(ggplot2)
library(DescTools)
library(here)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"),  recursive = TRUE, showWarnings = FALSE)

d <- read_xlsx(here("data", "CRD_PredictRVF_data.xlsx"), sheet = "donnees", na = c("", "NA"))
eq1 <- function(x) !is.na(x) & x == 1
frf <- function(...) gsub("([0-9])\\.([0-9])", "\\1,\\2", sprintf(...))

# Correction PRED134
d <- d |> mutate(DM_RAC = if_else(id_patient == "PRED134", 1, DM_RAC))

predictr_dm <- d |> filter(!is.na(DM_repere)) |> mutate(
  # Une valeur de DFG datee dans la fenetre vaut DFG evalue, meme si le
  # champ DM_DFG_disponible n'a pas ete coche (erreur de saisie : 4 dossiers).
  DFG_evalue = eq1(DM_DFG_disponible) | eq1(DM_DFG_prescrit_non_realise) |
               !is.na(DM_DFG_valeur),
  test_urinaire = eq1(DM_RAC) | eq1(DM_prot_creat) | eq1(DM_microalbuminurie) |
                  eq1(DM_BU_automate) | eq1(DM_proteinurie),
  fdr_expo_contraste = eq1(DM_expo_PCI) | eq1(DM_expo_radiotherapie),
  nb_FDR_HAS = eq1(DM_diabete) + eq1(DM_HTA) + eq1(DM_MCV) + eq1(DM_insuf_cardiaque) +
    eq1(DM_obesite) + eq1(DM_maladie_auto_immune) + eq1(DM_patho_uro) +
    eq1(DM_ATCD_fam_nephro) + eq1(DM_ATCD_nephro_aigue) +
    eq1(DM_nephrotoxiques) + fdr_expo_contraste + eq1(DM_expo_toxiques_pro),
  cat_FDR_HAS = cut(nb_FDR_HAS, breaks = c(-Inf, 0, 2, Inf),
                    labels = c("0", "1-2", ">=3"), ordered_result = TRUE),
  depistage_niveau = case_when(
    eq1(DM_suivi_nephro)        ~ "Suivi n\u00e9phrologique",
    DFG_evalue & test_urinaire  ~ "Complet (DFG + urinaire)",
    DFG_evalue & !test_urinaire ~ "DFG seul",
    !DFG_evalue & test_urinaire ~ "Test urinaire seul",
    TRUE                        ~ "Aucun"
  ),
  depistage_niveau = factor(depistage_niveau,
    levels = c("Aucun", "DFG seul", "Test urinaire seul",
               "Complet (DFG + urinaire)", "Suivi n\u00e9phrologique")),
  # STRICT : complet = DFG + urinaire UNIQUEMENT
  depistage_complet = as.integer(depistage_niveau == "Complet (DFG + urinaire)")
)
cat("DFG evalue par la seule valeur (case non cochee) :",
    sum(!is.na(predictr_dm$DM_DFG_valeur) & !eq1(predictr_dm$DM_DFG_disponible) &
          !eq1(predictr_dm$DM_DFG_prescrit_non_realise)), "dossiers\n")

# --- Tableau 7 du manuscrit : repartition globale ---
repartition_dep <- predictr_dm |>
  count(depistage_niveau, .drop = FALSE) |>   # garde la ligne « Test urinaire seul » (0)
  mutate(`%` = frf("%.1f", n / sum(n) * 100))

dep_df <- tibble(
  niveau = recode(as.character(repartition_dep$depistage_niveau),
    "Aucun" = "Aucun examen r\u00e9nal",
    "Complet (DFG + urinaire)" = "D\u00e9pistage complet (DFG + urinaire)"),
  n = repartition_dep$n,
  `%` = repartition_dep$`%`
)

# Sous-groupe
sg_fdr <- predictr_dm |> filter(nb_FDR_HAS >= 1)
n_fdr <- nrow(sg_fdr)
n_conf <- sum(sg_fdr$depistage_complet)
ic_conf <- BinomCI(n_conf, n_fdr, method = "wilson")

sg_dfg <- sg_fdr |> filter(DFG_evalue)
n_sg_dfg <- nrow(sg_dfg)
n_sans_rac <- sum(!sg_dfg$test_urinaire)
ic_sans_rac <- BinomCI(n_sans_rac, n_sg_dfg, method = "wilson")

# A posteriori (Methode 5.2.3) : depistages complets dont le test urinaire est un RAC
n_conf_rac <- sum(sg_fdr$depistage_complet == 1 & eq1(sg_fdr$DM_RAC))

# Resultats 9 : examens renaux par niveau de risque Predict-R (% en colonne)
rep_couleur <- predictr_dm |>
  mutate(PR_couleur = factor(PR_couleur, levels = c("vert", "orange", "rouge"))) |>
  count(PR_couleur, depistage_niveau, .drop = FALSE) |>
  group_by(PR_couleur) |>
  mutate(pct = frf("%.1f", n / sum(n) * 100)) |>
  ungroup()

cat("=== Resultats 9 (chiffres du texte) ===\n")
print(repartition_dep)
print(tidyr::pivot_wider(rep_couleur, names_from = PR_couleur, values_from = c(n, pct)))
cat("Sous-groupe >= 1 FDR HAS : n =", n_fdr, "; depistage complet =", n_conf,
    frf("(%.1f %%) [IC 95 %% Wilson %.1f ; %.1f]", n_conf / n_fdr * 100, ic_conf[2] * 100, ic_conf[3] * 100), "\n")
cat("  dont test urinaire = RAC :", n_conf_rac, "sur", n_conf,
    frf("(%.1f %% des %d)", n_conf_rac / n_fdr * 100, n_fdr), "\n")
cat(">= 1 FDR HAS et DFG evalue : n =", n_sg_dfg, "; sans test urinaire =", n_sans_rac,
    frf("(%.1f %%) [IC 95 %% Wilson %.1f ; %.1f]", n_sans_rac / n_sg_dfg * 100, ic_sans_rac[2] * 100, ic_sans_rac[3] * 100), "\n")

flex_dep <- flextable(dep_df) |>
  set_header_labels(niveau = "Examens r\u00e9naux disponibles") |>
  set_caption("Tableau 7. R\u00e9partition des examens r\u00e9naux dans le dossier m\u00e9dical (n = 130)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(frf(
    "Sous-groupe \u2265 1 FDR HAS (n = %d) : d\u00e9pistage complet = %d (%.1f %%), IC 95 %% Wilson [%.1f ; %.1f].",
    n_fdr, n_conf, n_conf/n_fdr*100, ic_conf[2]*100, ic_conf[3]*100)) |>
  add_footer_lines(frf(
    "Parmi les %d patients avec DFG \u00e9valu\u00e9 et \u22651 FDR, %d (%.1f %%) n'avaient pas de test urinaire associ\u00e9.",
    n_sg_dfg, n_sans_rac, n_sans_rac/n_sg_dfg*100)) |>
  add_footer_lines(paste(
    "Cat\u00e9gories exclusives : chaque patient est class\u00e9 une seule fois, dans l'ordre suivi",
    "n\u00e9phrologique, d\u00e9pistage complet, DFG seul, test urinaire seul, aucun examen.",
    "Examens prescrits ou r\u00e9alis\u00e9s depuis le 1er janvier 2025."))

# --- Donnees chiffrees de la Figure 5 : gradient par categorie de FDR HAS ---
tA <- predictr_dm |> group_by(cat_FDR_HAS) |>
  summarise(s = sum(DFG_evalue), n = n(), .groups = "drop")
ptrend_dfg <- prop.trend.test(x = tA$s, n = tA$n)

tB <- predictr_dm |> group_by(cat_FDR_HAS) |>
  summarise(s = sum(depistage_complet), n = n(), .groups = "drop")
ptrend_comp <- prop.trend.test(x = tB$s, n = tB$n)

tab_fdr_dep <- table(FDR = predictr_dm$nb_FDR_HAS >= 1, complet = predictr_dm$depistage_complet)
fisher_fdr_dep <- fisher.test(tab_fdr_dep)

fmt_p_fr <- function(p) {
  if (p < 0.001) return("< 0,001")
  gsub("\\.", ",", sprintf("%.3f", p))
}

gradient <- predictr_dm |>
  group_by(cat_FDR_HAS) |>
  summarise(
    n = n(),
    DFG_eval_pct = frf("%.1f", mean(DFG_evalue) * 100),
    Dep_complet_pct = frf("%.1f", mean(depistage_complet) * 100),
    .groups = "drop"
  )

names(gradient) <- c("FDR HAS", "n", "DFG evalue (%)", "Depistage complet (%)")
flex_grad <- flextable(gradient) |>
  set_caption("Gradient de d\u00e9pistage par cat\u00e9gorie de FDR HAS (donn\u00e9es de la Figure 5)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "Tendance Cochran-Armitage : DFG \u00e9valu\u00e9 p = ",
    fmt_p_fr(ptrend_dfg$p.value),
    " ; d\u00e9pistage complet p = ",
    fmt_p_fr(ptrend_comp$p.value), ".")) |>
  add_footer_lines(paste0(
    "Association \u22651 FDR HAS vs d\u00e9pistage complet : test exact de Fisher, p = ",
    fmt_p_fr(fisher_fdr_dep$p.value), "."))

cat("Fisher >= 1 FDR HAS vs depistage complet : p =", fmt_p_fr(fisher_fdr_dep$p.value),
    "; Cochran-Armitage depistage complet : p =", fmt_p_fr(ptrend_comp$p.value), "\n")
cat("Gradient (Figure 5) :\n"); print(gradient)

save_as_docx(flex_dep, flex_grad,
  path = here("output", "tableaux", "Tableau7_examens_renaux.docx"))
cat("Tableau 7 sauvegarde.\n")

# --- Figure 5 du manuscrit ---
fig5_data <- predictr_dm |>
  group_by(cat_FDR_HAS) |>
  summarise(
    DFG_eval = mean(DFG_evalue) * 100,
    Dep_complet = mean(depistage_complet) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-cat_FDR_HAS, names_to = "Type", values_to = "pct")

fig5 <- fig5_data |>
  ggplot(aes(x = cat_FDR_HAS, y = pct, fill = Type)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_dodge(0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c("DFG_eval" = "#5C9BD5", "Dep_complet" = "#ED7D31"),
    labels = c("DFG_eval" = "DFG \u00e9valu\u00e9", "Dep_complet" = "D\u00e9pistage complet")) +
  labs(title = NULL, x = "Nombre de FDR HAS", y = "Proportion (%)", fill = NULL,
       caption = "D\u00e9pistage complet = DFG + test urinaire (hors suivi n\u00e9phrologique)") +
  scale_y_continuous(limits = c(0, 105)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(here("output", "figures", "Figure5_gradient_depistage.png"),
       fig5, width = 7, height = 5, dpi = 300)
cat("Figure 5 sauvegardee.\n")
