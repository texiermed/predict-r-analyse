# =============================================================================
#  PREDICT-R - Tableau 4 et Figure 4, definition STRICTE du depistage complet
# =============================================================================
#  C'est CETTE version qui alimente le Tableau 4 et la Figure 4 du manuscrit.
#
#  Deux differences volontaires avec `analyse_finale.R` :
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

d <- read_xlsx(here("data", "CRD_PredictRVF_data.xlsx"), na = c("", "NA"))
eq1 <- function(x) !is.na(x) & x == 1
frf <- function(...) sprintf(...)

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

# --- Tableau 4a : repartition globale ---
repartition_dep <- predictr_dm |>
  count(depistage_niveau) |>
  mutate(`%` = frf("%.1f", n / sum(n) * 100))

dep_df <- tibble(
  "Niveau de depistage" = as.character(repartition_dep$depistage_niveau),
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

flex_dep <- flextable(dep_df) |>
  set_caption("Tableau 4a. Niveau de d\u00e9pistage de la MRC (n = 130)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(frf(
    "Sous-groupe \u22651 FDR HAS (n = %d) : d\u00e9pistage complet (DFG + test urinaire) = %d (%.1f %%) [IC 95 %% Wilson : %.1f - %.1f].",
    n_fdr, n_conf, n_conf/n_fdr*100, ic_conf[2]*100, ic_conf[3]*100)) |>
  add_footer_lines(frf(
    "Occasion manqu\u00e9e : parmi les %d patients avec DFG \u00e9valu\u00e9 et \u22651 FDR, %d (%.1f %%) n'avaient pas de test urinaire associ\u00e9.",
    n_sg_dfg, n_sans_rac, n_sans_rac/n_sg_dfg*100))

# --- Tableau 4b : gradient STRICT ---
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
  set_caption("Tableau 4b. Gradient de d\u00e9pistage par cat\u00e9gorie de FDR HAS") |>
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

save_as_docx(flex_dep, flex_grad,
  path = here("output", "tableaux", "Tableau4_depistage_strict.docx"))
cat("Tableau 4 (strict) sauvegarde.\n")

# --- Figure 4 : barres STRICTES ---
fig4_data <- predictr_dm |>
  group_by(cat_FDR_HAS) |>
  summarise(
    DFG_eval = mean(DFG_evalue) * 100,
    Dep_complet = mean(depistage_complet) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-cat_FDR_HAS, names_to = "Type", values_to = "pct")

fig4 <- fig4_data |>
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

ggsave(here("output", "figures", "Figure4_gradient_depistage_strict.png"),
       fig4, width = 7, height = 5, dpi = 300)
cat("Figure 4 (strict) sauvegardee.\n")
