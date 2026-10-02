# ============================================================================
#  Tableaux sous-groupes exploratoires — PREDICT-R
#  Tableau 5 : Rattrapés par tranche d'âge
#  Tableau 6 : Rattrapés par nombre de FDR HAS
#
#  Sortie : output/tableaux/Tableau5_sous_groupe_age.docx
#           output/tableaux/Tableau6_sous_groupe_fdr.docx
# ============================================================================

library(tidyverse)
library(readxl)
library(here)
library(DescTools)    # BinomCI (IC Wilson)
library(flextable)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

set.seed(2026)

# --- Formateurs français ---
frf <- function(...) gsub(".", ",", sprintf(...), fixed = TRUE)
fmt_p_fr <- function(p) {
  if (p < 0.001) return("< 0,001")
  frf("%.3f", p)
}
eq1 <- function(x) !is.na(x) & x == 1

# --- Chargement données ---
predictr <- read_excel(here("data", "CRD_PredictRVF_data.xlsx"))

predictr <- predictr |>
  mutate(
    PR_couleur = factor(PR_couleur, levels = c("vert", "orange", "rouge")),
    PR_bin     = if_else(PR_couleur %in% c("orange", "rouge"), 1L, 0L)
  )

predictr_dm <- predictr |> filter(!is.na(DM_repere))

predictr_dm <- predictr_dm |>
  mutate(
    rattrape = as.integer(PR_bin == 1 & DM_repere == 0),
    fdr_expo_contraste = eq1(DM_expo_PCI) | eq1(DM_expo_radiotherapie),
    nb_FDR_HAS =
      eq1(DM_diabete) + eq1(DM_HTA) + eq1(DM_MCV) + eq1(DM_insuf_cardiaque) +
      eq1(DM_obesite) + eq1(DM_maladie_auto_immune) + eq1(DM_patho_uro) +
      eq1(DM_ATCD_fam_nephro) + eq1(DM_ATCD_nephro_aigue) +
      eq1(DM_nephrotoxiques) + fdr_expo_contraste + eq1(DM_expo_toxiques_pro),
    cat_FDR_HAS = cut(nb_FDR_HAS,
                      breaks = c(-Inf, 0, 2, Inf),
                      labels = c("0", "1-2", "\u22653"),
                      ordered_result = TRUE)
  )

orange_rouge <- predictr_dm |> filter(PR_bin == 1)
cat("Orange/rouge :", nrow(orange_rouge), "\n")
cat("Rattrapes :", sum(orange_rouge$rattrape), "\n\n")

# ============================================================================
# TABLEAU 5 — Rattrapés par tranche d'âge
# ============================================================================

orange_rouge <- orange_rouge |>
  mutate(
    tranche_age = cut(DM_age,
                      breaks = c(-Inf, 40, 60, Inf),
                      labels = c("< 40 ans", "40-60 ans", "> 60 ans"),
                      right = TRUE)
  )

tab_age <- orange_rouge |>
  group_by(tranche_age) |>
  summarise(
    n = n(),
    rattrapes = sum(rattrape),
    .groups = "drop"
  ) |>
  mutate(
    pct = rattrapes / n * 100,
    ic = map2(rattrapes, n, ~ BinomCI(.x, .y, method = "wilson")),
    ic_low  = map_dbl(ic, ~ .[2] * 100),
    ic_high = map_dbl(ic, ~ .[3] * 100)
  ) |>
  select(-ic)

# Ligne Total
total_n   <- sum(tab_age$n)
total_rat <- sum(tab_age$rattrapes)
total_pct <- total_rat / total_n * 100
total_ic  <- BinomCI(total_rat, total_n, method = "wilson")

tab_age <- tab_age |>
  add_row(
    tranche_age = "Total",
    n = total_n, rattrapes = total_rat, pct = total_pct,
    ic_low = total_ic[2] * 100, ic_high = total_ic[3] * 100
  )

# Fisher
fisher_age <- fisher.test(table(orange_rouge$tranche_age, orange_rouge$rattrape))
cat("Fisher age p =", fisher_age$p.value, "\n")

# Formater
tab5_df <- tab_age |>
  transmute(
    `Tranche d'âge` = tranche_age,
    `n` = as.character(n),
    `Rattrapés` = as.character(rattrapes),
    `%` = frf("%.1f", pct),
    `IC 95 % Wilson` = paste0("[", frf("%.1f", ic_low), " ; ", frf("%.1f", ic_high), "]")
  )

flex_age <- flextable(tab5_df) |>
  set_caption("Tableau 5. Proportion de patients rattrapés par tranche d'âge (n = 52, analyse exploratoire).") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = nrow(tab5_df), part = "body") |>
  align(j = 2:5, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "Rattrapés = patients orange/rouge non repérés dans le dossier médical. ",
    "IC Wilson sans correction de continuité.")) |>
  add_footer_lines(paste0(
    "Test exact de Fisher : p = ", fmt_p_fr(fisher_age$p.value), "."))

save_as_docx(flex_age,
             path = here("output", "tableaux", "Tableau5_sous_groupe_age.docx"))
cat("Tableau 5 sauvegardé.\n\n")


# ============================================================================
# TABLEAU 6 — Rattrapés par nombre de FDR HAS
# ============================================================================

tab_fdr <- orange_rouge |>
  group_by(cat_FDR_HAS) |>
  summarise(
    n = n(),
    rattrapes = sum(rattrape),
    .groups = "drop"
  ) |>
  mutate(
    pct = rattrapes / n * 100,
    ic = map2(rattrapes, n, ~ BinomCI(.x, .y, method = "wilson")),
    ic_low  = map_dbl(ic, ~ .[2] * 100),
    ic_high = map_dbl(ic, ~ .[3] * 100)
  ) |>
  select(-ic)

# Ligne Total
tab_fdr <- tab_fdr |>
  add_row(
    cat_FDR_HAS = "Total",
    n = total_n, rattrapes = total_rat, pct = total_pct,
    ic_low = total_ic[2] * 100, ic_high = total_ic[3] * 100
  )

# Fisher
fisher_fdr <- fisher.test(table(orange_rouge$cat_FDR_HAS, orange_rouge$rattrape))
cat("Fisher FDR p =", fisher_fdr$p.value, "\n")

# Formater
tab6_df <- tab_fdr |>
  transmute(
    `FDR HAS` = as.character(cat_FDR_HAS),
    `n` = as.character(n),
    `Rattrapés` = as.character(rattrapes),
    `%` = frf("%.1f", pct),
    `IC 95 % Wilson` = paste0("[", frf("%.1f", ic_low), " ; ", frf("%.1f", ic_high), "]")
  )

flex_fdr <- flextable(tab6_df) |>
  set_caption("Tableau 6. Proportion de patients rattrapés par nombre de FDR HAS (n = 52, analyse exploratoire).") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = nrow(tab6_df), part = "body") |>
  align(j = 2:5, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "FDR HAS = facteurs de risque de dépistage de la MRC (HAS 2021, 12 facteurs). ",
    "Rattrapés = patients orange/rouge non repérés dans le dossier médical.")) |>
  add_footer_lines(paste0(
    "IC Wilson sans correction de continuité. ",
    "Test exact de Fisher : p = ", fmt_p_fr(fisher_fdr$p.value), "."))

save_as_docx(flex_fdr,
             path = here("output", "tableaux", "Tableau6_sous_groupe_fdr.docx"))
cat("Tableau 6 sauvegardé.\n")
cat("\nTerminé. Les tableaux sont dans output/tableaux/.\n")
