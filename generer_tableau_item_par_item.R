# Tableau comparatif item par item : Predict-R (auto-declare) vs Dossier Medical
# Pour les 130 patients analysables

library(readxl)
library(flextable)
library(tidyverse)
library(here)

d <- read_excel(here("data", "CRD_PredictRVF_data.xlsx"))
# Garder les 130 analysables (DM_repere non NA)
d <- d[!is.na(d$DM_repere), ]
n <- nrow(d)
cat("Patients analysables:", n, "\n")

# Paires comparables PR vs DM
paires <- list(
  list(pr = "PR_HTA",              dm = "DM_HTA",              nom = "HTA"),
  list(pr = "PR_diabete",          dm = "DM_diabete",           nom = "Diabete"),
  list(pr = "PR_tabagisme",        dm = "DM_tabagisme",         nom = "Tabagisme"),
  list(pr = "PR_dyslipidemie",     dm = "DM_dyslipidemie",      nom = "Dyslipidemie"),
  list(pr = "PR_MCV",              dm = "DM_MCV",               nom = "Maladie cardiovasculaire"),
  list(pr = "PR_insuf_cardiaque",  dm = "DM_insuf_cardiaque",   nom = "Insuffisance cardiaque"),
  list(pr = "PR_maladie_uro",      dm = "DM_patho_uro",         nom = "Pathologie urologique"),
  list(pr = "PR_prematurite",      dm = "DM_prematurite",       nom = "Prematurite"),
  list(pr = "PR_expo_PCI",         dm = "DM_expo_PCI",          nom = "Exposition PCI/radiotherapie")
)

# IMC : PR_IMC > 30 vs DM_obesite
d$PR_obesite <- as.integer(!is.na(d$PR_IMC) & d$PR_IMC > 30)
paires <- c(paires, list(
  list(pr = "PR_obesite", dm = "DM_obesite", nom = "Obesite (IMC > 30)")
))

# AINS : PR_AINS vs DM_ttt_AINS
paires <- c(paires, list(
  list(pr = "PR_AINS", dm = "DM_ttt_AINS", nom = "AINS/nephrotoxiques")
))

results <- purrr::map_dfr(paires, function(p) {
  pr_val <- as.integer(d[[p$pr]])
  dm_val <- as.integer(d[[p$dm]])
  # Remplacer NA par 0
  pr_val[is.na(pr_val)] <- 0L
  dm_val[is.na(dm_val)] <- 0L

  both_pos  <- sum(pr_val == 1 & dm_val == 1)
  pr_only   <- sum(pr_val == 1 & dm_val == 0)
  dm_only   <- sum(pr_val == 0 & dm_val == 1)
  both_neg  <- sum(pr_val == 0 & dm_val == 0)
  accord    <- both_pos + both_neg

  tibble(
    Item = p$nom,
    PR_pos = sprintf("%d (%.1f)", sum(pr_val), sum(pr_val)/n*100),
    DM_pos = sprintf("%d (%.1f)", sum(dm_val), sum(dm_val)/n*100),
    Accord = sprintf("%d/%d (%.1f)", accord, n, accord/n*100),
    PR_seul = as.character(pr_only),
    DM_seul = as.character(dm_only)
  )
})

names(results) <- c(
  "Facteur de risque",
  "Predict-R +\nn (%)",
  "Dossier medical +\nn (%)",
  "Concordance\nn/N (%)",
  "PR+ / DM-",
  "PR- / DM+"
)

cat("\n=== Tableau comparatif ===\n")
print(as.data.frame(results))

# Flextable
ft <- flextable(results) |>
  theme_vanilla() |>
  set_caption("Concordance item par item entre Predict-R (auto-declare) et le dossier medical (n = 130)") |>
  bold(part = "header") |>
  bold(j = 1) |>
  align(j = 2:6, align = "center", part = "all") |>
  add_footer_lines("PR+ / DM- : facteur declare par le patient mais absent du dossier. PR- / DM+ : facteur present dans le dossier mais non declare. Les donnees manquantes sont traitees comme des absences.") |>
  autofit()

save_as_docx(ft, path = here("output", "tableaux", "Tableau_concordance_item_par_item.docx"))
cat("\nTableau sauvegarde dans output/tableaux/Tableau_concordance_item_par_item.docx\n")
