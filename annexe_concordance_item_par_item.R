# Annexe 16 du manuscrit : concordance item par item
# Declaration du patient dans Predict-R vs codage dans le dossier medical.

library(readxl)
library(tidyverse)
library(flextable)
library(irr)
library(here)

# --- Import et filtre analysables ---
d <- read_excel(here("data", "CRD_PredictRVF_data.xlsx"),
                sheet = "donnees", na = c("", "NA"))
d <- d |> filter(!is.na(DM_repere))  # n = 130
cat("n =", nrow(d), "patients analysables\n")

# IMC > 30 depuis PR
d <- d |> mutate(PR_obesite = as.integer(!is.na(PR_IMC) & PR_IMC > 30))

# --- Definition des paires croisables ---
paires <- list(
  list(nom = "HTA",                     pr = "PR_HTA",                 dm = "DM_HTA"),
  list(nom = "Diabete",                 pr = "PR_diabete",             dm = "DM_diabete"),
  list(nom = "Obesite (IMC > 30)",      pr = "PR_obesite",             dm = "DM_obesite"),
  list(nom = "MCV",                     pr = "PR_MCV",                 dm = "DM_MCV"),
  list(nom = "Insuffisance cardiaque",  pr = "PR_insuf_cardiaque",     dm = "DM_insuf_cardiaque"),
  list(nom = "Pathologie urologique",   pr = "PR_maladie_uro",         dm = "DM_patho_uro"),
  list(nom = "Tabagisme",               pr = "PR_tabagisme",           dm = "DM_tabagisme"),
  list(nom = "Dyslipidemie",            pr = "PR_dyslipidemie",        dm = "DM_dyslipidemie"),
  list(nom = "Prematurite",             pr = "PR_prematurite",         dm = "DM_prematurite"),
  list(nom = "Exposition PCI",          pr = "PR_expo_PCI",            dm = "DM_expo_PCI"),
  list(nom = "Radiotherapie renale",    pr = "PR_expo_radiotherapie",  dm = "DM_expo_radiotherapie"),
  list(nom = "AINS (usage regulier)",   pr = "PR_AINS",               dm = "DM_ttt_AINS")
)

# --- Calcul concordance pour chaque paire ---
resultats <- purrr::map_dfr(paires, function(p) {
  pr_val <- as.integer(d[[p$pr]])
  dm_val <- as.integer(d[[p$dm]])

  # Cas complets
  ok <- !is.na(pr_val) & !is.na(dm_val)
  n_ok <- sum(ok)
  pr <- pr_val[ok]
  dm <- dm_val[ok]

  # Effectifs
  pr_pos <- sum(pr == 1)
  dm_pos <- sum(dm == 1)

  # Accord brut
  accord <- sum(pr == dm)
  pct_accord <- accord / n_ok * 100

  # Kappa (si variance suffisante)
  kappa_val <- NA_real_
  if (length(unique(pr)) > 1 & length(unique(dm)) > 1) {
    k <- tryCatch(irr::kappa2(cbind(pr, dm))$value, error = function(e) NA_real_)
    kappa_val <- k
  }

  tibble(
    Item = p$nom,
    `PR+ n` = pr_pos,
    `DM+ n` = dm_pos,
    `Accord n` = accord,
    `Accord %` = sprintf("%.1f", pct_accord),
    Kappa = if (is.na(kappa_val)) "-" else sprintf("%.2f", kappa_val),
    n = n_ok
  )
})

cat("\n=== Resultats ===\n")
print(as.data.frame(resultats))

# --- Flextable ---
ft <- flextable(resultats) |>
  theme_vanilla() |>
  set_caption("Concordance item par item entre Predict-R (auto-declaration) et le dossier medical (n = 130)") |>
  set_header_labels(
    Item = "Facteur de risque",
    `PR+ n` = "PR+\n(n)",
    `DM+ n` = "DM+\n(n)",
    `Accord n` = "Accord\n(n)",
    `Accord %` = "Accord\n(%)",
    Kappa = "Kappa",
    n = "n"
  ) |>
  bold(part = "header") |>
  bold(j = 1) |>
  align(j = 2:7, align = "center", part = "all") |>
  add_footer_lines("PR+ = facteur declare present dans Predict-R. DM+ = facteur code dans le dossier medical.") |>
  add_footer_lines("Accord = les deux sources concordent (toutes deux positives ou toutes deux negatives).") |>
  add_footer_lines("Kappa de Cohen ; \"-\" = variance insuffisante (prevalence nulle dans une source).") |>
  autofit()

save_as_docx(ft, path = here("output", "tableaux", "Concordance_item_par_item.docx"))
cat("\nTableau sauvegarde : output/tableaux/Concordance_item_par_item.docx\n")
