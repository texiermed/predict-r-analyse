# Annexe 16 du manuscrit : concordance item par item
# Declaration du patient dans Predict-R vs codage dans le dossier medical.

library(readxl)
library(tidyverse)
library(flextable)
library(irr)
library(here)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

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
  list(nom = "Diab\u00e8te",                 pr = "PR_diabete",             dm = "DM_diabete"),
  list(nom = "Ob\u00e9sit\u00e9 (IMC > 30)",      pr = "PR_obesite",             dm = "DM_obesite"),
  list(nom = "MCV",                     pr = "PR_MCV",                 dm = "DM_MCV"),
  list(nom = "Insuffisance cardiaque",  pr = "PR_insuf_cardiaque",     dm = "DM_insuf_cardiaque"),
  list(nom = "Pathologie urologique",   pr = "PR_maladie_uro",         dm = "DM_patho_uro"),
  list(nom = "Tabagisme",               pr = "PR_tabagisme",           dm = "DM_tabagisme"),
  list(nom = "Dyslipid\u00e9mie",            pr = "PR_dyslipidemie",        dm = "DM_dyslipidemie"),
  list(nom = "Pr\u00e9maturit\u00e9",             pr = "PR_prematurite",         dm = "DM_prematurite"),
  list(nom = "Exposition PCI",          pr = "PR_expo_PCI",            dm = "DM_expo_PCI"),
  list(nom = "Radioth\u00e9rapie abdominale",    pr = "PR_expo_radiotherapie",  dm = "DM_expo_radiotherapie"),
  list(nom = "AINS (usage r\u00e9gulier)",   pr = "PR_AINS",               dm = "DM_ttt_AINS")
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
    `Accord %` = sub(".", ",", sprintf("%.1f", pct_accord), fixed = TRUE),
    Kappa = if (is.na(kappa_val)) "-" else sub(".", ",", sprintf("%.2f", kappa_val), fixed = TRUE),
    n = n_ok
  )
})

cat("\n=== Resultats ===\n")
print(as.data.frame(resultats))

# --- Flextable ---
ft <- flextable(resultats) |>
  theme_vanilla() |>
  set_caption(sprintf("Concordance item par item entre Predict-R et le dossier m\u00e9dical (n = %d)", nrow(d))) |>
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
  add_footer_lines("PR+ = facteur d\u00e9clar\u00e9 pr\u00e9sent dans Predict-R. DM+ = facteur retrouv\u00e9 dans le dossier m\u00e9dical.") |>
  add_footer_lines("Accord = les deux sources concordent (toutes deux positives ou toutes deux n\u00e9gatives).") |>
  add_footer_lines("Kappa de Cohen ; \u00ab - \u00bb = pr\u00e9valence nulle dans une source.") |>
  autofit()

save_as_docx(ft, path = here("output", "tableaux", "Concordance_item_par_item.docx"))
cat("\nTableau sauvegarde : output/tableaux/Concordance_item_par_item.docx\n")
