# Tableau 11 du manuscrit : satisfaction des patients (n = 39)
# Items Likert + items complementaires, regroupes dans un seul tableau.

library(flextable)
library(tidyverse)
library(here)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

# --- Lecture des donnees ---
sat_pat <- read.csv(here("data", "satisfaction_patient.csv"), fileEncoding = "UTF-8")
n_sat <- nrow(sat_pat)
cat("Satisfaction patients : n =", n_sat, "\n")

# --- Conversion Likert (ordre specifique pour eviter faux positif) ---
to_likert <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(x, "pas du tout") ~ 1L,
    stringr::str_detect(x, "plut.*pas")   ~ 2L,
    stringr::str_detect(x, "plut.*d.accord|plut.*oui") ~ 3L,
    stringr::str_detect(x, "tout") ~ 4L,
    TRUE ~ NA_integer_
  )
}

fmt <- function(n, total) sprintf("%d (%.1f %%)", n, n / total * 100)

# --- Items Likert ---
items <- list(
  list(col = 2, nom = "Simplicit\u00e9 d'utilisation"),
  list(col = 3, nom = "Dur\u00e9e adapt\u00e9e"),
  list(col = 4, nom = "Compr\u00e9hension des questions"),
  list(col = 7, nom = "Comprendre son risque r\u00e9nal"),
  list(col = 9, nom = "Recommandation \u00e0 un proche")
)

likert_df <- purrr::map_dfr(items, function(item) {
  vals <- to_likert(sat_pat[[item$col]])
  n_rep <- sum(!is.na(vals))
  n4 <- sum(vals == 4, na.rm = TRUE)
  n3 <- sum(vals == 3, na.rm = TRUE)
  n2 <- sum(vals == 2, na.rm = TRUE)
  n1 <- sum(vals == 1, na.rm = TRUE)
  tibble(
    Item = item$nom,
    c2 = fmt(n4, n_rep),
    c3 = fmt(n3, n_rep),
    c4 = fmt(n2, n_rep),
    c5 = fmt(n1, n_rep),
    c6 = as.character(n_rep)
  )
})

# --- Items non-Likert ---
intrusion_oui <- sum(stringr::str_detect(tolower(sat_pat[[5]]), "oui"), na.rm = TRUE)

mt_oui <- sum(sat_pat[[8]] == "Oui", na.rm = TRUE)
mt_pe  <- sum(stringr::str_detect(sat_pat[[8]], "tre"), na.rm = TRUE)
mt_non <- sum(sat_pat[[8]] == "Non", na.rm = TRUE)

notes <- sat_pat[[10]]
notes_v <- notes[!is.na(notes)]
n_notes <- length(notes_v)

# --- Ligne separatrice ---
sep <- tibble(Item = "Autres items", c2 = "", c3 = "", c4 = "", c5 = "", c6 = "")

# --- Lignes non-Likert ---
other_df <- tibble(
  Item = c(
    "Questions intrusives",
    "Intention de consulter le MT",
    "Note globale (/10)"
  ),
  c2 = c(
    sprintf("Oui : %d (%.0f %%)  |  Non : %d (%.0f %%)",
            intrusion_oui, intrusion_oui / n_sat * 100,
            n_sat - intrusion_oui, (n_sat - intrusion_oui) / n_sat * 100),
    sprintf("Oui : %d (%.1f %%)  |  Peut-\u00eatre : %d (%.1f %%)  |  Non : %d (%.1f %%)",
            mt_oui, mt_oui / n_sat * 100,
            mt_pe, mt_pe / n_sat * 100,
            mt_non, mt_non / n_sat * 100),
    sprintf("M\u00e9diane : %.0f [Q1 = %d ; Q3 = %d]  |  Moyenne : %.2f  |  \u2265 8 : %d (%.1f %%)",
            median(notes_v), quantile(notes_v, 0.25), quantile(notes_v, 0.75),
            mean(notes_v), sum(notes_v >= 8), sum(notes_v >= 8) / n_notes * 100)
  ),
  c3 = rep("", 3),
  c4 = rep("", 3),
  c5 = rep("", 3),
  c6 = c("39", "39", as.character(n_notes))
)

# --- Assemblage ---
full_df <- bind_rows(likert_df, sep, other_df)

names(full_df) <- c(
  "Item",
  "Tout \u00e0 fait\nd'accord",
  "Plut\u00f4t\nd'accord",
  "Plut\u00f4t pas\nd'accord",
  "Pas du tout\nd'accord",
  "n"
)

# --- Flextable ---
n_lik <- nrow(likert_df)
i_sep <- n_lik + 1
i_other <- (n_lik + 2):(n_lik + 4)

ft <- flextable(full_df) |>
  theme_vanilla() |>
  set_caption(sprintf("Tableau 11. Satisfaction des patients (n = %d)", n_sat)) |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:6, align = "center", part = "all") |>
  # Separateur : fusionner toute la ligne, fond gris
  merge_at(i = i_sep, j = 1:6) |>
  bold(i = i_sep) |>
  italic(i = i_sep) |>
  bg(i = i_sep, bg = "#F0F0F0") |>
  align(i = i_sep, j = 1, align = "left") |>
  # Lignes non-Likert : fusionner colonnes 2-5
  merge_at(i = i_other[1], j = 2:5) |>
  merge_at(i = i_other[2], j = 2:5) |>
  merge_at(i = i_other[3], j = 2:5) |>
  align(i = i_other, j = 2, align = "left") |>
  # Footer
  add_footer_lines(sprintf(
    "Taux de r\u00e9ponse : %d/139 (%.1f %%). \u00c9chelle de Likert : 1 = Pas du tout d'accord, 4 = Tout \u00e0 fait d'accord. MT = m\u00e9decin traitant.",
    n_sat, n_sat / 139 * 100)) |>
  autofit()

# --- Sauvegarde ---
save_as_docx(ft, path = here("output", "tableaux", "Tableau11_satisfaction_patients.docx"))
cat("Tableau 11 sauvegarde.\n")
