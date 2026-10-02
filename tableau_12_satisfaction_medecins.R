# Tableau 12 du manuscrit : satisfaction des medecins (n = 9)
# Items Likert + questions ouvertes, regroupes dans un seul tableau.

library(flextable)
library(tidyverse)
library(here)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

# --- Lecture des donnees ---
sat_med <- read.csv(here("data", "satisfaction_medecin.csv"), fileEncoding = "UTF-8")
n_sat <- nrow(sat_med)
cat("Satisfaction m\u00e9decins : n =", n_sat, "\n")

# --- Conversion Likert ---
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

# --- Items Likert (7) ---
items <- list(
  list(col = 2, nom = "Facile \u00e0 comprendre pour les patients"),
  list(col = 3, nom = "Pr\u00e9sentation claire et lisible"),
  list(col = 4, nom = "Am\u00e9liorer le rep\u00e9rage MRC"),
  list(col = 5, nom = "Identifier des patients non rep\u00e9r\u00e9s"),
  list(col = 6, nom = "Favorable \u00e0 l'utilisation"),
  list(col = 7, nom = "Int\u00e9grer dans la pratique"),
  list(col = 8, nom = "Recommander \u00e0 des confr\u00e8res")
)

likert_df <- purrr::map_dfr(items, function(item) {
  vals <- to_likert(sat_med[[item$col]])
  n_rep <- sum(!is.na(vals))
  n4 <- sum(vals == 4, na.rm = TRUE)
  n3 <- sum(vals == 3, na.rm = TRUE)
  n2 <- sum(vals == 2, na.rm = TRUE)
  n1 <- sum(vals == 1, na.rm = TRUE)
  tibble(
    Item = item$nom,
    c2 = sprintf("%d/%d", n4, n_rep),
    c3 = sprintf("%d/%d", n3, n_rep),
    c4 = sprintf("%d/%d", n2, n_rep),
    c5 = sprintf("%d/%d", n1, n_rep),
    c6 = as.character(n_rep)
  )
})

# --- Separateur ---
sep <- tibble(Item = "Questions ouvertes", c2 = "", c3 = "", c4 = "", c5 = "", c6 = "")

# --- Reponses ouvertes (synthese thematique) ---
other_df <- tibble(
  Item = c(
    "Freins identifi\u00e9s",
    "Suggestions"
  ),
  c2 = c(
    "Fracture num\u00e9rique des personnes \u00e2g\u00e9es (3/9)  |  Temps de r\u00e9alisation (1/9)  |  Pas de frein franc (1/9)",
    "Version papier  |  Ajout ant\u00e9c\u00e9dents familiaux  |  Visuel plus agr\u00e9able"
  ),
  c3 = rep("", 2),
  c4 = rep("", 2),
  c5 = rep("", 2),
  c6 = c(as.character(n_sat), as.character(n_sat))
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
n_lik <- nrow(likert_df)   # 7
i_sep <- n_lik + 1          # 8
i_other <- (n_lik + 2):(n_lik + 3)  # 9, 10

ft <- flextable(full_df) |>
  theme_vanilla() |>
  set_caption(sprintf("Tableau 12. Satisfaction des m\u00e9decins (n = %d)", n_sat)) |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:6, align = "center", part = "all") |>
  # Separateur
  merge_at(i = i_sep, j = 1:6) |>
  bold(i = i_sep) |>
  italic(i = i_sep) |>
  bg(i = i_sep, bg = "#F0F0F0") |>
  align(i = i_sep, j = 1, align = "left") |>
  # Lignes ouvertes : fusionner colonnes 2-5
  merge_at(i = i_other[1], j = 2:5) |>
  merge_at(i = i_other[2], j = 2:5) |>
  align(i = i_other, j = 2, align = "left") |>
  # Footer
  add_footer_lines(sprintf(
    "Taux de r\u00e9ponse : %d/9 (100 %%). Format k/n (pourcentages omis, effectif limit\u00e9). Aucune r\u00e9ponse n\u00e9gative sur l'ensemble des items Likert.",
    n_sat)) |>
  autofit()

# --- Sauvegarde ---
save_as_docx(ft, path = here("output", "tableaux", "Tableau12_satisfaction_medecins.docx"))
cat("Tableau 12 sauvegard\u00e9.\n")
