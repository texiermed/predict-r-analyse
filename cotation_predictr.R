# cotation_predictr.R
# Tableau des items ponderes du score Predict-R (annexe)
# Source : Cotation_PredictR.xlsx (capture ecran verifiee)

library(flextable)
library(officer)
library(here)

cotation <- data.frame(
  N = 1:14,
  Type = c(
    rep("D\u00e9claratif", 12),
    rep("Biologique (BU)", 2)
  ),
  Item = c(
    "Pr\u00e9maturit\u00e9",
    "Diab\u00e8te",
    "Hypertension art\u00e9rielle (trait\u00e9e ou non)",
    "Tabagisme quotidien",
    "Dyslipid\u00e9mie",
    "Maladie cardiovasculaire (AVC, IDM, AOMI\u2026)",
    "Insuffisance cardiaque",
    "Ob\u00e9sit\u00e9 (IMC > 30 kg/m\u00b2)",
    "Pathologie urologique (IU r\u00e9cidivantes, calculs, py\u00e9lon\u00e9phrites, n\u00e9phrectomie, malformation)",
    "AINS r\u00e9guliers (\u2265 1\u00d7/semaine)",
    "Exposition aux produits de contraste iod\u00e9s (\u2265 1\u00d7/an)",
    "Ant\u00e9c\u00e9dent de radioth\u00e9rapie abdominale",
    "H\u00e9maturie (bandelette urinaire positive)",
    "Prot\u00e9inurie (bandelette urinaire positive)"
  ),
  Questions = c(
    "Q5", "Q6 + Q7", "Q8", "Q9", "Q10", "Q11", "Q12",
    "Q13 + Q14", "Q15", "Q18", "Q19", "Q20", "Q24", "Q25"
  ),
  Points = c(
    "1", "2 ou 4 *", "2", "1", "1", "1", "1",
    "1", "3", "1", "1", "1", "6", "6"
  ),
  stringsAsFactors = FALSE
)

ft <- flextable(cotation) |>
  set_header_labels(
    N = "N\u00b0",
    Type = "Type",
    Item = "Item cot\u00e9",
    Questions = "Question(s)",
    Points = "Points"
  ) |>
  theme_vanilla() |>
  set_caption("Items pond\u00e9r\u00e9s du score Predict-R (14 items cot\u00e9s / 16 questions)") |>
  bold(part = "header") |>
  bold(j = 1) |>
  fontsize(size = 9.5, part = "body") |>
  fontsize(size = 9.5, part = "header") |>
  width(j = 1, width = 0.35) |>
  width(j = 2, width = 1.05) |>
  width(j = 3, width = 3.4) |>
  width(j = 4, width = 0.8) |>
  width(j = 5, width = 0.7) |>
  merge_v(j = 2) |>
  valign(j = 2, valign = "top") |>
  align(j = c(1, 4, 5), align = "center") |>
  align(j = c(1, 4, 5), align = "center", part = "header") |>
  add_footer_lines("* Diab\u00e8te < 5 ans = 2 points ; diab\u00e8te \u2265 5 ans = 4 points.") |>
  add_footer_lines("Score < 4 : risque faible (vert) \u2014 Score [4 ; 6[ : risque mod\u00e9r\u00e9 (orange) \u2014 Score \u2265 6 : risque \u00e9lev\u00e9 (rouge).") |>
  add_footer_lines("Les items 13-14 sont optionnels et issus de la bandelette urinaire (BU).") |>
  fontsize(size = 8, part = "footer") |>
  padding(padding.top = 2, padding.bottom = 2, part = "body")

sect <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69, orient = "portrait"),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.7, right = 0.7,
                          header = 0.2, footer = 0.2)
)

save_as_docx(ft, path = here("output", "tableaux", "Cotation_PredictR.docx"),
             pr_section = sect)
cat("Tableau cotation Predict-R corrige et sauvegarde.\n")
