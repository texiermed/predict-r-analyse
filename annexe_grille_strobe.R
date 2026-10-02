# Annexe 5 du manuscrit : grille STROBE renseignee (etude transversale)
# Reference : von Elm E et al. Ann Intern Med 2007;147(8):573-7

library(flextable)
library(officer)
library(here)

strobe <- data.frame(
  Item = c(
    "1a", "1b",
    "2", "3",
    "4", "5", "6a", "7", "8", "9", "10", "11", "12",
    "13", "14", "15", "16", "17",
    "18", "19", "20", "21",
    "22"
  ),
  Section = c(
    "Titre et r\u00e9sum\u00e9", "Titre et r\u00e9sum\u00e9",
    "Introduction", "Introduction",
    "M\u00e9thodes", "M\u00e9thodes", "M\u00e9thodes", "M\u00e9thodes", "M\u00e9thodes",
    "M\u00e9thodes", "M\u00e9thodes", "M\u00e9thodes", "M\u00e9thodes",
    "R\u00e9sultats", "R\u00e9sultats", "R\u00e9sultats", "R\u00e9sultats", "R\u00e9sultats",
    "Discussion", "Discussion", "Discussion", "Discussion",
    "Autres"
  ),
  Description = c(
    "Indiquer le type d'\u00e9tude dans le titre ou le r\u00e9sum\u00e9 avec un terme couramment utilis\u00e9",
    "Fournir dans le r\u00e9sum\u00e9 un texte informatif et \u00e9quilibr\u00e9 de ce qui a \u00e9t\u00e9 fait et trouv\u00e9",
    "Expliquer le contexte scientifique et la justification de la recherche",
    "\u00c9noncer les objectifs sp\u00e9cifiques, y compris toute hypoth\u00e8se pr\u00e9d\u00e9finie",
    "Pr\u00e9senter les \u00e9l\u00e9ments cl\u00e9s du plan d'\u00e9tude d\u00e8s les premi\u00e8res \u00e9tapes",
    "D\u00e9crire le cadre, les lieux et les dates pertinentes (recrutement, exposition, suivi, recueil)",
    "Pr\u00e9ciser les crit\u00e8res d'\u00e9ligibilit\u00e9, les sources et les m\u00e9thodes de s\u00e9lection des participants",
    "D\u00e9finir clairement les variables (crit\u00e8res de jugement, expositions, facteurs pr\u00e9dictifs, confusion, modificateurs d'effet) et les crit\u00e8res diagnostiques",
    "Pr\u00e9ciser les sources de donn\u00e9es et les m\u00e9thodes de mesure pour chaque variable. Si plusieurs groupes, pr\u00e9ciser la comparabilit\u00e9",
    "D\u00e9crire les efforts entrepris pour limiter les sources de biais potentiels",
    "Pr\u00e9ciser comment la taille de l'\u00e9tude a \u00e9t\u00e9 d\u00e9termin\u00e9e",
    "Expliquer comment les variables quantitatives ont \u00e9t\u00e9 trait\u00e9es dans l'analyse. D\u00e9crire et justifier les regroupements \u00e9ventuels",
    "D\u00e9crire toutes les m\u00e9thodes statistiques, y compris contr\u00f4le de la confusion, sous-groupes, interactions, donn\u00e9es manquantes et analyses de sensibilit\u00e9",
    "Indiquer le nombre de participants \u00e0 chaque \u00e9tape (\u00e9ligibles, examin\u00e9s, inclus, analys\u00e9s). Donner les raisons de non-participation. Envisager un diagramme de flux",
    "Donner les caract\u00e9ristiques des participants (d\u00e9mographiques, cliniques, sociales) et les informations sur les expositions et confondants. Indiquer les donn\u00e9es manquantes par variable",
    "\u00c9tude transversale : indiquer le nombre d'\u00e9v\u00e9nements observ\u00e9s ou les mesures de synth\u00e8se",
    "Donner les estimations non ajust\u00e9es et, le cas \u00e9ch\u00e9ant, ajust\u00e9es avec leur pr\u00e9cision (IC 95 %). Pr\u00e9ciser les bornes de cat\u00e9gorisation des variables continues",
    "Pr\u00e9senter les autres analyses effectu\u00e9es (sous-groupes, interactions, sensibilit\u00e9)",
    "R\u00e9sumer les r\u00e9sultats principaux en regard des objectifs de l'\u00e9tude",
    "Discuter les limites de l'\u00e9tude (sources de biais, impr\u00e9cision). Discuter le sens et la magnitude de tout biais potentiel",
    "Fournir une interpr\u00e9tation g\u00e9n\u00e9rale prudente tenant compte des objectifs, limites, multiplicit\u00e9 des analyses et r\u00e9sultats d'\u00e9tudes similaires",
    "Discuter la g\u00e9n\u00e9ralisabilit\u00e9 (validit\u00e9 externe) des r\u00e9sultats",
    "Pr\u00e9ciser la source de financement et le r\u00f4le des financeurs"
  ),
  stringsAsFactors = FALSE
)

ft <- flextable(strobe) |>
  set_header_labels(
    Item = "Item",
    Section = "Section",
    Description = "Recommandation"
  ) |>
  theme_vanilla() |>
  set_caption("Grille STROBE \u2014 \u00c9tude transversale (von Elm et al., 2007)") |>
  bold(part = "header") |>
  bold(j = 1) |>
  fontsize(size = 9, part = "body") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 7.5, part = "footer") |>
  padding(padding.top = 1, padding.bottom = 1, part = "body") |>
  width(j = 1, width = 0.45) |>
  width(j = 2, width = 1.2) |>
  width(j = 3, width = 5.1) |>
  merge_v(j = 2) |>
  valign(j = 2, valign = "top") |>
  add_footer_lines("von Elm E, Altman DG, Egger M et al. Ann Intern Med. 2007;147(8):573-7.")

# Sauvegarder en portrait avec marges reduites pour tenir sur 1 page
sect <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69, orient = "portrait"),
  page_margins = page_mar(top = 0.4, bottom = 0.4, left = 0.5, right = 0.5,
                          header = 0.2, footer = 0.2)
)

save_as_docx(ft, path = here("output", "tableaux", "Grille_STROBE_renseignee.docx"),
             pr_section = sect)
cat("Grille STROBE sauvegardee (portrait, 1 page).\n")
