# Annexe 5 du manuscrit : grille STROBE renseignee (etude transversale, 32 lignes)
# Version du 13/10/2026 : sous-items 12a-12e, 13a-13c, 14a-14b, 16a-16c et
# colonne "Ou dans la these". Source : checklist STROBE des etudes transversales,
# reference (56) du manuscrit (Vandenbroucke et al., Epidemiology 2007).

library(flextable)
library(officer)
library(here)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

strobe <- data.frame(
  Item = c(
    "1a",
    "1b",
    "2",
    "3",
    "4",
    "5",
    "6a",
    "7",
    "8*",
    "9",
    "10",
    "11",
    "12a",
    "12b",
    "12c",
    "12d",
    "12e",
    "13a*",
    "13b*",
    "13c*",
    "14a*",
    "14b*",
    "15*",
    "16a",
    "16b",
    "16c",
    "17",
    "18",
    "19",
    "20",
    "21",
    "22"
  ),
  Rubrique = c(
    "Titre et r\u00e9sum\u00e9",
    "",
    "Introduction
Contexte",
    "Objectifs",
    "M\u00e9thodes
Plan d\u2019\u00e9tude",
    "Cadre",
    "Participants",
    "Variables",
    "Sources de donn\u00e9es / mesure",
    "Biais",
    "Taille de l\u2019\u00e9tude",
    "Variables quantitatives",
    "M\u00e9thodes statistiques",
    "",
    "",
    "",
    "",
    "R\u00e9sultats
Participants",
    "",
    "",
    "Donn\u00e9es descriptives",
    "",
    "Crit\u00e8re de jugement",
    "R\u00e9sultats principaux",
    "",
    "",
    "Autres analyses",
    "Discussion
R\u00e9sultats cl\u00e9s",
    "Limites",
    "Interpr\u00e9tation",
    "G\u00e9n\u00e9ralisabilit\u00e9",
    "Autres informations
Financement"
  ),
  Recommandation = c(
    "Indiquer le type d\u2019\u00e9tude dans le titre ou le r\u00e9sum\u00e9 avec un terme couramment utilis\u00e9",
    "Fournir dans le r\u00e9sum\u00e9 un texte informatif et \u00e9quilibr\u00e9 de ce qui a \u00e9t\u00e9 fait et trouv\u00e9",
    "Expliquer le contexte scientifique et la justification de la recherche",
    "\u00c9noncer les objectifs sp\u00e9cifiques, y compris toute hypoth\u00e8se pr\u00e9d\u00e9finie",
    "Pr\u00e9senter les \u00e9l\u00e9ments cl\u00e9s du plan d\u2019\u00e9tude d\u00e8s le d\u00e9but du document",
    "D\u00e9crire le cadre, les lieux et les dates pertinentes, y compris les p\u00e9riodes de recrutement, d\u2019exposition, de suivi et de recueil des donn\u00e9es",
    "Pr\u00e9ciser les crit\u00e8res d\u2019\u00e9ligibilit\u00e9, les sources et les m\u00e9thodes de s\u00e9lection des participants",
    "D\u00e9finir clairement tous les crit\u00e8res de jugement, expositions, facteurs pr\u00e9dictifs, facteurs de confusion potentiels et modificateurs d\u2019effet. Donner les crit\u00e8res diagnostiques, le cas \u00e9ch\u00e9ant",
    "Pour chaque variable d\u2019int\u00e9r\u00eat, pr\u00e9ciser les sources de donn\u00e9es et les m\u00e9thodes de mesure. D\u00e9crire la comparabilit\u00e9 des m\u00e9thodes de mesure s\u2019il y a plusieurs groupes",
    "D\u00e9crire les efforts entrepris pour limiter les sources de biais potentiels",
    "Pr\u00e9ciser comment la taille de l\u2019\u00e9tude a \u00e9t\u00e9 d\u00e9termin\u00e9e",
    "Expliquer comment les variables quantitatives ont \u00e9t\u00e9 trait\u00e9es dans l\u2019analyse. Le cas \u00e9ch\u00e9ant, d\u00e9crire et justifier les regroupements",
    "D\u00e9crire toutes les m\u00e9thodes statistiques, y compris celles utilis\u00e9es pour contr\u00f4ler les facteurs de confusion",
    "D\u00e9crire les m\u00e9thodes utilis\u00e9es pour \u00e9tudier les sous-groupes et les interactions",
    "Expliquer comment les donn\u00e9es manquantes ont \u00e9t\u00e9 trait\u00e9es",
    "Le cas \u00e9ch\u00e9ant, d\u00e9crire les m\u00e9thodes d\u2019analyse tenant compte de la strat\u00e9gie d\u2019\u00e9chantillonnage",
    "D\u00e9crire les analyses de sensibilit\u00e9",
    "Indiquer le nombre de participants \u00e0 chaque \u00e9tape de l\u2019\u00e9tude (par exemple : potentiellement \u00e9ligibles, examin\u00e9s pour l\u2019\u00e9ligibilit\u00e9, confirm\u00e9s \u00e9ligibles, inclus, ayant termin\u00e9 le suivi et analys\u00e9s)",
    "Donner les raisons de non-participation \u00e0 chaque \u00e9tape",
    "Envisager un diagramme de flux",
    "Donner les caract\u00e9ristiques des participants (d\u00e9mographiques, cliniques, sociales) et les informations sur les expositions et les facteurs de confusion potentiels",
    "Indiquer le nombre de participants ayant des donn\u00e9es manquantes pour chaque variable d\u2019int\u00e9r\u00eat",
    "Indiquer le nombre d\u2019\u00e9v\u00e8nements observ\u00e9s ou les mesures de synth\u00e8se",
    "Donner les estimations non ajust\u00e9es et, le cas \u00e9ch\u00e9ant, ajust\u00e9es sur les facteurs de confusion, avec leur pr\u00e9cision (par exemple IC 95 %). Pr\u00e9ciser quels facteurs de confusion ont \u00e9t\u00e9 pris en compte et pourquoi",
    "Indiquer les bornes des cat\u00e9gories lorsque des variables continues ont \u00e9t\u00e9 cat\u00e9goris\u00e9es",
    "Le cas \u00e9ch\u00e9ant, envisager de traduire les risques relatifs en risques absolus sur une p\u00e9riode pertinente",
    "Pr\u00e9senter les autres analyses effectu\u00e9es (sous-groupes, interactions, analyses de sensibilit\u00e9)",
    "R\u00e9sumer les r\u00e9sultats principaux en regard des objectifs de l\u2019\u00e9tude",
    "Discuter les limites de l\u2019\u00e9tude (sources de biais potentiels, impr\u00e9cision). Discuter le sens et l\u2019ampleur de tout biais potentiel",
    "Fournir une interpr\u00e9tation g\u00e9n\u00e9rale prudente des r\u00e9sultats tenant compte des objectifs, des limites, de la multiplicit\u00e9 des analyses, des r\u00e9sultats d\u2019\u00e9tudes similaires et des autres donn\u00e9es pertinentes",
    "Discuter la g\u00e9n\u00e9ralisabilit\u00e9 (validit\u00e9 externe) des r\u00e9sultats",
    "Pr\u00e9ciser la source de financement et le r\u00f4le des financeurs pour la pr\u00e9sente \u00e9tude et, le cas \u00e9ch\u00e9ant, pour l\u2019\u00e9tude originale sur laquelle elle repose"
  ),
  Ou = c(
    "R\u00e9sum\u00e9 et abstract (Mat\u00e9riel et m\u00e9thode)",
    "R\u00e9sum\u00e9 et abstract",
    "Introduction 1 \u00e0 7",
    "Introduction 8 ; hypoth\u00e8se (seuil de 10 %) : M\u00e9thode 5.1 et 6.3",
    "M\u00e9thode 1.1 ; r\u00e9sum\u00e9",
    "M\u00e9thode 2.1, 2.2 et 3.2 ; R\u00e9sultats 1. Suivi : sans objet (\u00e9tude transversale)",
    "M\u00e9thode 1.2 et 3.1",
    "M\u00e9thode 4 et 5 (crit\u00e8re principal : 5.1) ; crit\u00e8res diagnostiques : M\u00e9thode 4.2 et 5.2.4",
    "M\u00e9thode 3, 4 et Annexe 9 ; m\u00eame proc\u00e9dure de recueil pour tous les patients (M\u00e9thode 3.2)",
    "M\u00e9thode 6.6 ; aussi 1.2.4 et 3.2",
    "M\u00e9thode 2.3",
    "M\u00e9thode 6.1 ; regroupements : M\u00e9thode 4.1.3 et 5.3",
    "M\u00e9thode 6.1 \u00e0 6.4. Confusion : sans objet (\u00e9tude descriptive, pas d\u2019analyse ajust\u00e9e)",
    "M\u00e9thode 5.3 et 6.2. Interactions : non \u00e9tudi\u00e9es",
    "M\u00e9thode 6.5",
    "Sans objet (pas de plan de sondage ni de pond\u00e9ration)",
    "M\u00e9thode 5.3 ; robustesse du seuil : M\u00e9thode 6.3",
    "R\u00e9sultats 1, Figure 2 et Tableau 10",
    "R\u00e9sultats 1 et 11",
    "R\u00e9sultats 1, Figure 2",
    "R\u00e9sultats 2 et 3, Tableau 1",
    "Notes du Tableau 1 ; R\u00e9sultats 11 ; Tableau 11",
    "R\u00e9sultats 4.1 et Tableau 2 ; R\u00e9sultats 5 et Tableau 3",
    "R\u00e9sultats 4.1, 5 et 10 (Tableaux 2, 4 et 9). Ajustement : sans objet",
    "R\u00e9sultats 8.1 et 8.2 (Tableaux 5 et 6) ; M\u00e9thode 4.1.3 et 5.3",
    "Sans objet (aucun risque relatif estim\u00e9)",
    "R\u00e9sultats 4.2, 4.3, 6 \u00e0 8, 10 et 11",
    "Discussion 1",
    "Discussion 3.2 et 3.3",
    "Discussion 2 et Conclusion ; tests multiples : M\u00e9thode 6.4 et Discussion 3.2",
    "Discussion 3.2 et 4.3",
    "M\u00e9thode 7.5"
  ),
  stringsAsFactors = FALSE
)
stopifnot(nrow(strobe) == 32)

ft <- flextable(strobe) |>
  set_header_labels(Item = "Item", Rubrique = "Rubrique",
                    Recommandation = "Recommandation", Ou = "O\u00f9 dans la th\u00e8se") |>
  theme_vanilla() |>
  bold(part = "header") |>
  bold(j = 1) |>
  fontsize(size = 8, part = "all") |>
  padding(padding.top = 1, padding.bottom = 1, part = "body") |>
  width(j = 1, width = 0.45) |>
  width(j = 2, width = 1.25) |>
  width(j = 3, width = 3.3) |>
  width(j = 4, width = 2.2) |>
  valign(j = 2, valign = "top") |>
  add_footer_lines("* Donner l\u2019information s\u00e9par\u00e9ment pour les groupes expos\u00e9s et non expos\u00e9s.") |>
  add_footer_lines("D\u2019apr\u00e8s la checklist STROBE pour les \u00e9tudes transversales (56).")

sect <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69, orient = "portrait"),
  page_margins = page_mar(top = 0.4, bottom = 0.4, left = 0.5, right = 0.5,
                          header = 0.2, footer = 0.2)
)

save_as_docx(ft, path = here("output", "tableaux", "Grille_STROBE_renseignee.docx"),
             pr_section = sect)
cat("Grille STROBE sauvegardee (", nrow(strobe), "lignes).\n")

