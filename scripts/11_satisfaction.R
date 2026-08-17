# ============================================================================
# 11_satisfaction.R - Satisfaction patient et medecin (OS4)
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Decrire la satisfaction des patients et des medecins quant a
#   l'autoquestionnaire numerique Predict-R.
#
# 2 tableaux produits par questionnaire :
#   - Tableau "Distribution" : n (%) par niveau de Likert (1, 2, 3, 4)
#   - Tableau "Synthese" : mediane [Q1 ; Q3] + % satisfaction positive (3-4)
#
# DONNEES SOURCE :
#   - 08 - Donnees/Satisfaction_Patient_PredictR.xlsx (10 items)
#   - 08 - Donnees/Satisfaction_Medecin_PredictR.xlsx (9 items)
#   Les donnees seront saisies apres recueil terrain.
#
# REFERENCES :
#   - PROTOCOLE_PREDICT-R.md §8.1 (satisfaction)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §11 (echelles Likert)
# ============================================================================


library(here)
source(here("scripts", "01_codebook.R"))
library(gtsummary)
library(flextable)


# ============================================================================
# 1. IMPORT DES DONNEES (fictives pour l'instant)
# ============================================================================

set.seed(2026)
n_sat_patient <- 80
n_sat_medecin <- 12

sat_patient <- tibble(
  id_patient = sprintf("PRED%03d", 1:n_sat_patient),
  facilite_utilisation = sample(1:4, n_sat_patient, replace = TRUE,
                                prob = c(0.05, 0.10, 0.40, 0.45)),
  comprehension_questions = sample(1:4, n_sat_patient, replace = TRUE,
                                   prob = c(0.05, 0.15, 0.40, 0.40)),
  duree_acceptable = sample(1:4, n_sat_patient, replace = TRUE,
                            prob = c(0.05, 0.10, 0.35, 0.50)),
  resultat_clair = sample(1:4, n_sat_patient, replace = TRUE,
                          prob = c(0.05, 0.10, 0.35, 0.50)),
  fiche_conseil_utile = sample(1:4, n_sat_patient, replace = TRUE,
                               prob = c(0.05, 0.15, 0.35, 0.45)),
  outil_pertinent = sample(1:4, n_sat_patient, replace = TRUE,
                           prob = c(0.05, 0.10, 0.40, 0.45)),
  recommandation = sample(1:4, n_sat_patient, replace = TRUE,
                          prob = c(0.05, 0.10, 0.30, 0.55)),
  satisfaction_globale = sample(1:4, n_sat_patient, replace = TRUE,
                                prob = c(0.05, 0.10, 0.30, 0.55)),
  note_numerique = pmin(10, pmax(0, round(rnorm(n_sat_patient, 8, 1.2))))
)

sat_medecin <- tibble(
  id_medecin = sprintf("MED%02d", 1:n_sat_medecin),
  outil_utile = sample(1:4, n_sat_medecin, replace = TRUE,
                       prob = c(0.05, 0.15, 0.50, 0.30)),
  faisable_consultation = sample(1:4, n_sat_medecin, replace = TRUE,
                                 prob = c(0.10, 0.30, 0.40, 0.20)),
  bonne_acceptation_patients = sample(1:4, n_sat_medecin, replace = TRUE,
                                      prob = c(0.05, 0.20, 0.50, 0.25)),
  resultats_pertinents = sample(1:4, n_sat_medecin, replace = TRUE,
                                prob = c(0.05, 0.20, 0.55, 0.20)),
  recommandation_outil = sample(1:4, n_sat_medecin, replace = TRUE,
                                prob = c(0.10, 0.20, 0.45, 0.25)),
  satisfaction_globale = sample(1:4, n_sat_medecin, replace = TRUE,
                                prob = c(0.10, 0.25, 0.45, 0.20)),
  utilisation_future = sample(1:4, n_sat_medecin, replace = TRUE,
                              prob = c(0.10, 0.30, 0.40, 0.20))
)


# ============================================================================
# 2. FONCTION DE PRESENTATION COMPLETE - tableau brut + synthese
# ============================================================================
# Pour chaque item Likert, on calcule :
#   - n (%) pour chaque niveau (1, 2, 3, 4)
#   - mediane [Q1 ; Q3]
#   - n (%) de satisfaction positive (niveaux 3 ou 4)

# Format pourcentage (pour n >= ~30)
construire_synthese_likert <- function(data, vars, labels_items) {
  resultats <- purrr::map_dfr(vars, function(var) {
    v <- data[[var]]
    n_total <- sum(!is.na(v))
    n1 <- sum(v == 1, na.rm = TRUE)
    n2 <- sum(v == 2, na.rm = TRUE)
    n3 <- sum(v == 3, na.rm = TRUE)
    n4 <- sum(v == 4, na.rm = TRUE)
    n_pos <- n3 + n4
    med <- median(v, na.rm = TRUE)
    q1 <- quantile(v, 0.25, na.rm = TRUE)
    q3 <- quantile(v, 0.75, na.rm = TRUE)

    tibble(
      Item = labels_items[[var]],
      `1 - Pas du tout` = sprintf("%d (%.1f %%)", n1, n1 / n_total * 100),
      `2 - Plutot non`  = sprintf("%d (%.1f %%)", n2, n2 / n_total * 100),
      `3 - Plutot oui`  = sprintf("%d (%.1f %%)", n3, n3 / n_total * 100),
      `4 - Tout a fait` = sprintf("%d (%.1f %%)", n4, n4 / n_total * 100),
      `Mediane [Q1 ; Q3]` = sprintf("%.0f [%.0f ; %.0f]", med, q1, q3),
      `Satisfaction positive (3-4)` = sprintf("%d / %d (%.1f %%)",
                                              n_pos, n_total,
                                              n_pos / n_total * 100)
    )
  })
  return(resultats)
}

# Format effectif brut k/n (pour petits effectifs comme n_medecin = 12)
# Recommandation thèse MG française : ne pas afficher de % si n < 20.
construire_synthese_likert_brut <- function(data, vars, labels_items) {
  resultats <- purrr::map_dfr(vars, function(var) {
    v <- data[[var]]
    n_total <- sum(!is.na(v))
    n1 <- sum(v == 1, na.rm = TRUE)
    n2 <- sum(v == 2, na.rm = TRUE)
    n3 <- sum(v == 3, na.rm = TRUE)
    n4 <- sum(v == 4, na.rm = TRUE)
    n_pos <- n3 + n4
    med <- median(v, na.rm = TRUE)
    q1 <- quantile(v, 0.25, na.rm = TRUE)
    q3 <- quantile(v, 0.75, na.rm = TRUE)

    tibble(
      Item = labels_items[[var]],
      `1 - Pas du tout` = sprintf("%d/%d", n1, n_total),
      `2 - Plutot non`  = sprintf("%d/%d", n2, n_total),
      `3 - Plutot oui`  = sprintf("%d/%d", n3, n_total),
      `4 - Tout a fait` = sprintf("%d/%d", n4, n_total),
      `Mediane [Q1 ; Q3]` = sprintf("%.0f [%.0f ; %.0f]", med, q1, q3),
      `Satisfaction positive (3-4)` = sprintf("%d/%d", n_pos, n_total)
    )
  })
  return(resultats)
}


# ============================================================================
# 3. SATISFACTION PATIENT
# ============================================================================

cat("---------------------------------\n")
cat("SATISFACTION PATIENT - Distribution complete\n")
cat("---------------------------------\n\n")

labels_patient <- c(
  facilite_utilisation = "Facilite d'utilisation",
  comprehension_questions = "Comprehension des questions",
  duree_acceptable = "Duree acceptable",
  resultat_clair = "Resultat clair et comprehensible",
  fiche_conseil_utile = "Fiche conseil utile",
  outil_pertinent = "Outil pertinent",
  recommandation = "Recommanderait l'outil",
  satisfaction_globale = "Satisfaction globale"
)

vars_likert_patient <- names(labels_patient)

synthese_patient <- construire_synthese_likert(sat_patient,
                                                vars_likert_patient,
                                                labels_patient)
print(synthese_patient)
cat("\n")

# Note numerique (echelle 0-10) - traitement separe
note_med <- median(sat_patient$note_numerique, na.rm = TRUE)
note_q1 <- quantile(sat_patient$note_numerique, 0.25, na.rm = TRUE)
note_q3 <- quantile(sat_patient$note_numerique, 0.75, na.rm = TRUE)
note_min <- min(sat_patient$note_numerique, na.rm = TRUE)
note_max <- max(sat_patient$note_numerique, na.rm = TRUE)

cat(sprintf("Note numerique globale (0-10) : mediane = %.0f [Q1-Q3 : %.0f-%.0f] (min %.0f - max %.0f)\n\n",
            note_med, note_q1, note_q3, note_min, note_max))

# Export CSV
write_csv(synthese_patient,
          here("output", "tableaux", "tableau4a_satisfaction_patient.csv"))

# Export Word
flex_patient <- flextable(synthese_patient) |>
  set_caption(paste0("Tableau 4a. Satisfaction des patients (n = ",
                     n_sat_patient, ") - Distribution complete + synthese")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:7, align = "center", part = "all") |>
  add_footer_lines(
    paste0("Note numerique globale (0-10) : mediane = ",
           note_med, " [Q1-Q3 : ", note_q1, "-", note_q3,
           "] (min ", note_min, " - max ", note_max, ")")
  ) |>
  add_footer_lines("Satisfaction positive = pourcentage de reponses au niveau 3 ou 4 (Plutot oui + Tout a fait).")

save_as_docx(flex_patient,
             path = here("output", "tableaux", "tableau4a_satisfaction_patient.docx"))


# ============================================================================
# 4. SATISFACTION MEDECIN
# ============================================================================

cat("---------------------------------\n")
cat("SATISFACTION MEDECIN - Distribution complete\n")
cat("---------------------------------\n\n")

labels_medecin <- c(
  outil_utile = "Outil utile en pratique",
  faisable_consultation = "Faisable en consultation",
  bonne_acceptation_patients = "Bonne acceptation par les patients",
  resultats_pertinents = "Resultats pertinents cliniquement",
  recommandation_outil = "Recommanderait l'outil",
  satisfaction_globale = "Satisfaction globale",
  utilisation_future = "Utilisation future envisagee"
)

vars_likert_medecin <- names(labels_medecin)

synthese_medecin <- construire_synthese_likert_brut(sat_medecin,
                                                     vars_likert_medecin,
                                                     labels_medecin)
print(synthese_medecin)
cat("\n")

# Export CSV
write_csv(synthese_medecin,
          here("output", "tableaux", "tableau4b_satisfaction_medecin.csv"))

# Export Word
flex_medecin <- flextable(synthese_medecin) |>
  set_caption(paste0("Tableau 4b. Satisfaction des medecins (n = ",
                     n_sat_medecin, ") - Distribution complete + synthese")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:7, align = "center", part = "all") |>
  add_footer_lines("Echelle de Likert : 1 = pas du tout d'accord, 4 = tout a fait d'accord.") |>
  add_footer_lines("Format k/n : k medecins ayant repondu ce niveau sur n repondants au total.") |>
  add_footer_lines(paste0("ATTENTION : effectif tres limite (n=", n_sat_medecin, " medecins de la MSP). Resultats strictement descriptifs, les pourcentages sont volontairement omis pour eviter une interpretation trompeuse."))

save_as_docx(flex_medecin,
             path = here("output", "tableaux", "tableau4b_satisfaction_medecin.docx"))


# ============================================================================
# 5. INTERPRETATION
# ============================================================================

cat("Lecture des resultats :\n")
cat(" - Echelle Likert : 1 = pas du tout, 4 = tout a fait d'accord.\n")
cat(" - Distribution n (%) par niveau pour visualiser la repartition reelle.\n")
cat(" - Mediane >= 3 : satisfaction globalement positive (centralite).\n")
cat(" - Satisfaction positive (3-4) : indicateur synthetique pour la these.\n\n")

cat("Tableaux sauvegardes :\n")
cat("  - tableau4a_satisfaction_patient (csv + docx)\n")
cat("  - tableau4b_satisfaction_medecin (csv + docx)\n\n")

cat("--- 11_satisfaction.R termine ---\n\n")
cat("ATTENTION : donnees fictives utilisees. Remplacer par les vraies\n")
cat("            donnees apres saisie des questionnaires papier.\n\n")
