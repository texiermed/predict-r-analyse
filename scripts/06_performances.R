# ============================================================================
# 06_performances.R - Objectif secondaire 6 (OS6, exploratoire)
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Estimer les performances diagnostiques de Predict-R par rapport au
#   gold standard KDIGO (DFG < 60 et/ou RAC >= 3 mg/mmol).
#
# !!! IMPORTANT - LIMITATION MAJEURE !!!
#   Biais de verification : le gold standard n'est disponible QUE chez les
#   patients deja depistes (ayant DFG et/ou RAC dans le DM). Les patients
#   non depistes n'ont pas de gold standard - donc les Se/Sp calculees ici
#   ne sont PAS extrapolables a la population generale.
#   Resultats EXPLORATOIRES UNIQUEMENT.
#
# REFERENCES METHODOLOGIQUES :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §9 (performances diagnostiques)
#   - PROTOCOLE_PREDICT-R.md §6 (OS6 exploratoire)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Packages ---------------------------------------------------------------
library(epiR)
library(flextable)


# ============================================================================
# 1. SOUS-GROUPE - patients avec biologie disponible
# ============================================================================

avec_bio <- predict_r |> filter(!is.na(MRC_KDIGO))

n_avec_bio <- nrow(avec_bio)

cat("---------------------------------\n")
cat("PERFORMANCES DIAGNOSTIQUES - OS6 exploratoire\n")
cat("---------------------------------\n")
cat("Sous-groupe avec biologie disponible : n =", n_avec_bio, "\n\n")

if (n_avec_bio < 10) {
  cat("Effectif insuffisant (<10) pour estimer les performances diagnostiques.\n")
  cat("--- 06_performances.R termine ---\n\n")
} else {

# ============================================================================
# 2. TABLEAU 2 x 2 - reordonner pour epi.tests
# ============================================================================
# Format attendu par epi.tests :
#   ligne 1 = Test +, ligne 2 = Test -
#   colonne 1 = Malade +, colonne 2 = Malade -

# Etape 1 : creer le tableau brut (R met 0 en premier par defaut)
tab_brut <- table(
  test = avec_bio$PR_bin,
  maladie = avec_bio$MRC_KDIGO
)

# Etape 2 : reordonner pour mettre + en premier
# c(2, 1) = "ligne/colonne 2 puis ligne/colonne 1" -> inverse l'ordre
tab_perf <- tab_brut[c(2, 1), c(2, 1)]

# Renommer les dimensions
dimnames(tab_perf) <- list(
  PR = c("PR+", "PR-"),
  KDIGO = c("MRC+", "MRC-")
)

cat("Tableau 2 x 2 (PR vs gold standard KDIGO) :\n")
print(tab_perf)
cat("\n")


# ============================================================================
# 3. PERFORMANCES via epi.tests()
# ============================================================================
# epi.tests() retourne : Se, Sp, VPP, VPN avec IC 95 %
# Depuis epiR 2.x, $detail est un data frame avec une colonne `statistic`
# (et non plus des rownames). On filtre les 4 indicateurs qu'on veut.

perf <- epi.tests(tab_perf, conf.level = 0.95)

# $detail contient toutes les statistiques (prevalence, Se, Sp, etc.)
perf_brut <- as.data.frame(perf$detail)

# Filtrer les 4 indicateurs cles (se, sp, pv.pos, pv.neg)
perf_4 <- perf_brut[perf_brut$statistic %in% c("se", "sp", "pv.pos", "pv.neg"), ]

# Reordonner et renommer en francais
ordre_indic <- c("se", "sp", "pv.pos", "pv.neg")
nom_indic <- c("Sensibilite", "Specificite", "VPP", "VPN")

perf_4 <- perf_4[match(ordre_indic, perf_4$statistic), ]
perf_4$indicateur <- nom_indic

cat("Performances diagnostiques (avec IC 95 %) :\n")
print(perf_4)
cat("\n")


# ============================================================================
# 4. SAUVEGARDE - tableau performances formate
# ============================================================================

# Construire le tableau publication-ready
perf_df <- tibble(
  Indicateur = nom_indic,
  Valeur = sprintf("%.1f %%", perf_4$est * 100),
  `IC 95 %` = sprintf("[%.1f %% - %.1f %%]",
                      perf_4$lower * 100, perf_4$upper * 100)
)

cat("Tableau performances :\n")
print(perf_df)
cat("\n")

write_csv(perf_df,
          here("output", "tableaux", "tableau7_performances.csv"))

# Extraire les VP / FP / FN / VN du tableau 2x2
vp <- tab_perf["PR+", "MRC+"]
fp <- tab_perf["PR+", "MRC-"]
fn <- tab_perf["PR-", "MRC+"]
vn <- tab_perf["PR-", "MRC-"]

footer_2x2 <- sprintf("Tableau 2x2 (n = %d) : VP = %d ; FP = %d ; FN = %d ; VN = %d.",
                     n_avec_bio, vp, fp, fn, vn)

# Tableau Word formate
flex_perf <- flextable(perf_df) |>
  set_caption(paste0("Tableau 7. Performances diagnostiques de Predict-R vs gold standard KDIGO (n = ",
                     n_avec_bio, ", analyse exploratoire)")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(footer_2x2) |>
  add_footer_lines(
    "Gold standard KDIGO : DFG < 60 ml/min/1,73 m² et/ou RAC >= 3 mg/mmol."
  ) |>
  add_footer_lines(
    "IC 95 % calcules par la methode de Clopper-Pearson (epiR::epi.tests)."
  ) |>
  add_footer_lines(
    "Biais de verification : le gold standard n'est disponible que chez les patients deja depistes (presence d'une biologie). Les performances ne sont pas extrapolables a la population generale - analyse strictement exploratoire."
  )

save_as_docx(flex_perf,
             path = here("output", "tableaux", "tableau7_performances.docx"))

cat("Tableaux sauvegardes :\n")
cat("  - tableau7_performances.csv (brut)\n")
cat("  - tableau7_performances.docx (Word formate)\n\n")


# ============================================================================
# 5. RAPPEL : LIMITES
# ============================================================================
cat("\n----- LIMITES IMPORTANTES -----\n")
cat("1. Biais de verification : le gold standard KDIGO n'est disponible\n")
cat("   que chez les patients deja depistes. La Se/Sp ne peut pas etre\n")
cat("   extrapolee a la population generale.\n")
cat("2. Effectif petit dans le sous-groupe - IC larges.\n")
cat("3. Resultats EXPLORATOIRES UNIQUEMENT.\n\n")

cat("--- 06_performances.R termine ---\n\n")
}  # fin du if(n_avec_bio >= 10)
