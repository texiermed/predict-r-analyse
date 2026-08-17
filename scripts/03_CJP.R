# ============================================================================
# 03_CJP.R - Critere de jugement principal
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Calculer la proportion de patients "rattrapes" par Predict-R,
#   avec son IC 95 % (Wilson) et un test binomial unilateral vs 10 %.
#
# DEFINITION :
#   - Rattrape = orange/rouge PR ET non repere dans le DM
#   - CJP = nb_rattrapes / nb_orange_rouge
#
# HYPOTHESES :
#   - H0 : p <= 0.10 (apport clinique marginal)
#   - H1 : p >  0.10 (apport cliniquement pertinent)
#   - Test unilateral (alternative = "greater")
#
# REFERENCES METHODOLOGIQUES :
#   - GUIDE_STATISTIQUE_PREDICT-R.md §2 (test binomial CJP)
#   - GUIDE_STATISTIQUE_PREDICT-R.md §1 (NSN)
#   - PROTOCOLE_PREDICT-R.md §6 (critere de jugement principal)
# ============================================================================


# --- Charger le codebook ----------------------------------------------------
library(here)
source(here("scripts", "01_codebook.R"))

# --- Package pour export Word -----------------------------------------------
library(flextable)


# ============================================================================
# 1. PREPARATION - extraction du sous-groupe orange/rouge
# ============================================================================

# Sous-groupe des patients orange ou rouge
or_rouge <- predict_r |> filter(PR_bin == 1)

n_orange_rouge <- nrow(or_rouge)
k_rattrapes <- sum(or_rouge$rattrape)

cat("---------------------------------\n")
cat("CRITERE DE JUGEMENT PRINCIPAL\n")
cat("---------------------------------\n")
cat("  n orange/rouge :", n_orange_rouge, "\n")
cat("  k rattrapes    :", k_rattrapes, "\n")
cat("  Proportion brute :",
    round(k_rattrapes / n_orange_rouge * 100, 1), "%\n\n")


# ============================================================================
# 2. IC 95 % de Wilson via prop.test()
# ============================================================================
# `prop.test(x, n, correct = FALSE)` donne l'IC 95 % de Wilson de la
# proportion x/n. La correction de Yates est desactivee pour avoir un
# vrai IC Wilson (guide stat §2).

ic_wilson <- prop.test(k_rattrapes, n_orange_rouge, correct = FALSE)

cat("IC 95 % de Wilson :\n")
cat("  Proportion =", round(ic_wilson$estimate * 100, 1), "%\n")
cat("  IC 95 %    = [",
    round(ic_wilson$conf.int[1] * 100, 1), "% - ",
    round(ic_wilson$conf.int[2] * 100, 1), "%]\n\n", sep = "")


# ============================================================================
# 3. TEST BINOMIAL UNILATERAL vs 10 %
# ============================================================================
# H0 : p <= 0.10
# H1 : p >  0.10
# Test exact (pas d'approximation) - adapte aux petits effectifs.

SEUIL_CLINIQUE <- 0.10

test_bin <- binom.test(k_rattrapes,
                       n_orange_rouge,
                       p = SEUIL_CLINIQUE,
                       alternative = "greater")

cat("Test binomial exact unilateral :\n")
cat("  H0 : p <=", SEUIL_CLINIQUE, "\n")
cat("  H1 : p > ", SEUIL_CLINIQUE, "\n")
cat("  p-value =", format(test_bin$p.value, digits = 4), "\n\n")

if (test_bin$p.value < 0.05) {
  cat("CONCLUSION : la proportion de rattrapes est SIGNIFICATIVEMENT\n")
  cat("             superieure au seuil clinique de ",
      SEUIL_CLINIQUE * 100, " %.\n\n", sep = "")
} else {
  cat("CONCLUSION : la proportion de rattrapes n'est PAS significativement\n")
  cat("             superieure au seuil clinique de ",
      SEUIL_CLINIQUE * 100, " %.\n\n", sep = "")
}


# ============================================================================
# 4. SAUVEGARDE - tableau CJP format publication
# ============================================================================
# Format horizontal compact : une ligne par population analysee,
# colonnes pour les indicateurs cles. Plus lisible pour la these.

formate_pct <- function(p) sprintf("%.1f", p * 100)
formate_ic <- function(b1, b2) sprintf("[%.1f - %.1f]", b1 * 100, b2 * 100)
formate_pval <- function(p) ifelse(p < 0.001, "< 0,001", sprintf("%.3f", p))

# Calculer aussi l'analyse de sensibilite hors MRC connue (deja fait en section 5)
or_rouge_sans_mrc_calc <- predict_r |> filter(PR_bin == 1, DM_MRC_connue == 0)
n_or_sm <- nrow(or_rouge_sans_mrc_calc)
k_sm <- sum(or_rouge_sans_mrc_calc$rattrape)
ic_sm <- prop.test(k_sm, n_or_sm, correct = FALSE)
test_sm <- binom.test(k_sm, n_or_sm, p = SEUIL_CLINIQUE, alternative = "greater")

# Tableau format horizontal : 2 lignes (analyse principale + sensibilite)
tableau_cjp <- tibble(
  Population = c("Analyse principale (tous les O/R)",
                 "Sensibilite : hors MRC connue"),
  `n O/R` = c(n_orange_rouge, n_or_sm),
  `Rattrapes n` = c(k_rattrapes, k_sm),
  `Proportion %` = c(formate_pct(ic_wilson$estimate),
                     formate_pct(ic_sm$estimate)),
  `IC 95 %` = c(formate_ic(ic_wilson$conf.int[1], ic_wilson$conf.int[2]),
                formate_ic(ic_sm$conf.int[1], ic_sm$conf.int[2])),
  `p (vs 10 %)` = c(formate_pval(test_bin$p.value),
                    formate_pval(test_sm$p.value))
)

# Affichage console
cat("Tableau CJP final :\n")
print(tableau_cjp)
cat("\n")

# Sauvegarde CSV (brut)
write_csv(tableau_cjp,
          here("output", "tableaux", "tableau2_CJP.csv"))

# Sauvegarde Word formate (publication-ready)
flex_cjp <- flextable(tableau_cjp) |>
  set_caption("Tableau 2. Critere de jugement principal - Proportion de patients rattrapes par Predict-R") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:6, align = "center", part = "all") |>
  add_footer_lines(
    "O/R : patients classes orange ou rouge par Predict-R."
  ) |>
  add_footer_lines(
    "Rattrapes : patients orange/rouge non reperes dans le dossier medical."
  ) |>
  add_footer_lines(
    "IC 95 % calcule par la methode de Wilson (sans correction de continuite)."
  ) |>
  add_footer_lines(
    "Test binomial exact unilateral, hypothese alternative : p > 10 %."
  )

save_as_docx(flex_cjp,
             path = here("output", "tableaux", "tableau2_CJP.docx"))

cat("Tableau CJP sauvegarde :\n")
cat("  - CSV : output/tableaux/tableau2_CJP.csv\n")
cat("  - DOCX : output/tableaux/tableau2_CJP.docx\n\n")


# ============================================================================
# 5. ANALYSE DE SENSIBILITE - excluant les MRC connues
# ============================================================================
# Analyse pre-specifiee dans le guide statistique §7 (sous-groupe MRC connue)

cat("---------------------------------\n")
cat("ANALYSE DE SENSIBILITE (hors MRC connue)\n")
cat("---------------------------------\n")

or_rouge_sans_mrc <- predict_r |>
  filter(PR_bin == 1, DM_MRC_connue == 0)

n_sans_mrc <- nrow(or_rouge_sans_mrc)
k_sans_mrc <- sum(or_rouge_sans_mrc$rattrape)

if (n_sans_mrc > 0) {
  ic_sans <- prop.test(k_sans_mrc, n_sans_mrc, correct = FALSE)
  test_sans <- binom.test(k_sans_mrc, n_sans_mrc,
                          p = SEUIL_CLINIQUE, alternative = "greater")

  cat("  n orange/rouge hors MRC :", n_sans_mrc, "\n")
  cat("  k rattrapes             :", k_sans_mrc, "\n")
  cat("  proportion              :", round(ic_sans$estimate * 100, 1), " %\n",
      sep = "")
  cat("  IC 95 %                 : [",
      round(ic_sans$conf.int[1] * 100, 1), "% - ",
      round(ic_sans$conf.int[2] * 100, 1), "%]\n", sep = "")
  cat("  p-value (vs 10 %)       :", format(test_sans$p.value, digits = 4),
      "\n\n")
} else {
  cat("  Aucun patient orange/rouge hors MRC connue.\n\n")
}


cat("--- 03_CJP.R termine ---\n\n")
