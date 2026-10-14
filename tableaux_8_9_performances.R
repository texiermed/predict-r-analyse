# ============================================================================
#  Tableaux 8 et 9 du manuscrit -- Performances diagnostiques (exploratoire)
#  Tableau 8 : contingence 2x2 (M+/M-, T+/T-)
#  Tableau 9 : Se/Sp/VPP/VPN + IC 95 % de Clopper-Pearson
#  Sortie : output/tableaux/Tableaux8_9_performances.docx
# ============================================================================

library(tidyverse)
library(readxl)
library(here)
library(epiR)
library(flextable)
library(officer)

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)

set.seed(2026)

frf <- function(...) gsub(".", ",", sprintf(...), fixed = TRUE)
eq1 <- function(x) !is.na(x) & x == 1

# --- Chargement ---
predictr <- read_excel(here("data", "CRD_PredictRVF_data.xlsx"),
                       sheet = "donnees", na = c("", "NA"))
predictr <- predictr |>
  mutate(
    PR_couleur = factor(PR_couleur, levels = c("vert", "orange", "rouge")),
    PR_bin = if_else(PR_couleur %in% c("orange", "rouge"), 1L, 0L),
    DM_DFG_valeur = as.numeric(DM_DFG_valeur),
    DM_RAC_valeur = as.numeric(DM_RAC_valeur)
  )

predictr_dm <- predictr |> filter(!is.na(DM_repere))

predictr_dm <- predictr_dm |>
  mutate(
    biologie_dispo = !is.na(DM_DFG_valeur) | !is.na(DM_RAC_valeur),
    MRC_KDIGO = case_when(
      !biologie_dispo ~ NA_integer_,
      (!is.na(DM_DFG_valeur) & DM_DFG_valeur < 60) ~ 1L,
      (!is.na(DM_RAC_valeur) & DM_RAC_valeur >= 3)  ~ 1L,
      TRUE ~ 0L
    )
  )

# --- Sous-groupe avec biologie ---
os6 <- predictr_dm |> filter(!is.na(MRC_KDIGO))
cat("n avec biologie :", nrow(os6), "\n")
# --- Resultats 10 : prevalence et completude du standard de reference ---
cat("KDIGO+ :", frf("%d/%d (%.1f %%)", sum(os6$MRC_KDIGO == 1), nrow(os6),
                    mean(os6$MRC_KDIGO == 1) * 100), "\n")
# RAC compte seulement si DM_RAC = 1 (PRED020 : RAC_valeur = 0,00 saisi avec DM_RAC = 0)
n_ref_complet <- sum(!is.na(os6$DM_DFG_valeur) & eq1(os6$DM_RAC) & !is.na(os6$DM_RAC_valeur))
cat("Standard de reference complet (DFG + RAC) :", n_ref_complet, "/", nrow(os6), "\n")
cat("  (toute valeur de RAC saisie :",
    sum(!is.na(os6$DM_DFG_valeur) & !is.na(os6$DM_RAC_valeur)), ")\n")
cat("RAC sans DFG :", sum(is.na(os6$DM_DFG_valeur) & !is.na(os6$DM_RAC_valeur)), "\n")
# Chronicite (non tracee dans le CRD) : les M+ doivent etre des MRC connues
# (cahier + PRED001, PRED071, PRED129 : albuminurie persistante verifiee dans le dossier)
mrc_connue_8 <- eq1(os6$DM_MRC_connue) | os6$id_patient %in% c("PRED001", "PRED071", "PRED129")
print(table(MRC_KDIGO = os6$MRC_KDIGO, MRC_connue = mrc_connue_8))
cat("M+ :", sort(os6$id_patient[os6$MRC_KDIGO == 1]), "\n")

os6 <- os6 |>
  mutate(
    test_pos = factor(if_else(PR_bin == 1, "T+", "T-"), levels = c("T+", "T-")),
    mal_pos  = factor(if_else(MRC_KDIGO == 1, "M+", "M-"), levels = c("M+", "M-"))
  )

tab_2x2 <- table(os6$test_pos, os6$mal_pos)
print(tab_2x2)

perf <- epi.tests(tab_2x2, conf.level = 0.95)
perf_d <- as.data.frame(perf$detail)

vp <- tab_2x2["T+", "M+"]
fp <- tab_2x2["T+", "M-"]
fn <- tab_2x2["T-", "M+"]
vn <- tab_2x2["T-", "M-"]

cat(sprintf("VP=%d FP=%d FN=%d VN=%d\n", vp, fp, fn, vn))

# --- Tableau 2x2 pour flextable ---
tab_croise <- tibble(
  ` ` = c("T+ (orange/rouge)", "T- (vert)", "Total"),
  `M+ (MRC KDIGO)` = c(as.character(vp), as.character(fn), as.character(vp + fn)),
  `M- (pas de MRC détectée)` = c(as.character(fp), as.character(vn), as.character(fp + vn)),
  Total = c(as.character(vp + fp), as.character(fn + vn), as.character(nrow(os6)))
)

flex_croise <- flextable(tab_croise) |>
  set_caption(paste0("Tableau 8. Tableau de contingence Predict-R ", "\u00d7",
                     " standard de r\u00e9f\u00e9rence KDIGO (n = ", nrow(os6), ").")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = 3, part = "body") |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "M+ = MRC selon les crit", "\u00e8", "res KDIGO (DFG < 60 mL/min/1,73 m", "\u00b2",
    " et/ou RAC ", "\u2265", " 3 mg/mmol), chronicit", "\u00e9",
    " confirm", "\u00e9", "e par au moins 2 mesures espac",
    "\u00e9", "es de plus de 3 mois.")) |>
  add_footer_lines(paste0(
    "T+ = Predict-R orange ou rouge ; T- = Predict-R vert."))

# --- Tableau performances ---
indics <- c("se", "sp", "pv.pos", "pv.neg")
noms <- c("Sensibilit\u00e9", "Sp\u00e9cificit\u00e9", "VPP", "VPN")
perf_sel <- perf_d[match(indics, perf_d$statistic), ]
print(perf_sel[, c("statistic", "est", "lower", "upper")], digits = 4)

tab_perf <- tibble(
  Indicateur = noms,
  Valeur = frf("%.1f %%", perf_sel$est * 100),
  `IC 95 %` = paste0("[", frf("%.1f", perf_sel$lower * 100), " ; ",
                     frf("%.1f", perf_sel$upper * 100), "]")
)

flex_perf <- flextable(tab_perf) |>
  set_caption(paste0("Tableau 9. Performances diagnostiques de Predict-R (analyse exploratoire, n = ",
                     nrow(os6), ").")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(paste0("IC 95 % : m", "\u00e9", "thode de Clopper-Pearson"))

# --- Export ---
save_as_docx(
  flex_croise,
  flex_perf,
  path = here("output", "tableaux", "Tableaux8_9_performances.docx")
)
cat("Tableaux 8 et 9 sauvegard\u00e9s.\n")
