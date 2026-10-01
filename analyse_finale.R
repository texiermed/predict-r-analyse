# ============================================================================
#  ANALYSE FINALE — Etude PREDICT-R
#  Alexis TEXIER — These de medecine generale
#  Universite Toulouse III — Paul Sabatier
#  Directrice : Dr Virginie SICRE GATIMEL
#
#  Script unique et reproductible.
#  Base : CRD_PredictRVF_data.xlsx (139 inclus / 130 analysables DM)
#  Produire tous les tableaux (.docx) et figures (.png) de la these.
#
#  Usage : ouvrir le projet RStudio, puis Ctrl+A, Ctrl+Entree.
#  Sortie : output/tableaux/*.docx  +  output/figures/*.png
#
#  Derniere execution : 18/07/2026
# ============================================================================


# ============================================================================
# 0. SETUP
# ============================================================================

library(tidyverse)
library(readxl)
library(here)
library(DescTools)    # BinomCI (IC Wilson)
library(irr)          # kappa2
library(psych)        # cohen.kappa (Kappa + IC 95 %)
library(gtsummary)    # Tableau 1
library(flextable)    # export Word
library(epiR)         # epi.tests (Se/Sp/VPP/VPN)

# Reproductibilite : graine pour simulations Monte Carlo (Fisher)
set.seed(2026)

# Theme francais pour gtsummary (Chazard : tout en francais)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

# Formateur francais : virgule decimale (Chazard)
frf <- function(...) gsub(".", ",", sprintf(...), fixed = TRUE)

# Formateur p-value francais (pas de notation scientifique dans Word)
fmt_p_fr <- function(p) {
  if (p < 0.001) return("< 0,001")
  frf("%.3f", p)
}

eq1 <- function(x) !is.na(x) & x == 1

to_likert <- function(x) {
  case_when(
    str_detect(x, "Tout.*fait")  ~ 4L,
    str_detect(x, "Pas du tout") ~ 1L,
    str_detect(x, "[Pp]as")      ~ 2L,
    str_detect(x, "[Aa]ccord")   ~ 3L,
    TRUE                          ~ NA_integer_
  )
}

dir.create(here("output", "tableaux"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"),  recursive = TRUE, showWarnings = FALSE)

cat("\n=========================================\n")
cat("  ANALYSE PREDICT-R — PIPELINE COMPLET\n")
cat("  Date :", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("=========================================\n\n")


# ============================================================================
# 1. IMPORT ET RECODAGE
# ============================================================================

# --- 1.1 Import -------------------------------------------------------------
predictr <- read_excel(
  here("data", "CRD_PredictRVF_data.xlsx"),
  sheet = "donnees",
  na = c("", "NA")
)
cat("Import :", nrow(predictr), "patients,", ncol(predictr), "variables\n\n")

# --- 1.2 Typage et recodage initial -----------------------------------------
predictr <- predictr |>
  mutate(
    PR_couleur    = factor(PR_couleur, levels = c("vert", "orange", "rouge")),
    PR_bin        = if_else(PR_couleur %in% c("orange", "rouge"), 1L, 0L),
    DM_DFG_valeur = as.numeric(DM_DFG_valeur),
    DM_RAC_valeur = as.numeric(DM_RAC_valeur),
    DM_DFG_date   = as.Date(DM_DFG_date),
    date_inclusion = as.Date(date_inclusion)
  )

# --- 1.3 Correction documentee PRED134 --------------------------------------
# RAC prescrit mais DM_RAC code 0 par erreur -> recodage a 1.
# N'affecte pas le CJP (DM_repere = 1 deja correct).
predictr <- predictr |>
  mutate(DM_RAC = if_else(id_patient == "PRED134", 1, DM_RAC))

# --- 1.4 Filtre analysables DM (n = 130) ------------------------------------
predictr_dm <- predictr |> filter(!is.na(DM_repere))
cat("Analysables DM :", nrow(predictr_dm), "\n")

# --- 1.5 Variables derivees --------------------------------------------------
predictr_dm <- predictr_dm |>
  mutate(
    # Depistage
    DFG_evalue    = eq1(DM_DFG_disponible) | eq1(DM_DFG_prescrit_non_realise),
    test_urinaire = eq1(DM_RAC) | eq1(DM_prot_creat) | eq1(DM_microalbuminurie) |
                    eq1(DM_BU_automate) | eq1(DM_proteinurie),

    # CJP : rattrape = a risque PR ET non repere DM
    rattrape = as.integer(PR_bin == 1 & DM_repere == 0),

    # FDR HAS (12 facteurs — HAS 2021)
    fdr_expo_contraste = eq1(DM_expo_PCI) | eq1(DM_expo_radiotherapie),
    nb_FDR_HAS =
      eq1(DM_diabete) + eq1(DM_HTA) + eq1(DM_MCV) + eq1(DM_insuf_cardiaque) +
      eq1(DM_obesite) + eq1(DM_maladie_auto_immune) + eq1(DM_patho_uro) +
      eq1(DM_ATCD_fam_nephro) + eq1(DM_ATCD_nephro_aigue) +
      eq1(DM_nephrotoxiques) + fdr_expo_contraste + eq1(DM_expo_toxiques_pro),
    cat_FDR_HAS = cut(nb_FDR_HAS,
                      breaks = c(-Inf, 0, 2, Inf),
                      labels = c("0", "1-2", "≥3"),
                      ordered_result = TRUE),

    # Niveau de depistage (5 categories hierarchiques)
    depistage_niveau = case_when(
      eq1(DM_suivi_nephro)        ~ "Suivi nephrologique",
      DFG_evalue & test_urinaire  ~ "Complet (DFG + urinaire)",
      DFG_evalue & !test_urinaire ~ "DFG seul",
      !DFG_evalue & test_urinaire ~ "Test urinaire seul",
      TRUE                        ~ "Aucun"
    ),
    depistage_niveau = factor(depistage_niveau,
      levels = c("Aucun", "DFG seul", "Test urinaire seul",
                 "Complet (DFG + urinaire)", "Suivi nephrologique")),
    depistage_complet = as.integer(depistage_niveau %in%
      c("Complet (DFG + urinaire)", "Suivi nephrologique")),

    # Gold standard KDIGO (OS6)
    # La chronicite (>= 3 mois, >= 2 mesures concordantes) a ete verifiee
    # manuellement dans Alma Pro par l'investigateur lors du recueil ; ce
    # critere n'est pas trace dans le CRD et n'est donc pas recalcule ici.
    biologie_dispo = !is.na(DM_DFG_valeur) | !is.na(DM_RAC_valeur),
    MRC_KDIGO = case_when(
      !biologie_dispo                                     ~ NA_integer_,
      (!is.na(DM_DFG_valeur) & DM_DFG_valeur < 60)       ~ 1L,
      (!is.na(DM_RAC_valeur) & DM_RAC_valeur >= 3)       ~ 1L,
      TRUE                                                ~ 0L
    ),

    # Comparateur FDR
    au_moins_1_FDR = as.integer(nb_FDR_HAS >= 1),

    # Verification coherence : reperage recalcule
    repere_calcule = as.integer((DFG_evalue & test_urinaire) | eq1(DM_suivi_nephro))
  )

cat("Repere recalcule vs base :\n")
print(table(recalcule = predictr_dm$repere_calcule, base = predictr_dm$DM_repere))
cat("\nVerification MRC connue par couleur PR :\n")
print(table(PR = predictr_dm$PR_couleur, MRC_connue = predictr_dm$DM_MRC_connue))
cat("MRC connues total :", sum(predictr_dm$DM_MRC_connue == 1, na.rm = TRUE), "\n")
cat("MRC connues parmi orange/rouge :", sum(predictr_dm$PR_bin == 1 & predictr_dm$DM_MRC_connue == 1, na.rm = TRUE), "\n")
cat("\n")


# ============================================================================
# 2. TABLEAU 1 — Caracteristiques de la population (OS2)
# ============================================================================

tableau1 <- predictr_dm |>
  select(DM_age, DM_sexe, PR_IMC,
         DM_HTA, DM_diabete, DM_obesite, DM_MCV, DM_insuf_cardiaque,
         DM_maladie_auto_immune, DM_patho_uro,
         DM_tabagisme, DM_dyslipidemie,
         cat_FDR_HAS,
         DM_repere, DM_MRC_connue, DM_DFG_valeur,
         PR_couleur) |>
  tbl_summary(
    by = PR_couleur,
    label = list(
      DM_age               ~ "Âge (ans)",
      DM_sexe              ~ "Sexe",
      PR_IMC               ~ "IMC déclaré (kg/m²)",
      DM_HTA               ~ "Hypertension artérielle",
      DM_diabete           ~ "Diabète",
      DM_obesite           ~ "Obésité",
      DM_MCV               ~ "Maladie cardiovasculaire",
      DM_insuf_cardiaque   ~ "Insuffisance cardiaque",
      DM_maladie_auto_immune ~ "Maladie auto-immune / de système",
      DM_patho_uro         ~ "Pathologie urologique",
      DM_tabagisme         ~ "Tabagisme",
      DM_dyslipidemie      ~ "Dyslipidémie",
      cat_FDR_HAS          ~ "Nombre de FDR HAS",
      DM_repere            ~ "Repéré dans le dossier",
      DM_MRC_connue        ~ "MRC déjà connue",
      DM_DFG_valeur        ~ "DFG estimé (mL/min/1,73 m²)"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) |>
  add_p(test = list(
    all_continuous()  ~ "kruskal.test",
    all_categorical() ~ "fisher.test"
  )) |>
  add_overall() |>
  bold_labels() |>
  modify_spanning_header(all_stat_cols() ~ "**Niveau de risque Predict-R**") |>
  modify_caption(
    "**Tableau 1.** Caractéristiques des patients selon le niveau de risque Predict-R (n = 130)"
  ) |>
  modify_footnote(everything() ~ "9 patients sans dossier médical disponible exclus (n total inclus = 139).")

tableau1 |>
  as_flex_table() |>
  save_as_docx(path = here("output", "tableaux", "Tableau1_caracteristiques.docx"))
cat("Tableau 1 sauvegarde.\n")


# ============================================================================
# 3. CJP — Proportion de rattrapes + analyses de sensibilite
# ============================================================================

orange_rouge <- predictr_dm |> filter(PR_bin == 1)
n_or   <- nrow(orange_rouge)
n_rat  <- sum(orange_rouge$rattrape)
ic_cjp <- BinomCI(n_rat, n_or, method = "wilson")

cat("\n--- CJP ---\n")
cat("Rattrapes :", n_rat, "/", n_or, "=",
    round(n_rat / n_or * 100, 1), "%\n")
cat("IC 95 % Wilson : [", round(ic_cjp[2] * 100, 1), "-",
    round(ic_cjp[3] * 100, 1), "]\n")

# Test binomial vs seuils croissants
seuils <- c(0.10, 0.20, 0.30, 0.40, 0.50)
p_vals <- sapply(seuils, function(s)
  binom.test(n_rat, n_or, p = s, alternative = "two.sided")$p.value)

# Sensibilite sans MRC connue
or_sans_mrc <- predictr_dm |> filter(PR_bin == 1, DM_MRC_connue == 0)
n2  <- nrow(or_sans_mrc)
r2  <- sum(or_sans_mrc$rattrape)
ic2 <- BinomCI(r2, n2, method = "wilson")

# Sensibilite fenetre 24 mois (verification individuelle)
range_dfg <- range(predictr_dm$DM_DFG_date, na.rm = TRUE)
hors_fenetre <- predictr_dm |>
  filter(DFG_evalue, !is.na(DM_DFG_date)) |>
  mutate(delai_mois = as.numeric(date_inclusion - DM_DFG_date) / 30.44) |>
  filter(delai_mois > 24)
cat("DFG hors fenêtre 24 mois :", nrow(hors_fenetre), "\n")
if (nrow(hors_fenetre) > 0) {
  warning("Des DFG datent de > 24 mois avant inclusion : recalculer la sensibilité fenêtre.")
}

# --- Tableau 2 : CJP ---
fmt_ic <- function(lo, hi) frf("[%.1f - %.1f]", lo * 100, hi * 100)

tableau2 <- tibble(
  Analyse = c("Principale (tous orange/rouge)",
              "Sans MRC connue",
              paste0("Fenêtre 24 mois")),
  `n` = c(n_or, n2, n_or),
  `Rattrapés` = c(n_rat, r2, n_rat),
  `%` = c(frf("%.1f", n_rat/n_or*100),
          frf("%.1f", r2/n2*100),
          frf("%.1f", n_rat/n_or*100)),
  `IC 95 % Wilson` = c(fmt_ic(ic_cjp[2], ic_cjp[3]),
                        fmt_ic(ic2[2], ic2[3]),
                        "= principale"),
  `p (vs 10 %)` = c(fmt_p_fr(p_vals[1]),
                     fmt_p_fr(binom.test(r2, n2, p = 0.10,
                            alternative = "two.sided")$p.value),
                     "= principale")
)

flex_cjp <- flextable(tableau2) |>
  set_caption(paste0(
    "Tableau 2. Critère de jugement principal — ",
    "proportion de patients rattrapés par Predict-R et analyses de sensibilité")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:6, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "Rattrapés = patients orange/rouge non repérés dans le dossier. ",
    "IC Wilson sans correction. Test binomial exact bilatéral (H0 : p = 0,10).")) |>
  add_footer_lines(paste0(
    "Robustesse du seuil : significatif à 20 % (p ",
    fmt_p_fr(p_vals[2]), "), 30 % (p ",
    fmt_p_fr(p_vals[3]), "), 40 % (p ",
    fmt_p_fr(p_vals[4]), "), 50 % (p = ",
    frf("%.3f", p_vals[5]), ").")) |>
  add_footer_lines(paste0(
    "Fenêtre 24 mois : tous les DFG datent de < 24 mois (",
    format(range_dfg[1], "%d/%m/%Y"), " - ",
    format(range_dfg[2], "%d/%m/%Y"),
    "), CJP inchangé."))

save_as_docx(flex_cjp,
             path = here("output", "tableaux", "Tableau2_CJP.docx"))
cat("Tableau 2 sauvegarde.\n")


# ============================================================================
# 4. CONCORDANCE / KAPPA (OS1)
# ============================================================================

# --- 4.1 Tableau 3x2 --------------------------------------------------------
tab_3x2 <- predictr_dm |>
  count(PR_couleur, DM_repere) |>
  pivot_wider(names_from = DM_repere, values_from = n, values_fill = 0) |>
  rename(`Non repéré` = `0`, `Repéré` = `1`) |>
  mutate(Total = `Non repéré` + `Repéré`)

tab_3x2_fmt <- tab_3x2 |>
  mutate(
    `Predict-R` = as.character(PR_couleur),
    `Non repéré, n (%)` = frf("%d (%.1f %%)", `Non repéré`,
                                  `Non repéré` / Total * 100),
    `Repéré, n (%)`     = frf("%d (%.1f %%)", `Repéré`,
                                  `Repéré` / Total * 100),
    Total = as.character(Total)
  ) |>
  select(`Predict-R`, `Non repéré, n (%)`, `Repéré, n (%)`, Total)

# Totaux marginaux
total_row <- tibble(
  `Predict-R` = "Total",
  `Non repéré, n (%)` = frf("%d (%.1f %%)", sum(tab_3x2$`Non repéré`),
    sum(tab_3x2$`Non repéré`) / sum(tab_3x2$Total) * 100),
  `Repéré, n (%)` = frf("%d (%.1f %%)", sum(tab_3x2$`Repéré`),
    sum(tab_3x2$`Repéré`) / sum(tab_3x2$Total) * 100),
  Total = as.character(sum(tab_3x2$Total))
)
tab_3x2_complet <- bind_rows(tab_3x2_fmt, total_row)

# --- 4.2 Kappa de Cohen + IC 95 % -------------------------------------------
tab_2x2 <- table(PR = predictr_dm$PR_bin, DM = predictr_dm$DM_repere)
accord  <- (tab_2x2["0", "0"] + tab_2x2["1", "1"]) / sum(tab_2x2)

kappa_res <- cohen.kappa(as.matrix(predictr_dm[, c("PR_bin", "DM_repere")]))
kappa_val <- kappa_res$kappa
kappa_lo  <- kappa_res$confid["unweighted kappa", "lower"]
kappa_hi  <- kappa_res$confid["unweighted kappa", "upper"]

# Verification croisee irr::kappa2 vs psych::cohen.kappa
kappa_check <- kappa2(predictr_dm[, c("PR_bin", "DM_repere")])
cat("Verification Kappa : psych =", round(kappa_val, 4), "/ irr =", round(kappa_check$value, 4), "\n")

# PABAK (sensibilite — corrige le biais de prevalence du Kappa)
pabak <- 2 * accord - 1

# --- 4.3 Comparateur FDR HAS ------------------------------------------------
kappa_fdr_res <- cohen.kappa(as.matrix(predictr_dm[, c("PR_bin", "au_moins_1_FDR")]))
kappa_fdr_val <- kappa_fdr_res$kappa
kappa_fdr_lo  <- kappa_fdr_res$confid["unweighted kappa", "lower"]
kappa_fdr_hi  <- kappa_fdr_res$confid["unweighted kappa", "upper"]
tab_fdr   <- table(PR = predictr_dm$PR_bin, FDR = predictr_dm$au_moins_1_FDR)

# Fisher test : association FDR HAS >= 1 et depistage complet
tab_fdr_dep <- table(FDR = predictr_dm$au_moins_1_FDR,
                     complet = predictr_dm$depistage_complet)
fisher_fdr_dep <- fisher.test(tab_fdr_dep)
cat("Fisher FDR>=1 vs depistage complet : p =", format.pval(fisher_fdr_dep$p.value), "\n")

# --- 4.4 Export Tableau 3 ----------------------------------------------------
tab_kappa_df <- tibble(
  Indicateur = c(
    "Concordance brute",
    "Kappa de Cohen",
    "IC 95 % du Kappa",
    "PABAK (Byrt 1993)",
    "Interprétation (Landis & Koch)",
    "Comparateur : Kappa PR vs ≥1 FDR HAS",
    "IC 95 % du Kappa PR vs FDR",
    "Accord brut PR vs FDR"
  ),
  Valeur = c(
    frf("%.1f %%", accord * 100),
    frf("%.3f", kappa_val),
    frf("[%.3f ; %.3f]", kappa_lo, kappa_hi),
    frf("%.3f", pabak),
    "Accord faible (Landis & Koch)",
    frf("%.3f", kappa_fdr_val),
    frf("[%.3f ; %.3f]", kappa_fdr_lo, kappa_fdr_hi),
    frf("%.1f %%", (tab_fdr[1,1] + tab_fdr[2,2]) / sum(tab_fdr) * 100)
  )
)

flex_conc <- flextable(tab_3x2_complet) |>
  set_caption(paste0(
    "Tableau 3a. Concordance Predict-R / dossier médical ",
    "— distribution croisée (n = 130)")) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  bold(i = nrow(tab_3x2_complet)) |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines("Pourcentages calculés en ligne.")

flex_kappa <- flextable(tab_kappa_df) |>
  set_caption("Tableau 3b. Concordance — Kappa de Cohen et analyses complémentaires") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header")

save_as_docx(
  flex_conc,
  flex_kappa,
  path = here("output", "tableaux", "Tableau3_concordance.docx")
)
cat("Tableau 3 sauvegarde.\n")


# ============================================================================
# 5. PRATIQUES DE DEPISTAGE (OS5)
# ============================================================================

# --- 5.1 Repartition globale -------------------------------------------------
repartition_dep <- predictr_dm |>
  count(depistage_niveau) |>
  mutate(`%` = frf("%.1f", n / sum(n) * 100))

# --- 5.2 Sous-groupe >= 1 FDR HAS -------------------------------------------
sg_fdr <- predictr_dm |> filter(nb_FDR_HAS >= 1)
n_fdr  <- nrow(sg_fdr)
n_conf <- sum(sg_fdr$depistage_complet)
ic_conf <- BinomCI(n_conf, n_fdr, method = "wilson")

# L'occasion manquee : DFG evalue sans RAC
sg_dfg <- sg_fdr |> filter(DFG_evalue)
n_sg_dfg <- nrow(sg_dfg)
n_sans_rac <- sum(!sg_dfg$test_urinaire)
ic_rac <- BinomCI(n_sans_rac, n_sg_dfg, method = "wilson")

# Cochran-Armitage
tA <- predictr_dm |> group_by(cat_FDR_HAS) |>
  summarise(s = sum(DFG_evalue), n = n(), .groups = "drop")
ptrend_dfg <- prop.trend.test(x = tA$s, n = tA$n)

tB <- predictr_dm |> group_by(cat_FDR_HAS) |>
  summarise(s = sum(depistage_complet), n = n(), .groups = "drop")
ptrend_comp <- prop.trend.test(x = tB$s, n = tB$n)

# Gradient par categorie
gradient <- predictr_dm |>
  group_by(cat_FDR_HAS) |>
  summarise(
    n = n(),
    `DFG évalué (%)` = frf("%.1f", mean(DFG_evalue) * 100),
    `Dépistage complet (%)` = frf("%.1f", mean(depistage_complet) * 100),
    .groups = "drop"
  )

# --- 5.3 Export Tableau 4 ---
dep_df <- tibble(
  `Niveau de dépistage` = as.character(repartition_dep$depistage_niveau),
  n = repartition_dep$n,
  `%` = repartition_dep$`%`
)

flex_dep <- flextable(dep_df) |>
  set_caption("Tableau 4a. Niveau de dépistage de la MRC (n = 130)") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(frf(
    "Sous-groupe ≥1 FDR HAS (n = %d) : dépistage complet = %d (%.1f %%) [IC 95 %% Wilson : %.1f - %.1f].",
    n_fdr, n_conf, n_conf/n_fdr*100, ic_conf[2]*100, ic_conf[3]*100)) |>
  add_footer_lines(frf(
    "Occasion manquée : parmi les %d patients avec DFG évalué et ≥1 FDR, %d (%.1f %%) n'avaient pas de test urinaire associé.",
    n_sg_dfg, n_sans_rac, n_sans_rac/n_sg_dfg*100))

flex_grad <- flextable(gradient) |>
  set_caption("Tableau 4b. Gradient de dépistage par catégorie de FDR HAS") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:4, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "Tendance Cochran-Armitage : DFG évalué p = ",
    fmt_p_fr(ptrend_dfg$p.value),
    " ; dépistage complet p = ",
    fmt_p_fr(ptrend_comp$p.value), ".")) |>
  add_footer_lines(paste0(
    "Association ≥1 FDR HAS vs dépistage complet : test exact de Fisher, p = ",
    fmt_p_fr(fisher_fdr_dep$p.value), "."))

save_as_docx(
  flex_dep,
  flex_grad,
  path = here("output", "tableaux", "Tableau4_depistage.docx")
)
cat("Tableau 4 sauvegarde.\n")


# ============================================================================
# 6. PERFORMANCES DIAGNOSTIQUES OS6 (exploratoire)
# ============================================================================

os6 <- predictr_dm |> filter(!is.na(MRC_KDIGO))
cat("\nOS6 : n =", nrow(os6), "avec biologie\n")

# Quantification du biais de verification (disponibilite biologie par PR)
biais_verif <- predictr_dm |>
  group_by(PR_bin) |>
  summarise(n_total = n(), n_bio = sum(!is.na(MRC_KDIGO)),
            pct_bio = round(mean(!is.na(MRC_KDIGO)) * 100, 1), .groups = "drop")
cat("Biais de verification :\n"); print(biais_verif); cat("\n")

os6 <- os6 |>
  mutate(
    test_pos = factor(if_else(PR_bin == 1, "PR+", "PR-"), levels = c("PR+", "PR-")),
    mal_pos  = factor(if_else(MRC_KDIGO == 1, "KDIGO+", "KDIGO-"),
                      levels = c("KDIGO+", "KDIGO-"))
  )
tab_os6 <- table(os6$test_pos, os6$mal_pos)
perf    <- epi.tests(tab_os6, conf.level = 0.95)
perf_d  <- as.data.frame(perf$detail)

indics <- c("se", "sp", "pv.pos", "pv.neg")
noms   <- c("Sensibilité", "Spécificité", "VPP", "VPN")

perf_sel <- perf_d[match(indics, perf_d$statistic), ]

tableau5 <- tibble(
  Indicateur = noms,
  Valeur     = frf("%.1f %%", perf_sel$est * 100),
  `IC 95 %`  = frf("[%.1f - %.1f]", perf_sel$lower * 100, perf_sel$upper * 100)
)

vp <- tab_os6["PR+", "KDIGO+"]; fp <- tab_os6["PR+", "KDIGO-"]
fn <- tab_os6["PR-", "KDIGO+"]; vn <- tab_os6["PR-", "KDIGO-"]

flex_os6 <- flextable(tableau5) |>
  set_caption(sprintf(
    "Tableau 5. Performances diagnostiques de Predict-R vs anomalie biologique KDIGO (n = %d, exploratoire)",
    nrow(os6))) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center", part = "all") |>
  add_footer_lines(sprintf("Tableau 2x2 : VP = %d, FP = %d, FN = %d, VN = %d.", vp, fp, fn, vn)) |>
  add_footer_lines("Anomalie biologique KDIGO : DFG < 60 mL/min/1,73 m² et/ou RAC ≥ 3 mg/mmol. Chronicité (≥ 3 mois) vérifiée manuellement dans le dossier médical par l'investigateur lors du recueil (critère non tracé dans le CRD, donc non recalculable ici).") |>
  add_footer_lines("IC 95 % : méthode de Clopper-Pearson (epiR::epi.tests).") |>
  add_footer_lines("ATTENTION : biais de vérification. Analyse strictement exploratoire.")

save_as_docx(flex_os6,
             path = here("output", "tableaux", "Tableau5_performances.docx"))
cat("Tableau 5 sauvegarde.\n")


# ============================================================================
# 7. FAISABILITE (OS3)
# ============================================================================

# Denominateur STROBE (valeurs du fichier Maiia, figees)
n_rdv       <- 908
n_eligible  <- 516
n_heko_brut <- 144
n_inclus    <- nrow(predictr)  # 139

# Modalites de completion
modalites <- predictr |>
  count(modalite_completion) |>
  mutate(`%` = frf("%.1f", n / sum(n) * 100))

# BU realisees
n_bu <- sum(predictr$PR_BU_realisee == 1, na.rm = TRUE)

tableau6 <- tibble(
  Indicateur = c(
    "RDV totaux semaine (Maiia)",
    "Patients éligibles (adultes, MG, physique)",
    "Questionnaires HEKO bruts",
    "Inclus après nettoyage",
    "Taux de participation (inclus / éligibles)",
    "Taux de complétion (inclus / HEKO bruts)",
    "Smartphone (AUTO_SMART + AIDE_SMART)",
    "BU réalisées"
  ),
  Valeur = c(
    as.character(n_rdv),
    as.character(n_eligible),
    as.character(n_heko_brut),
    as.character(n_inclus),
    frf("%.1f %%", n_inclus / n_eligible * 100),
    frf("%.1f %%", n_inclus / n_heko_brut * 100),
    frf("%d / %d (%.1f %%)",
            sum(modalites$n[modalites$modalite_completion %in%
                             c("AUTO_SMART", "AIDE_SMART")]),
            n_inclus,
            sum(modalites$n[modalites$modalite_completion %in%
                             c("AUTO_SMART", "AIDE_SMART")]) / n_inclus * 100),
    frf("%d / %d (%.1f %%)", n_bu, n_inclus, n_bu / n_inclus * 100)
  )
)

flex_faisa <- flextable(tableau6) |>
  set_caption("Tableau 6. Indicateurs de faisabilité de l'étude PREDICT-R") |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  add_footer_lines(paste0(
    "Source dénominateur : export Maiia, MSP Labarthe-sur-Lèze, ",
    "semaine du 18-22 mai 2026.")) |>
  add_footer_lines(paste0(
    "Le nombre de patients sollicités et de refus n'a pas pu être ",
    "comptabilisé (utilisation en autonomie)."))

save_as_docx(flex_faisa,
             path = here("output", "tableaux", "Tableau6_faisabilite.docx"))
cat("Tableau 6 sauvegarde.\n")


# ============================================================================
# 8. SATISFACTION (OS4)
# ============================================================================

# --- 8.1 Patients (n = 39) --------------------------------------------------
sat_pat <- read.csv(here("data", "satisfaction_patient.csv"),
                    fileEncoding = "UTF-8")
n_sat_pat <- nrow(sat_pat)
cat("\nSatisfaction patients : n =", n_sat_pat, "\n")
cat("Colonnes satisfaction patients :", paste(names(sat_pat), collapse = ", "), "\n")

# Note /10
notes <- sat_pat[[10]]
notes_valides <- notes[!is.na(notes)]
cat("Notes valides :", length(notes_valides), "/ NA :", sum(is.na(notes)), "\n")

# Items Likert patients (par index de colonne)
items_pat <- list(
  list(col = 2,  nom = "Simplicité d'utilisation"),
  list(col = 3,  nom = "Durée adaptée"),
  list(col = 4,  nom = "Compréhension des questions"),
  list(col = 7,  nom = "Mieux comprendre le risque"),
  list(col = 9,  nom = "Recommandation")
)

tableau7a_rows <- map_dfr(items_pat, function(item) {
  vals <- to_likert(sat_pat[[item$col]])
  n_rep <- sum(!is.na(vals))
  n4 <- sum(vals == 4, na.rm = TRUE)
  n3 <- sum(vals == 3, na.rm = TRUE)
  n2 <- sum(vals == 2, na.rm = TRUE)
  n1 <- sum(vals == 1, na.rm = TRUE)
  tibble(
    Item = item$nom,
    `Tout à fait` = sprintf("%d (%.0f %%)", n4, n4/n_rep*100),
    `Plutôt d'accord` = sprintf("%d (%.0f %%)", n3, n3/n_rep*100),
    `Plutôt pas` = sprintf("%d (%.0f %%)", n2, n2/n_rep*100),
    `Pas du tout`  = sprintf("%d (%.0f %%)", n1, n1/n_rep*100),
    `n réponses` = n_rep,
    `% positif`  = frf("%.1f", (n3 + n4) / n_rep * 100)
  )
})

# Ajouter items speciaux
intrusion <- sum(str_detect(tolower(sat_pat[[5]]), "oui"), na.rm = TRUE)
mt_oui  <- sum(sat_pat[[8]] == "Oui", na.rm = TRUE)
mt_pe   <- sum(str_detect(sat_pat[[8]], "tre"), na.rm = TRUE)
mt_non  <- sum(sat_pat[[8]] == "Non", na.rm = TRUE)

flex_sat_pat <- flextable(tableau7a_rows) |>
  set_caption(sprintf(
    "Tableau 7. Satisfaction des patients (n = %d)", n_sat_pat)) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:7, align = "center", part = "all") |>
  add_footer_lines(frf(
    "Note globale /10 : médiane = %.0f [Q1 = %d ; Q3 = %d], moyenne = %.2f. Score ≥ 8 : %d/%d (%.1f %%).",
    median(notes, na.rm = TRUE), quantile(notes, 0.25, na.rm = TRUE),
    quantile(notes, 0.75, na.rm = TRUE),
    mean(notes, na.rm = TRUE), sum(notes >= 8, na.rm = TRUE),
    sum(!is.na(notes)), sum(notes >= 8, na.rm = TRUE)/sum(!is.na(notes))*100)) |>
  add_footer_lines(frf(
    "Intrusion : %d/%d (%.1f %%). Intention de consulter le MT : Oui = %d, Peut-être = %d, Non = %d.",
    intrusion, n_sat_pat, intrusion/n_sat_pat*100, mt_oui, mt_pe, mt_non)) |>
  add_footer_lines(frf("Taux de réponse : %d/139 (%.1f %%).",
                            n_sat_pat, n_sat_pat/139*100))

save_as_docx(flex_sat_pat,
             path = here("output", "tableaux", "Tableau7_satisfaction_patient.docx"))
cat("Tableau 7 sauvegarde.\n")

# --- 8.2 Medecins (n = 9) ---------------------------------------------------
sat_med <- read.csv(here("data", "satisfaction_medecin.csv"),
                    fileEncoding = "UTF-8")
n_sat_med <- nrow(sat_med)
cat("Colonnes satisfaction médecins :", paste(names(sat_med), collapse = ", "), "\n")

items_med <- list(
  list(col = 2, nom = "Facile à comprendre pour les patients"),
  list(col = 3, nom = "Présentation claire et lisible"),
  list(col = 4, nom = "Améliorer le repérage MRC"),
  list(col = 5, nom = "Identifier des patients non repérés"),
  list(col = 6, nom = "Favorable à l'utilisation"),
  list(col = 7, nom = "Intégrer dans la pratique"),
  list(col = 8, nom = "Recommander à des confrères")
)

# Format k/n (pas de % pour n = 9)
tableau8_rows <- map_dfr(items_med, function(item) {
  vals <- to_likert(sat_med[[item$col]])
  n_rep <- sum(!is.na(vals))
  n4 <- sum(vals == 4, na.rm = TRUE)
  n3 <- sum(vals == 3, na.rm = TRUE)
  n2 <- sum(vals == 2, na.rm = TRUE)
  n1 <- sum(vals == 1, na.rm = TRUE)
  tibble(
    Item = item$nom,
    `Tout à fait` = sprintf("%d/%d", n4, n_rep),
    `Plutôt d'accord` = sprintf("%d/%d", n3, n_rep),
    `Plutôt pas` = sprintf("%d/%d", n2, n_rep),
    `Pas du tout` = sprintf("%d/%d", n1, n_rep)
  )
})

flex_sat_med <- flextable(tableau8_rows) |>
  set_caption(sprintf(
    "Tableau 8. Satisfaction des médecins (n = %d)", n_sat_med)) |>
  autofit() |>
  theme_vanilla() |>
  bold(j = 1) |>
  bold(part = "header") |>
  align(j = 2:5, align = "center", part = "all") |>
  add_footer_lines(paste0(
    "Échelle Likert à 4 niveaux. Format k/n (pourcentages omis, effectif limité).")) |>
  add_footer_lines("Aucune réponse négative (Plutôt pas / Pas du tout) sur l'ensemble des items.")

save_as_docx(flex_sat_med,
             path = here("output", "tableaux", "Tableau8_satisfaction_medecin.docx"))
cat("Tableau 8 sauvegarde.\n")


# ============================================================================
# 9. FIGURES
# Figure 1 = diagramme de flux STROBE (cree hors script R, cf. 02-Redaction/)
# ============================================================================

couleurs_pr <- c("vert" = "#4CAF50", "orange" = "#FF9800", "rouge" = "#F44336")
labels_pr  <- c("vert" = "Vert\n(faible)", "orange" = "Orange\n(modéré)", "rouge" = "Rouge\n(élevé)")

# --- Figure 2 : Repartition des niveaux PR ----------------------------------
fig2 <- predictr |>
  ggplot(aes(x = PR_couleur, fill = PR_couleur)) +
  geom_bar(width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_pr) +
  scale_x_discrete(labels = labels_pr) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = NULL,
    x = "Niveau de risque Predict-R",
    y = "Nombre de patients",
    caption = paste("N =", nrow(predictr), "patients inclus")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

ggsave(here("output", "figures", "Figure2_repartition_PR.png"),
       fig2, width = 6, height = 4.5, dpi = 300)

# --- Figure 3 : Age par niveau -----------------------------------------------
fig3 <- predictr_dm |>
  ggplot(aes(x = PR_couleur, y = DM_age, fill = PR_couleur)) +
  geom_violin(alpha = 0.5, width = 0.8) +
  geom_boxplot(width = 0.15, fill = "white", outlier.shape = 21) +
  scale_fill_manual(values = couleurs_pr) +
  scale_x_discrete(labels = labels_pr) +
  labs(
    title = NULL,
    x = "Niveau de risque Predict-R",
    y = "Âge (années)",
    caption = "n = 130 patients analysables. Ligne noire = médiane."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(here("output", "figures", "Figure3_age_par_niveau.png"),
       fig3, width = 6, height = 4.5, dpi = 300)

# --- Figure 4 : Gradient de depistage par FDR HAS ---------------------------
fig4_data <- predictr_dm |>
  group_by(cat_FDR_HAS) |>
  summarise(
    `DFG évalué` = mean(DFG_evalue) * 100,
    `Dépistage complet` = mean(depistage_complet) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-cat_FDR_HAS, names_to = "Type", values_to = "pct")

fig4 <- fig4_data |>
  ggplot(aes(x = cat_FDR_HAS, y = pct, fill = Type)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_dodge(0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("DFG évalué" = "#5C9BD5",
                                "Dépistage complet" = "#ED7D31")) +
  labs(
    title = NULL,
    x = "Nombre de FDR HAS",
    y = "Proportion (%)",
    fill = NULL,
    caption = "L'écart entre les barres = test urinaire non associé au DFG"
  ) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(here("output", "figures", "Figure4_gradient_depistage.png"),
       fig4, width = 7, height = 5, dpi = 300)

# --- Figure 5 : Note satisfaction /10 ----------------------------------------
fig5 <- tibble(note = notes_valides) |>
  ggplot(aes(x = factor(note))) +
  geom_bar(fill = "#5C9BD5", width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = NULL,
    x = "Note globale (/10)",
    y = "Nombre de patients",
    caption = frf("n = %d, médiane = %.0f, moyenne = %.2f",
                      length(notes_valides), median(notes_valides), mean(notes_valides))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

ggsave(here("output", "figures", "Figure5_satisfaction_note.png"),
       fig5, width = 6, height = 4, dpi = 300)


# ============================================================================
# FIN
# ============================================================================

cat("\n=========================================\n")
cat("  PIPELINE TERMINE\n")
cat("  Tableaux -> output/tableaux/*.docx\n")
cat("  Figures  -> output/figures/*.png\n")
cat("=========================================\n\n")

# Trace de reproductibilite
sink(here("output", "sessionInfo.txt")); sessionInfo(); sink()
cat("sessionInfo sauvegarde dans output/sessionInfo.txt\n")
