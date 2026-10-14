# ============================================================================
#  Analyses a posteriori — PREDICT-R
#  Résultats 8.3 : items ayant conduit au classement à risque des rattrapés
#  Résultats 8.4 : proportion de rattrapés par sexe
#
#  Sortie : console uniquement (pas de tableau). Les chiffres sont imprimés
#           dans l'ordre et avec la formulation du manuscrit, une phrase
#           par ligne, pour une comparaison ligne à ligne.
# ============================================================================

library(tidyverse)
library(readxl)
library(here)
library(DescTools)    # BinomCI (IC Wilson)

# --- Formateurs français ---
frf <- function(...) gsub(".", ",", sprintf(...), fixed = TRUE)
fmt_p_fr <- function(p) {
  if (p < 0.001) return("< 0,001")
  frf("%.3f", p)
}
eq1 <- function(x) !is.na(x) & x == 1

# Seuil du niveau orange (score Predict-R >= 4 = classé à risque)
SEUIL_ORANGE <- 4

# --- Chargement données ---
predictr <- read_excel(here("data", "CRD_PredictRVF_data.xlsx"),
                       sheet = "donnees", na = c("", "NA"))

predictr <- predictr |>
  mutate(
    PR_couleur = factor(PR_couleur, levels = c("vert", "orange", "rouge")),
    PR_bin     = if_else(PR_couleur %in% c("orange", "rouge"), 1L, 0L)
  )

# ============================================================================
# RECALCUL DU SCORE PREDICT-R ITEM PAR ITEM (barème de l'Annexe 3)
# ============================================================================
# Dans la base, l'item urologique s'appelle PR_maladie_uro
# (PR_patho_uro_recidivante dans codebook.md).

predictr <- predictr |>
  mutate(
    pts_prematurite    = 1L * eq1(PR_prematurite),
    pts_diabete        = case_when(
      eq1(PR_diabete) & PR_diabete_anciennete == ">=5_ans" ~ 4L,
      eq1(PR_diabete) & PR_diabete_anciennete == "<5_ans"  ~ 2L,
      TRUE ~ 0L
    ),
    pts_HTA            = 2L * eq1(PR_HTA),
    pts_tabagisme      = 1L * eq1(PR_tabagisme),
    pts_dyslipidemie   = 1L * eq1(PR_dyslipidemie),
    pts_MCV            = 1L * eq1(PR_MCV),
    pts_insuf_card     = 1L * eq1(PR_insuf_cardiaque),
    pts_obesite        = 1L * (!is.na(PR_IMC) & PR_IMC > 30),
    pts_uro            = 3L * eq1(PR_maladie_uro),
    pts_AINS           = 1L * eq1(PR_AINS),
    pts_PCI            = 1L * eq1(PR_expo_PCI),
    pts_radiotherapie  = 1L * eq1(PR_expo_radiotherapie),
    pts_BU_hematurie   = 6L * eq1(PR_BU_hematurie),
    pts_BU_proteinurie = 6L * eq1(PR_BU_proteinurie),
    score_recalcule =
      pts_prematurite + pts_diabete + pts_HTA + pts_tabagisme +
      pts_dyslipidemie + pts_MCV + pts_insuf_card + pts_obesite + pts_uro +
      pts_AINS + pts_PCI + pts_radiotherapie +
      pts_BU_hematurie + pts_BU_proteinurie,
    score_identique = !is.na(PR_score) & score_recalcule == PR_score
  )

# Comparaison avec le score HEKO (PR_score) sur tous les inclus, pour
# information. Un écart connu : PRED128, dont le poids et la taille sont
# absents de l'export HEKO (PR_IMC manquant) ; HEKO a compté 8 points, le
# recalcul en trouve 7 (point de l'IMC probablement). Patient rouge et repéré,
# donc hors de l'analyse 8.3, et rouge avec 7 comme avec 8 points.
ecart_score <- predictr |> filter(!score_identique)

cat("Score recalculé identique à PR_score pour", sum(predictr$score_identique),
    "des", nrow(predictr), "patients inclus.\n")
if (nrow(ecart_score) > 0) {
  cat(paste0("  Écart : ", ecart_score$id_patient,
             " (PR_score = ", ecart_score$PR_score,
             ", recalculé = ", ecart_score$score_recalcule,
             if_else(is.na(ecart_score$PR_IMC), ", PR_IMC manquant", ""),
             ")\n"), sep = "")
}

# --- Patients analysables et rattrapés ---
predictr_dm <- predictr |> filter(!is.na(DM_repere))

predictr_dm <- predictr_dm |>
  mutate(rattrape = as.integer(PR_bin == 1 & DM_repere == 0))

orange_rouge <- predictr_dm |> filter(PR_bin == 1)
rattrapes    <- orange_rouge |> filter(rattrape == 1)

# Contrôle bloquant : l'analyse 8.3 retire des points de PR_score, le
# recalcul doit donc être identique à PR_score chez tous les rattrapés.
ecart_rattrapes <- rattrapes |> filter(!score_identique)

if (nrow(ecart_rattrapes) > 0) {
  stop("Score recalculé différent de PR_score chez ", nrow(ecart_rattrapes),
       " rattrapé(s) : ", paste(ecart_rattrapes$id_patient, collapse = ", "),
       ". Vérifier le barème (Annexe 3) et les colonnes PR_ avant ",
       "d'interpréter le Résultats 8.3.", call. = FALSE)
}
cat("Contrôle : score recalculé identique à PR_score pour les",
    nrow(rattrapes), "rattrapés.\n\n")

cat("Patients analysables :", nrow(predictr_dm), "\n")
cat("Orange/rouge :", nrow(orange_rouge), "\n")
cat("Rattrapes :", nrow(rattrapes), "\n\n")


# ============================================================================
# RÉSULTATS 8.3 — Items ayant conduit au classement à risque des rattrapés
# ============================================================================
# Un item (ou groupe d'items) est « nécessaire » si le score, privé de ses
# points, passe sous le seuil de 4 : sans lui, le patient aurait été classé
# vert. Le tabagisme et la dyslipidémie (1 point chacun, hors FDR HAS) sont
# retirés ensemble.
# Présence dans le dossier : DM_patho_uro pour l'item urologique ; pour le
# tabagisme et la dyslipidémie, chaque facteur déclaré doit être présent
# (PR_tabagisme == 1 => DM_tabagisme == 1, PR_dyslipidemie == 1 =>
# DM_dyslipidemie == 1).

rattrapes <- rattrapes |>
  mutate(
    pts_tabac_dyslip = pts_tabagisme + pts_dyslipidemie,
    uro_necessaire = pts_uro > 0 & (PR_score - pts_uro) < SEUIL_ORANGE,
    tabac_dyslip_necessaire = pts_tabac_dyslip > 0 &
      (PR_score - pts_tabac_dyslip) < SEUIL_ORANGE,
    tabac_dyslip_dans_dossier =
      (!eq1(PR_tabagisme)    | eq1(DM_tabagisme)) &
      (!eq1(PR_dyslipidemie) | eq1(DM_dyslipidemie))
  )

n_rat              <- nrow(rattrapes)
n_score_seuil      <- sum(rattrapes$PR_score == SEUIL_ORANGE)
n_uro              <- sum(rattrapes$uro_necessaire)
n_uro_dossier      <- sum(rattrapes$uro_necessaire & eq1(rattrapes$DM_patho_uro))
n_tabac_dyslip     <- sum(rattrapes$tabac_dyslip_necessaire)
n_tabac_dyslip_dos <- sum(rattrapes$tabac_dyslip_necessaire &
                            rattrapes$tabac_dyslip_dans_dossier)
n_au_moins_un      <- sum(rattrapes$uro_necessaire |
                            rattrapes$tabac_dyslip_necessaire)
n_les_deux         <- sum(rattrapes$uro_necessaire &
                            rattrapes$tabac_dyslip_necessaire)

cat("=== Résultats 8.3 : items ayant conduit au classement à risque",
    "des rattrapés (analyse a posteriori) ===\n")
cat("Parmi les ", n_rat, " patients rattrapés, ", n_score_seuil, " (",
    frf("%.1f", n_score_seuil / n_rat * 100),
    " %) avaient un score de 4, soit exactement le seuil du niveau orange.\n",
    sep = "")
cat("L’item « pathologie urologique » (3 points) était nécessaire pour ",
    "atteindre ce seuil chez ", n_uro, " des ", n_rat, " rattrapés ; ",
    "il ne figurait dans le dossier médical que chez ", n_uro_dossier,
    " d’entre eux.\n", sep = "")
cat("Les points du tabagisme et de la dyslipidémie, qui ne font pas partie ",
    "des facteurs de risque HAS, étaient nécessaires chez ", n_tabac_dyslip,
    " rattrapés ; ces facteurs figuraient dans le dossier chez ",
    n_tabac_dyslip_dos, " d’entre eux.\n", sep = "")
cat("Au total, au moins l’un de ces items était nécessaire chez ",
    n_au_moins_un, " des ", n_rat, " rattrapés (",
    frf("%.1f", n_au_moins_un / n_rat * 100), " %) ; chez ", n_les_deux,
    " d’entre eux, l’item urologique et le tabagisme ou la dyslipidémie ",
    "étaient tous deux nécessaires.\n\n", sep = "")


# ============================================================================
# RÉSULTATS 8.4 — Proportion de rattrapés par sexe
# ============================================================================

if (anyNA(orange_rouge$DM_sexe)) {
  stop("DM_sexe manquant chez au moins un patient classé à risque.",
       call. = FALSE)
}

tab_sexe <- orange_rouge |>
  group_by(DM_sexe) |>
  summarise(
    n = n(),
    reperes = sum(rattrape == 0),
    rattrapes = sum(rattrape),
    .groups = "drop"
  ) |>
  mutate(
    pct = rattrapes / n * 100,
    ic = map2(rattrapes, n, ~ BinomCI(.x, .y, method = "wilson")),
    ic_low  = map_dbl(ic, ~ .[2] * 100),
    ic_high = map_dbl(ic, ~ .[3] * 100)
  ) |>
  select(-ic)

# Fisher (tableau 2 x 2 sexe x rattrapé)
fisher_sexe <- fisher.test(table(orange_rouge$DM_sexe, orange_rouge$rattrape))

femmes <- tab_sexe |> filter(DM_sexe == "F")
hommes <- tab_sexe |> filter(DM_sexe == "M")

cat("=== Résultats 8.4 : par sexe (analyse a posteriori) ===\n")
cat("Parmi les ", nrow(orange_rouge), " patients classés à risque, ",
    femmes$reperes, " des ", femmes$n, " femmes et ", hommes$reperes,
    " des ", hommes$n, " hommes étaient repérés dans le dossier médical.\n",
    sep = "")
cat("La proportion de rattrapés était de ", frf("%.1f", femmes$pct),
    " % chez les femmes (IC 95 % [", frf("%.1f", femmes$ic_low), " ; ",
    frf("%.1f", femmes$ic_high), "]) contre ", frf("%.1f", hommes$pct),
    " % chez les hommes (IC 95 % [", frf("%.1f", hommes$ic_low), " ; ",
    frf("%.1f", hommes$ic_high), "] ; test exact de Fisher, p = ",
    fmt_p_fr(fisher_sexe$p.value), ").\n", sep = "")
cat("Détail : ", femmes$rattrapes, " femmes rattrapées sur ", femmes$n,
    ", ", hommes$rattrapes, " hommes rattrapés sur ", hommes$n, ".\n",
    sep = "")

cat("\nTerminé.\n")
