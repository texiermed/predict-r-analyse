# ============================================================================
# 01_codebook.R - Etude PREDICT-R
# ----------------------------------------------------------------------------
# Auteur  : Alexis TEXIER
# Version : 1.0 - 14/05/2026
# ----------------------------------------------------------------------------
# OBJECTIF :
#   Importer les donnees brutes (CSV) et construire un data frame propre,
#   pret pour les analyses statistiques.
#
# CE SCRIPT REALISE :
#   A. Setup (packages, chemins)
#   B. Import des donnees brutes (CSV)
#   C. Conversion automatique "Oui"/"Non" -> 1/0 (format HEKO)
#   D. Recodage des variables (factor, dates, types)
#   E. Creation des variables derivees (calculees)
#   F. Validation et controle qualite
#
# SORTIE :
#   - Un data frame `predict_r` propre, dispo dans l'environnement
#   - Charger ce script au debut de chaque analyse :
#       source(here("scripts", "01_codebook.R"))
#
# REFERENCES METHODOLOGIQUES :
#   - 01 - Protocole/PROTOCOLE_PREDICT-R.md (v2.0)
#   - 01 - Protocole/GUIDE_STATISTIQUE_PREDICT-R.md (v1.1)
#   - 08 - Donnees/GUIDE_CRD_PREDICT-R.md (definitions variables)
# ============================================================================


# ============================================================================
# A. SETUP - Packages et chemins
# ============================================================================

# Charger les packages necessaires (tidyverse + here pour les chemins)
library(tidyverse)   # dplyr, ggplot2, readr, stringr, tidyr, lubridate
library(here)        # gestion des chemins (chemins relatifs au projet)

# `here::here()` construit automatiquement les chemins depuis la racine du
# projet RStudio. Si tu n'es pas dans un projet, R va chercher un fichier
# .Rproj ou utiliser getwd(). En cas de probleme, definir manuellement :
# here::i_am("scripts/01_codebook.R")

cat("Working directory :", here(), "\n\n")


# ============================================================================
# B. IMPORT DES DONNEES BRUTES
# ============================================================================

# Chemin vers le CSV : pour l'instant les donnees simulees,
# remplacer par "CRD_PredictR.csv" quand le recueil sera fini.
chemin_csv <- here("data", "data_simulees.csv")

# Lecture du CSV
predict_r_brut <- read_csv(
  chemin_csv,
  na = c("", "NA"),         # chaines a interpreter comme valeur manquante
  show_col_types = FALSE    # ne pas afficher le type devine de chaque colonne
)

cat("Donnees brutes importees :\n")
cat("  - Patients :", nrow(predict_r_brut), "\n")
cat("  - Variables :", ncol(predict_r_brut), "\n\n")


# ============================================================================
# C. CONVERSION "Oui" / "Non" -> 1 / 0
# ============================================================================
# HEKO exporte les variables binaires sous forme de chaines "Oui" / "Non".
# Cette fonction detecte automatiquement les colonnes texte ne contenant
# que ces deux valeurs et les convertit en 0/1.
# Sans effet sur les donnees simulees (deja en 0/1).

convertir_oui_non <- function(x) {
  if (!is.character(x)) return(x)
  vals <- unique(na.omit(x))
  if (length(vals) > 0 && all(tolower(vals) %in% c("oui", "non"))) {
    return(as.integer(tolower(x) == "oui"))
  }
  return(x)
}

predict_r_brut <- predict_r_brut |>
  mutate(across(where(is.character), convertir_oui_non))

# --------------------------------------------------------------------
# Exclusion des patients ayant retire leur non-opposition apres inclusion
# (critere d'exclusion - protocole CPP §5.4)
# --------------------------------------------------------------------
n_retraits <- 0
if ("retrait_non_opposition" %in% names(predict_r_brut)) {
  n_retraits <- sum(predict_r_brut$retrait_non_opposition == 1, na.rm = TRUE)
  predict_r_brut <- predict_r_brut |>
    filter(is.na(retrait_non_opposition) | retrait_non_opposition == 0)
  cat(" - Patients retires (non-opposition) :", n_retraits, "\n")
  cat(" - Patients restants apres exclusion :", nrow(predict_r_brut), "\n\n")
}


# ============================================================================
# D. RECODAGE DES VARIABLES - types et factor levels
# ============================================================================
# On transforme certaines colonnes en factor avec un ordre explicite.
# C'est crucial pour :
#   - gtsummary (ordre des colonnes dans le tableau 1)
#   - ggplot2 (ordre sur l'axe x)
#   - les tests de tendance (Cochran-Armitage, guide stat §3)

predict_r <- predict_r_brut |>
  mutate(
    # --- DATES ---
    date_inclusion = as.Date(date_inclusion),
    PR_date = as.Date(PR_date),
    DM_DFG_date = as.Date(DM_DFG_date),
    DM_suivi_nephro_date = as.Date(DM_suivi_nephro_date),

    # --- FACTORS ORDONNES ---
    # Niveau de risque Predict-R : ordre vert < orange < rouge (critique)
    PR_couleur = factor(PR_couleur,
                        levels = c("vert", "orange", "rouge")),

    # Sexe DM
    DM_sexe = factor(DM_sexe, levels = c("F", "M")),

    # Modalite de completion (5 niveaux ordonnes du plus autonome au moins)
    modalite_completion = factor(
      modalite_completion,
      levels = c("AUTO_SMART", "AUTO_PC", "AIDE_SMART", "AIDE_PC", "INV")
    ),

    # Stade MRC (ordre clinique)
    DM_MRC_stade = factor(DM_MRC_stade,
                          levels = c("G1", "G2", "G3a", "G3b",
                                     "G4", "G5", "dialyse", "greffe",
                                     "inconnu", "NA")),

    # Critere ayant defini la MRC connue
    DM_MRC_critere = factor(DM_MRC_critere,
                            levels = c("ATCD_code", "DFG_confirme",
                                       "nephro_MRC", "alb_persistante",
                                       "NA"))
  )


# ============================================================================
# E. VARIABLES DERIVEES - CE QU'ON CALCULE
# ============================================================================
# Reference : GUIDE_CRD_PREDICT-R.md (section "Ce qui sera calcule dans R")
# Ces variables sont indispensables pour toutes les analyses qui suivent.

ANNEE_ETUDE <- 2026   # parametre central, modifier si besoin

predict_r <- predict_r |>
  mutate(

    # ------ Age et tranches d'age (guide stat - sous-groupes) ------
    age = ANNEE_ETUDE - DM_annee_naissance,

    tranche_age = case_when(
      age < 40 ~ "< 40",
      age <= 60 ~ "40-60",
      TRUE ~ "> 60"
    ),
    tranche_age = factor(tranche_age,
                         levels = c("< 40", "40-60", "> 60")),

    # ------ Nombre de FDR HAS (12 facteurs) ------
    # On exclut intentionnellement : tabagisme, dyslipidemie, maladie rare,
    # prematurite (hors HAS, gardes comme comparateurs PR vs DM)
    # cf. protocole v2.0 §5.6 et guide stat §7
    nb_FDR_HAS = DM_HTA + DM_diabete + DM_obesite + DM_MCV +
                 DM_insuf_cardiaque + DM_maladie_auto_immune +
                 DM_patho_uro + DM_ATCD_fam_nephro +
                 DM_ATCD_nephro_aigue + DM_nephrotoxiques +
                 DM_expo_PCI_radiotherapie + DM_expo_toxiques_pro,

    # Categorisation FDR (pour Cochran-Armitage, guide stat - sous-groupe FDR)
    cat_FDR_HAS = case_when(
      nb_FDR_HAS == 0 ~ "0",
      nb_FDR_HAS <= 2 ~ "1-2",
      TRUE ~ ">=3"
    ),
    cat_FDR_HAS = factor(cat_FDR_HAS,
                         levels = c("0", "1-2", ">=3")),

    # ------ Statut "repere" dans le dossier medical ------
    # Au moins un test urinaire prescrit/realise (5 types acceptes)
    test_urinaire = as.integer(
      DM_RAC == 1 | DM_prot_creat == 1 | DM_microalbuminurie == 1 |
      DM_BU_automate == 1 | DM_proteinurie == 1
    ),

    # DFG cote : prescrit OU realise (les deux valident le reperage)
    # - DM_DFG_disponible : prescrit OU resultat dispo (binaire)
    # - DM_DFG_prescrit_non_realise : flag du sous-cas "prescrit non realise"
    # Note : la valeur DM_DFG_valeur n'existe que si resultat dispo (pas si
    # seulement prescrit) - utilisee pour MRC_KDIGO (OS6).
    DFG_evalue = as.integer(
      DM_DFG_disponible == 1 | DM_DFG_prescrit_non_realise == 1
    ),

    # Repere = (DFG evalue ET test urinaire) OU suivi nephrologique
    # Definition binaire stricte (cf. protocole v2.0)
    repere = as.integer(
      (DFG_evalue == 1 & test_urinaire == 1) | DM_suivi_nephro == 1
    ),

    # ------ Statut "rattrape" = CRITERE DE JUGEMENT PRINCIPAL ------
    # Un patient est "rattrape" par Predict-R si :
    #   - il est classe orange ou rouge par Predict-R
    #   - ET il n'etait PAS repere dans le dossier medical
    rattrape = as.integer(
      PR_couleur %in% c("orange", "rouge") & repere == 0
    ),

    # ------ Variable binaire PR pour Kappa et concordance ------
    # 0 = vert (faible risque), 1 = orange ou rouge (a risque)
    PR_bin = as.integer(PR_couleur %in% c("orange", "rouge")),

    # ------ Categorie de concordance (pour description) ------
    concordance = case_when(
      PR_bin == 0 & repere == 0 ~ "concordant_normal",
      PR_bin == 0 & repere == 1 ~ "oublie_PR",
      PR_bin == 1 & repere == 1 ~ "concordant_a_risque",
      PR_bin == 1 & repere == 0 ~ "rattrape"
    ),
    concordance = factor(concordance,
                         levels = c("concordant_normal",
                                    "oublie_PR",
                                    "concordant_a_risque",
                                    "rattrape")),

    # ------ Niveau de depistage realise (OS5, 5 categories hierarchiques) ------
    # Libelles francais propres pour copier-coller direct dans la these.
    depistage_niveau = case_when(
      DM_suivi_nephro == 1 ~ "Suivi nephrologique",
      DFG_evalue == 1 & test_urinaire == 1 ~ "Complet (DFG + test urinaire)",
      DFG_evalue == 0 & test_urinaire == 1 ~ "Partiel : test urinaire seul",
      DFG_evalue == 1 & test_urinaire == 0 ~ "Partiel : DFG seul",
      TRUE ~ "Aucun depistage"
    ),
    depistage_niveau = factor(depistage_niveau,
                              levels = c("Aucun depistage",
                                         "Partiel : DFG seul",
                                         "Partiel : test urinaire seul",
                                         "Complet (DFG + test urinaire)",
                                         "Suivi nephrologique")),

    # ------ MRC selon KDIGO (gold standard exploratoire OS6) ------
    # NA si on n'a NI DFG NI RAC -> indispensable pour exclure les patients
    # sans biologie du sous-groupe performances diagnostiques.
    MRC_KDIGO = case_when(
      is.na(DM_DFG_valeur) & is.na(DM_RAC_valeur) ~ NA_integer_,
      (!is.na(DM_DFG_valeur) & DM_DFG_valeur < 60) |
      (!is.na(DM_RAC_valeur) & DM_RAC_valeur >= 3) ~ 1L,
      TRUE ~ 0L
    ),

    # ------ Variables MIX (PR OU DM) pour Tableau 1 descriptif ------
    # Logique : si POSITIF dans au moins une des 2 sources -> compte positif.
    # Permet de decrire au mieux la population (description maximale).
    # L'analyse des discordances PR vs DM sera faite separement.
    mix_HTA = as.integer(PR_HTA == 1 | DM_HTA == 1),
    mix_diabete = as.integer(PR_diabete == 1 | DM_diabete == 1),
    mix_tabagisme = as.integer(PR_tabagisme == 1 | DM_tabagisme == 1),
    mix_dyslipidemie = as.integer(PR_dyslipidemie == 1 | DM_dyslipidemie == 1),
    mix_MCV = as.integer(PR_MCV == 1 | DM_MCV == 1),
    mix_insuf_cardiaque = as.integer(PR_insuf_cardiaque == 1 | DM_insuf_cardiaque == 1),
    # Patho uro : PR a 2 sous-questions (recidivante Q15 / connue Q16), DM a 1
    mix_patho_uro = as.integer(
      PR_patho_uro_recidivante == 1 | PR_maladie_uro_connue == 1 | DM_patho_uro == 1
    ),
    mix_obesite = as.integer((PR_IMC > 30) | DM_obesite == 1),
    mix_prematurite = as.integer(PR_prematurite == 1 | DM_prematurite == 1),
    mix_AINS = as.integer(PR_AINS == 1 | DM_ttt_AINS == 1),
    # Expo : PR a 2 sous-items (PCI Q19 / radio Q20), DM a 1
    mix_expo_contraste = as.integer(
      PR_expo_PCI == 1 | PR_expo_radiotherapie == 1 | DM_expo_PCI_radiotherapie == 1
    )
  )


# ============================================================================
# F. VALIDATION ET CONTROLE QUALITE
# ============================================================================
# Verifications basiques avant d'attaquer les analyses.

cat("---------------------------------\n")
cat("VALIDATION DU CODEBOOK\n")
cat("---------------------------------\n\n")

# 1. Effectifs par groupe Predict-R
cat("Repartition Predict-R :\n")
print(predict_r |> count(PR_couleur))
cat("\n")

# 2. Effectifs reperes / non reperes
cat("Statut repere :\n")
print(predict_r |> count(repere))
cat("\n")

# 3. Effectifs concordance (4 categories)
cat("Concordance Predict-R / DM :\n")
print(predict_r |> count(concordance))
cat("\n")

# 4. Donnees manquantes par variable cle
cat("Donnees manquantes sur variables cles :\n")
predict_r |>
  summarise(
    age_NA = sum(is.na(age)),
    PR_couleur_NA = sum(is.na(PR_couleur)),
    repere_NA = sum(is.na(repere)),
    DM_DFG_valeur_NA = sum(is.na(DM_DFG_valeur)),
    MRC_KDIGO_NA = sum(is.na(MRC_KDIGO))
  ) |>
  print()
cat("\n")

# 5. Coherence interne : range d'age plausible
range_age <- range(predict_r$age, na.rm = TRUE)
if (range_age[1] < 18 | range_age[2] > 110) {
  warning("Ages aberrants detectes : ", range_age[1], " - ", range_age[2])
} else {
  cat("Age range : ", range_age[1], " - ", range_age[2],
      "(OK, plausible)\n")
}


# ============================================================================
# G. LABELS POUR gtsummary (utilises dans 02_descriptif.R)
# ============================================================================
# On stocke les labels dans une liste qu'on pourra reutiliser dans
# les appels a tbl_summary().

labels_predict_r <- list(
  age = "Age (annees)",
  tranche_age = "Tranche d'age",
  DM_sexe = "Sexe",
  PR_IMC = "IMC (kg/m^2)",
  PR_couleur = "Niveau de risque Predict-R",
  DM_HTA = "Hypertension arterielle",
  DM_diabete = "Diabete",
  DM_obesite = "Obesite",
  DM_MCV = "Maladie cardiovasculaire",
  DM_insuf_cardiaque = "Insuffisance cardiaque",
  DM_maladie_auto_immune = "Maladie auto-immune",
  DM_patho_uro = "Pathologie urologique",
  DM_tabagisme = "Tabagisme",
  DM_dyslipidemie = "Dyslipidemie",
  DM_MRC_connue = "MRC connue",
  nb_FDR_HAS = "Nombre de FDR HAS",
  cat_FDR_HAS = "Nombre de FDR HAS (categorie)",
  repere = "Repere dans le dossier medical",
  rattrape = "Rattrape par Predict-R",
  depistage_niveau = "Niveau de depistage realise"
)


# ============================================================================
# H. MESSAGE FINAL
# ============================================================================

cat("\n---------------------------------\n")
cat("CODEBOOK CHARGE\n")
cat("---------------------------------\n")
cat("Data frame disponible : `predict_r` (", nrow(predict_r), " lignes, ",
    ncol(predict_r), " colonnes)\n", sep = "")
cat("Liste de labels disponible : `labels_predict_r`\n")
cat("Pret pour les analyses (scripts 02 a 07).\n\n")
