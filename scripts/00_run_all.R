# ============================================================================
# 00_run_all.R - Pipeline complet d'analyse PREDICT-R
# ----------------------------------------------------------------------------
# Execute l'ensemble des analyses dans l'ordre.
# Utiliser ce script pour produire tous les resultats en une seule commande.
#
# Prerequis : tous les packages installes (voir README.md).
#
# Usage dans RStudio :
#   - Ouvrir le projet RStudio (10 - Analyse R.Rproj)
#   - Ouvrir ce fichier
#   - Ctrl + A puis Ctrl + Entree
# ============================================================================

library(here)

cat("\n=========================================\n")
cat("ANALYSE PREDICT-R - PIPELINE COMPLET\n")
cat("Date :", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("=========================================\n\n")

# Note : pas de nettoyage automatique des fichiers de sortie.
# Les fichiers avec le meme nom sont ecrases. Si on renomme un fichier,
# l'ancien reste dans output/ -> a supprimer manuellement.
# Pour nettoyer manuellement, executer :
#   unlink(list.files(here("output", "tableaux"), full.names = TRUE))
#   unlink(list.files(here("output", "figures"), full.names = TRUE))

# Codebook (charge predict_r et labels_predict_r)
source(here("scripts", "01_codebook.R"))

# Tableau 1 et figures descriptives
source(here("scripts", "02_descriptif.R"))

# Critere de jugement principal
source(here("scripts", "03_CJP.R"))

# Concordance et Kappa
source(here("scripts", "04_concordance_kappa.R"))

# Pratiques de depistage (OS5)
source(here("scripts", "05_pratiques_depistage.R"))

# Performances diagnostiques (OS6 exploratoire)
source(here("scripts", "06_performances.R"))

# Sous-groupes (coherence, FDR, age)
source(here("scripts", "07_sous_groupes.R"))

# Concordance PR vs DM par FDR (analyse des discordances item par item)
source(here("scripts", "08_concordance_FDR.R"))

# Diagramme de flux STROBE
source(here("scripts", "09_flowchart_STROBE.R"))

# Faisabilite (taux participation, refus, BU)
source(here("scripts", "10_faisabilite.R"))

# Satisfaction patient et medecin (donnees fictives en attendant la saisie)
source(here("scripts", "11_satisfaction.R"))

cat("\n=========================================\n")
cat("PIPELINE TERMINE - voir output/ pour les resultats\n")
cat("=========================================\n\n")
