# PREDICT-R — Code source de l'analyse statistique

[![R >= 4.4](https://img.shields.io/badge/R-%E2%89%A5%204.4-blue)](https://cran.r-project.org/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![STROBE](https://img.shields.io/badge/reporting-STROBE-green)](https://www.strobe-statement.org/)

Code R de l'analyse statistique de l'étude **PREDICT-R**, thèse de médecine générale soutenue à l'Université Toulouse III — Paul Sabatier (2026).

## Étude

**Titre** : Apport de l'outil numérique Predict-R dans l'identification des patients à risque de maladie rénale chronique en médecine générale.

**Contexte** : La maladie rénale chronique (MRC) touche environ 6,3 % des adultes en France, mais reste largement sous-dépistée. Predict-R est un autoquestionnaire numérique de repérage du risque de MRC, autoadministrable par le patient via un QR code, indépendant de toute donnée biologique. Cette étude observationnelle transversale évalue son apport en soins primaires en comparant la classification Predict-R au repérage effectué par le médecin généraliste à partir du dossier médical.

- **Auteur** : Alexis TEXIER
- **Directrice de thèse** : Dr Virginie SICRE GATIMEL
- **Présidente du jury** : Pr Marie-Ève ROUGÉ-BUGAT
- **Promoteur** : DUMG Toulouse (Université Toulouse III — Paul Sabatier)
- **Cadre réglementaire** : RIPH3 / MR-003, avis favorable CPP Est III (avril 2026)
- **Population** : 139 patients inclus, 130 analysables (MSP de Labarthe-sur-Lèze, Haute-Garonne)

## Structure du dépôt

```
predict-r-analyse/
├── analyse_finale.R                  # Script principal (analyse complète)
├── concordance_item_par_item.R       # Concordance PR vs DM par facteur de risque
├── cotation_predictr.R               # Cotation officielle Predict-R (barème)
├── generer_tableau_item_par_item.R   # Génération du tableau de concordance
├── generer_tableaux_sous_groupes.R   # Analyses en sous-groupes (âge, FDR)
├── scripts/                          # Pipeline modulaire
│   ├── 00_run_all.R                  # Orchestrateur (lance tout le pipeline)
│   ├── 01_codebook.R                 # Import, recodage, variables dérivées
│   ├── 02_descriptif.R               # Statistiques descriptives (Tableau 1)
│   ├── 03_CJP.R                      # Critère de jugement principal
│   ├── 04_concordance_kappa.R        # Kappa de Cohen + PABAK
│   ├── 05_pratiques_depistage.R      # Examens rénaux et conformité HAS
│   ├── 06_performances.R             # Performances diagnostiques (exploratoire)
│   ├── 07_sous_groupes.R             # Analyses en sous-groupes
│   ├── 08_concordance_FDR.R          # Concordance item par item PR vs DM
│   ├── 09_flowchart_STROBE.R         # Diagramme de flux STROBE
│   ├── 10_faisabilite.R              # Indicateurs de faisabilité
│   └── 11_satisfaction.R             # Satisfaction patients et médecins
├── codebook.md                       # Dictionnaire des variables (67 variables)
└── output/
    └── sessionInfo.txt               # Versions R et packages utilisés
```

## Reproductibilité

Les scripts sont conçus pour être exécutés avec `scripts/00_run_all.R` (pipeline complet) ou individuellement. Chaque script charge automatiquement le codebook (`01_codebook.R`) au démarrage.

### Environnement requis

- **R** >= 4.4
- **Packages** : tidyverse, flextable, irr, psych, DescTools, epitools, here, officer, gtsummary, gt, epiR, readxl

Voir `output/sessionInfo.txt` pour les versions exactes utilisées dans l'analyse.

### Installation

```r
install.packages(c(
  "tidyverse", "here", "readxl",
  "gtsummary", "gt", "flextable", "officer",
  "irr", "psych", "DescTools", "epitools", "epiR"
))
```

## Méthodes statistiques

| Analyse | Méthode | Script |
|---|---|---|
| IC des proportions | Wilson | `03_CJP.R` |
| IC des performances diagnostiques | Clopper-Pearson | `06_performances.R` |
| Concordance | Kappa de Cohen (IC asymptotique) + PABAK | `04_concordance_kappa.R` |
| Comparaisons catégorielles | Test exact de Fisher / Chi² | `02_descriptif.R` |
| Comparaison de médianes | Kruskal-Wallis | `02_descriptif.R` |
| Test du critère de jugement principal | Test binomial exact bilatéral | `03_CJP.R` |
| Tendance par FDR | Test de Cochran-Armitage | `07_sous_groupes.R` |

Toutes les analyses suivent le guide statistique pré-spécifié (approuvé par le CPP).

## Données

La base de données pseudonymisée n'est pas incluse dans ce dépôt, conformément au cadre RGPD et à la déclaration MR-003 (n° 2025TA57). Pour toute demande d'accès aux données, contacter l'auteur.

## Comment citer

```
TEXIER A. Apport de l'outil numérique Predict-R dans l'identification des patients
à risque de maladie rénale chronique en médecine générale. Thèse de médecine générale,
Université Toulouse III — Paul Sabatier, 2026.
```

## Licence

Ce code est mis à disposition sous licence [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). Toute réutilisation doit citer la thèse originale.

## Contact

**Alexis TEXIER** — [texier.med@gmail.com](mailto:texier.med@gmail.com)
