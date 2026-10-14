# PREDICT-R — code R de l'analyse statistique

Ce dépôt contient le code R qui a produit les tableaux et les figures de ma thèse de médecine générale, soutenue à l'Université de Toulouse (Faculté de santé) en 2026. Il correspond à l'Annexe 10 du manuscrit.

**Titre** : Apport de l'outil numérique Predict-R dans l'identification des patients à risque de maladie rénale chronique en médecine générale.

Predict-R est un autoquestionnaire numérique de repérage du risque de maladie rénale chronique, rempli par le patient lui-même et indépendant de toute donnée biologique. Cette étude observationnelle transversale compare sa classification au repérage effectué par le médecin généraliste à partir du dossier médical, chez 139 patients inclus en soins primaires dont 130 analysables.

- Auteur : Alexis TEXIER
- Directrice de thèse : Dr Virginie SICRE-GATIMEL
- Promoteur : DUMG de Toulouse
- Cadre réglementaire : RIPH3 / MR-003 n° 2025TA57, avis favorable du CPP Est III (avril 2026)

## Correspondance manuscrit / scripts

Chaque script lit lui-même la base (feuille `donnees`), applique les corrections documentées dont il a besoin (voir plus bas) et écrit ses sorties dans `output/tableaux/` et `output/figures/`. Les chiffres cités dans le texte des Résultats sont imprimés en console. Les numéros ci-dessous sont ceux du manuscrit.

| Manuscrit | Script |
|---|---|
| Résultats 1 à 7 et 11, Tableaux 1 à 4 et 10, Figures 3, 4 et 6 | `analyse_principale.R` |
| Résultats 8.1 et 8.2, Tableaux 5 et 6 (sous-groupes) | `tableaux_5_6_sous_groupes.R` |
| Résultats 8.3 et 8.4 (analyses a posteriori, sortie console) | `resultats_8_3_8_4_a_posteriori.R` |
| Résultats 9, Tableau 7 et Figure 5 (examens rénaux) | `tableau_7_figure_5_examens_renaux.R` |
| Résultats 10, Tableaux 8 et 9 (performances diagnostiques) | `tableaux_8_9_performances.R` |
| Résultats 12.1, Tableau 11 (satisfaction des patients) | `tableau_11_satisfaction_patients.R` |
| Résultats 12.2, Tableau 12 (satisfaction des médecins) | `tableau_12_satisfaction_medecins.R` |
| Annexe 3 (barème du questionnaire) | `annexe_cotation_predictr.R` |
| Annexe 5 (grille STROBE, 32 lignes) | `annexe_grille_strobe.R` |
| Annexe 13 (dictionnaire des variables) | `codebook.md` |
| Annexe 16 (concordance item par item) | `annexe_concordance_item_par_item.R` |

Le diagramme de flux (Figure 2) a été dessiné à la main ; ses effectifs sont imprimés par `analyse_principale.R` (section 1.6).

## Deux définitions du dépistage complet

Le dépôt contient volontairement deux variantes du calcul du niveau de dépistage, et c'est le principal point d'attention pour reproduire les résultats :

- `analyse_principale.R` compte le suivi néphrologique comme un dépistage complet ;
- `tableau_7_figure_5_examens_renaux.R` ne retient que l'association DFG + test urinaire. **C'est cette définition stricte qui est celle du manuscrit** pour le Tableau 7 et la Figure 5.

Ce second script intègre aussi une correction de saisie : quatre dossiers portaient une valeur de DFG datée dans la fenêtre d'analyse sans que la case `DM_DFG_disponible` ait été cochée. La variable y est donc définie comme `DM_DFG_disponible | DM_DFG_prescrit_non_realise | !is.na(DM_DFG_valeur)` (le script imprime ce nombre de dossiers). Exécuter `analyse_principale.R` seul redonne les valeurs antérieures à cette correction.

Pour la même raison, `analyse_principale.R` produit encore ses propres versions du dépistage, des performances et de la satisfaction : ces sorties portent le suffixe `_version_de_travail` et ne sont pas celles du manuscrit.

## Corrections documentées

Trois corrections de saisie sont appliquées dans le code, sans modifier la base :

- **PRED134** : RAC prescrit mais `DM_RAC` codé 0 par erreur, recodé à 1 dans tous les scripts qui utilisent cette variable ;
- **PRED001, PRED071, PRED129** : albuminurie persistante (second RAC ≥ 3 mg/mmol à plus de 3 mois, vérifié dans le dossier), donc MRC connue selon la définition du manuscrit ; `DM_MRC_connue` est recodé à 1 dans `analyse_principale.R` (5 → 8 MRC connues, Tableaux 1 et 2, Résultats 4.3 et 6) ;
- **quatre dossiers** avec une valeur de DFG sans case cochée (`tableau_7_figure_5_examens_renaux.R`, voir ci-dessus).

## Méthodes statistiques

Toutes les variables qualitatives sont comparées par test exact de Fisher, toutes les variables quantitatives par test de Kruskal-Wallis, et décrites en médiane [Q1 ; Q3]. Aucun test du Chi² d'indépendance, de Student ou d'ANOVA n'est utilisé ; seul le test de tendance de Cochran-Armitage repose sur une statistique du Chi² à 1 degré de liberté.

- Intervalles de confiance des proportions : méthode de Wilson (`DescTools::BinomCI`)
- Intervalles de confiance des performances diagnostiques : Clopper-Pearson (`epiR::epi.tests`)
- Critère de jugement principal : test binomial exact bilatéral
- Concordance : kappa de Cohen (`psych::cohen.kappa`, valeur et IC 95 %, contre-vérifié avec `irr::kappa2`) et PABAK
- Tendance selon le nombre de facteurs de risque : test de Cochran-Armitage (`prop.trend.test`)

Les analyses suivent le guide statistique pré-spécifié dans le protocole approuvé par le CPP, à l'exception des analyses a posteriori signalées comme telles dans le manuscrit (Résultats 8.3 et 8.4, effet de la bandelette urinaire en Résultats 11). Aucune correction pour tests multiples n'a été appliquée : en dehors du critère de jugement principal, toutes les analyses sont exploratoires.

## Environnement

Analyse réalisée sous R 4.5.2 (Windows 11), avec les packages `tidyverse`, `readxl`, `here`, `gtsummary`, `flextable`, `officer`, `DescTools`, `irr`, `psych` et `epiR`. Les versions exactes utilisées pour l'analyse figurent dans `output/sessionInfo.txt`. `broom` ≥ 1.0.8 est nécessaire : avec une version plus ancienne, `gtsummary::add_p()` laisse vide la colonne p du Tableau 1 (le script s'arrête alors avec un message). Exécuter R en locale UTF-8.

## Données

La base pseudonymisée n'est pas diffusée, conformément au RGPD et à la déclaration MR-003 n° 2025TA57. Les scripts attendent le fichier `data/CRD_PredictRVF_data.xlsx`, dont le dictionnaire des variables est fourni dans `codebook.md`. Les Tableaux 11 et 12 et la Figure 6 lisent aussi `data/satisfaction_patient.csv` et `data/satisfaction_medecin.csv` (exports Google Forms, non diffusés) ; en leur absence, `analyse_principale.R` s'exécute jusqu'au bout sans ces sorties. Toute demande d'accès aux données est à adresser à l'auteur.

## Licence

Code mis à disposition sous licence [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Toute réutilisation doit citer la thèse d'origine.

Contact : Alexis TEXIER — texier.med@gmail.com
