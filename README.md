# PREDICT-R — code R de l'analyse statistique

Ce dépôt contient le code R qui a produit les tableaux et les figures de ma thèse de médecine générale, soutenue à l'Université Toulouse III — Paul Sabatier en 2026. Il correspond à l'Annexe 10 du manuscrit.

**Titre** : Apport de l'outil numérique Predict-R dans l'identification des patients à risque de maladie rénale chronique en médecine générale.

Predict-R est un autoquestionnaire numérique de repérage du risque de maladie rénale chronique, rempli par le patient lui-même et indépendant de toute donnée biologique. Cette étude observationnelle transversale compare sa classification au repérage effectué par le médecin généraliste à partir du dossier médical, chez 139 patients inclus en soins primaires dont 130 analysables.

- Auteur : Alexis TEXIER
- Directrice de thèse : Dr Virginie SICRE GATIMEL
- Promoteur : DUMG de Toulouse
- Cadre réglementaire : RIPH3 / MR-003 n° 2025TA57, avis favorable du CPP Est III (avril 2026)

## Correspondance manuscrit / scripts

Chaque script est autonome : il lit la base et écrit ses sorties dans `output/tableaux/` et `output/figures/`. Les numéros ci-dessous sont ceux du manuscrit.

| Manuscrit | Script |
|---|---|
| Tableaux 1 à 4 et 10, Figures 3, 4 et 6 | `analyse_principale.R` |
| Tableaux 5 et 6 (sous-groupes) | `tableaux_5_6_sous_groupes.R` |
| Tableau 7 et Figure 5 (examens rénaux) | `tableau_7_figure_5_examens_renaux.R` |
| Tableaux 8 et 9 (performances diagnostiques) | `tableaux_8_9_performances.R` |
| Tableau 11 (satisfaction des patients) | `tableau_11_satisfaction_patients.R` |
| Tableau 12 (satisfaction des médecins) | `tableau_12_satisfaction_medecins.R` |
| Annexe 3 (cotation du questionnaire) | `annexe_cotation_predictr.R` |
| Annexe 5 (grille STROBE) | `annexe_grille_strobe.R` |
| Annexe 13 (codebook) | `codebook.md` |
| Annexe 16 (concordance item par item) | `annexe_concordance_item_par_item.R` |

## Deux définitions du dépistage complet

Le dépôt contient volontairement deux variantes du calcul du niveau de dépistage, et c'est le seul point d'attention pour reproduire les résultats :

- `analyse_principale.R` compte le suivi néphrologique comme un dépistage complet ;
- `tableau_7_figure_5_examens_renaux.R` ne retient que l'association DFG + test urinaire. **C'est cette définition stricte qui est celle du manuscrit** pour le Tableau 7 et la Figure 5.

Ce second script intègre aussi une correction de saisie : quatre dossiers portaient une valeur de DFG datée dans la fenêtre d'analyse sans que la case `DM_DFG_disponible` ait été cochée. La variable y est donc définie comme `DM_DFG_disponible | DM_DFG_prescrit_non_realise | !is.na(DM_DFG_valeur)`. Exécuter `analyse_principale.R` seul redonne les valeurs antérieures à cette correction.

## Méthodes statistiques

Toutes les variables qualitatives sont comparées par test exact de Fisher, toutes les variables quantitatives par test de Kruskal-Wallis, et décrites en médiane [Q1-Q3]. Aucun test du Chi², de Student ou d'ANOVA n'est utilisé.

- Intervalles de confiance des proportions : méthode de Wilson (`DescTools::BinomCI`)
- Intervalles de confiance des performances diagnostiques : Clopper-Pearson (`epiR::epi.tests`)
- Critère de jugement principal : test binomial exact bilatéral
- Concordance : kappa de Cohen (`irr::kappa2`, contre-vérifié avec `psych::cohen.kappa`) et PABAK
- Tendance selon le nombre de facteurs de risque : test de Cochran-Armitage (`prop.trend.test`)

Les analyses suivent le guide statistique pré-spécifié dans le protocole approuvé par le CPP. Aucune correction pour tests multiples n'a été appliquée : en dehors du critère de jugement principal, toutes les analyses sont exploratoires.

## Environnement

Analyse réalisée sous R 4.5.2 (Windows 11), avec les packages `tidyverse`, `readxl`, `here`, `gtsummary`, `flextable`, `officer`, `DescTools`, `irr`, `psych` et `epiR`. Les versions exactes utilisées pour l'analyse figurent dans `output/sessionInfo.txt`.

## Données

La base pseudonymisée n'est pas diffusée, conformément au RGPD et à la déclaration MR-003 n° 2025TA57. Les scripts attendent le fichier `data/CRD_PredictRVF_data.xlsx`, dont le dictionnaire des variables est fourni dans `codebook.md`. Toute demande d'accès aux données est à adresser à l'auteur.

## Licence

Code mis à disposition sous licence [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Toute réutilisation doit citer la thèse d'origine.

Contact : Alexis TEXIER — texier.med@gmail.com
