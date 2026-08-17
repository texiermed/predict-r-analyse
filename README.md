# PREDICT-R — Code source de l'analyse statistique

Code R de l'analyse statistique de l'etude **PREDICT-R** (these de medecine generale, Universite Toulouse III — Paul Sabatier, 2026).

## Etude

**Titre** : Depistage et evaluation du risque de Maladie Renale Chronique en medecine generale : potentiel de l'autoquestionnaire numerique Predict-R dans l'identification des patients a risque.

- **Auteur** : Alexis TEXIER
- **Directrice de these** : Dr Virginie SICRE GATIMEL
- **Presidente du jury** : Pr Marie-Eve ROUGE-BUGAT
- **Promoteur** : DUMG Toulouse
- **Cadre reglementaire** : RIPH3 / MR-003 (avis favorable CPP Est III, avril 2026)

## Structure du depot

```
predict-r-analyse/
├── analyse_finale.R                  # Script principal (analyse complete)
├── concordance_item_par_item.R       # Concordance PR vs DM par facteur de risque
├── cotation_predictr.R               # Cotation officielle Predict-R
├── generer_tableau_item_par_item.R   # Tableau concordance item par item
├── generer_tableaux_sous_groupes.R   # Sous-groupes age et FDR
├── scripts/                          # Pipeline modulaire
│   ├── 00_run_all.R                  # Orchestrateur
│   ├── 01_codebook.R                 # Codebook et preparation des donnees
│   ├── 02_descriptif.R               # Statistiques descriptives (Tableau 1)
│   ├── 03_CJP.R                      # Critere de jugement principal
│   ├── 04_concordance_kappa.R        # Kappa de Cohen + PABAK
│   ├── 05_pratiques_depistage.R      # Examens renaux et conformite HAS
│   ├── 06_performances.R             # Performances diagnostiques (OS6)
│   ├── 07_sous_groupes.R             # Analyses en sous-groupes
│   ├── 08_concordance_FDR.R          # Concordance item par item
│   ├── 09_flowchart_STROBE.R         # Diagramme de flux STROBE
│   ├── 10_faisabilite.R              # Faisabilite (taux, smartphone, BU)
│   └── 11_satisfaction.R             # Satisfaction patients et medecins
└── output/
    └── sessionInfo.txt               # Versions R et packages utilises
```

## Reproductibilite

Les scripts sont concus pour etre executes avec le fichier `00_run_all.R` (pipeline complet) ou individuellement. Les donnees source ne sont pas incluses dans ce depot (voir ci-dessous).

### Environnement

- **R** >= 4.4
- **Packages principaux** : tidyverse, flextable, irr, psych, DescTools, epitools, here, officer
- Voir `output/sessionInfo.txt` pour les versions exactes.

### Methodes statistiques

| Analyse | Methode |
|---|---|
| IC des proportions | Wilson (DescTools::BinomCI) |
| IC des performances diagnostiques | Clopper-Pearson (epitools) |
| Concordance | Kappa de Cohen + PABAK |
| Comparaisons categorielles | Test exact de Fisher |
| Comparaison de medianes | Kruskal-Wallis |
| Test du CJP | Test binomial exact bilateral |

## Donnees

La base de donnees pseudonymisee n'est pas incluse dans ce depot, conformement au cadre RGPD et a la declaration MR-003 (n 2025TA57). Pour toute demande d'acces aux donnees, contacter l'auteur.

## Licence

Ce code est mis a disposition a des fins de transparence scientifique et de reproductibilite. Toute reutilisation doit citer la these originale.

## Contact

Alexis TEXIER — texier.med@gmail.com
