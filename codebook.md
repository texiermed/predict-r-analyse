# Codebook — Etude PREDICT-R

> Dictionnaire des 67 variables utilisees dans l'analyse statistique.

**Version** : 1.0 | **Date** : 14/05/2026 | **Auteur** : Alexis TEXIER

---

## Structure du jeu de donnees analytique

- **Source** : CRD (Cahier de Recueil de Donnees) — 64 variables brutes
- **Sortie codebook.R** : data frame `predict_r` enrichi des variables derivees
- **Identifiant** : `id_patient` (format `PRED001`, `PRED002`...)

---

## Variables brutes (64 colonnes)

### Bloc 1 — Identification

| Variable | Type | Modalites | Description |
|---|---|---|---|
| `id_patient` | character | `PRED001`-`PRED200` | Identifiant pseudonymise |
| `date_inclusion` | Date | AAAA-MM-JJ | Date d'inclusion dans l'etude |

### Bloc 2 — Donnees Predict-R (HEKO)

| Variable | Type | Modalites | Description | Points |
|---|---|---|---|---|
| `PR_annee_naissance` | numeric | >= 1900 | Annee de naissance declaree | — |
| `PR_prematurite` | 0/1 | binaire | Ne(e) prematurement < 37 SA | 1 |
| `PR_HTA` | 0/1 | binaire | HTA traitee ou non | 2 |
| `PR_tabagisme` | 0/1 | binaire | Tabac quotidien | 1 |
| `PR_diabete` | 0/1 | binaire | Diabetique | voir anciennete |
| `PR_diabete_anciennete` | categoriel | `<5_ans`, `>=5_ans`, NA | Anciennete du diabete | **2** si < 5 ans, **4** si >= 5 ans |
| `PR_dyslipidemie` | 0/1 | binaire | Anomalie cholesterol/triglycerides | 1 |
| `PR_MCV` | 0/1 | binaire | Maladie cardiovasculaire (AVC, IDM, AOMI) | 1 |
| `PR_insuf_cardiaque` | 0/1 | binaire | Insuffisance cardiaque | 1 |
| `PR_IMC` | numeric | kg/m2 | IMC (calcule par HEKO) | 1 si > 30 |
| `PR_patho_uro_recidivante` | 0/1 | binaire | IU recidivantes / calculs / pyelonephrites / nephrectomie | **3** |
| `PR_AINS` | 0/1 | binaire | AINS > 1x/semaine | 1 |
| `PR_expo_PCI` | 0/1 | binaire | Scanner PCI > 1x/an | 1 |
| `PR_expo_radiotherapie` | 0/1 | binaire | Radiotherapie ventre | 1 |
| `PR_BU_realisee` | 0/1 | binaire | Bandelette urinaire optionnelle realisee | — |
| `PR_BU_hematurie` | 0/1/NA | categoriel | Resultat BU hematurie (NA si non realisee) | **6** si positive |
| `PR_BU_proteinurie` | 0/1/NA | categoriel | Resultat BU proteinurie (NA si non realisee) | **6** si positive |
| `PR_score` | numeric | entier >= 0 | Score total Predict-R (calcule par HEKO) | — |
| `PR_couleur` | factor | `vert`, `orange`, `rouge` | Niveau de risque (vert <4, orange 4-5, rouge >=6) | — |

### Bloc 3 — Sociodemographiques DM

| Variable | Type | Modalites | Description |
|---|---|---|---|
| `DM_annee_naissance` | numeric | >= 1900 | Annee de naissance (source principale pour `age`) |
| `DM_sexe` | factor | `M`, `F` | Sexe |

### Bloc 4 — Facteurs de risque DM

| Variable | Type | Description | FDR HAS |
|---|---|---|---|
| `DM_HTA` | 0/1 | HTA (ATCD code OU traitement antihypertenseur) | oui |
| `DM_diabete` | 0/1 | Diabete (ATCD code OU traitement antidiabetique) | oui |
| `DM_obesite` | 0/1 | Obesite (ATCD code) | oui |
| `DM_MCV` | 0/1 | MCV atheromateuse (AVC, IDM, AOMI) | oui |
| `DM_insuf_cardiaque` | 0/1 | Insuffisance cardiaque | oui |
| `DM_maladie_auto_immune` | 0/1 | Maladie auto-immune / systemique | oui |
| `DM_patho_uro` | 0/1 | Pathologie urologique | oui |
| `DM_ATCD_fam_nephro` | 0/1 | ATCD familiaux de nephropathie | oui |
| `DM_ATCD_nephro_aigue` | 0/1 | ATCD nephropathie aigue | oui |
| `DM_nephrotoxiques` | 0/1 | Exposition medicaments nephrotoxiques | oui |
| `DM_expo_PCI_radiotherapie` | 0/1 | Exposition PCI ou radiotherapie renale | oui |
| `DM_expo_toxiques_pro` | 0/1 | Toxiques professionnels (Pb, Cd, Hg) | oui |
| `DM_tabagisme` | 0/1 | Tabagisme actif | non (comparatif PR) |
| `DM_dyslipidemie` | 0/1 | Dyslipidemie (ATCD code OU statine) | non (comparatif PR) |
| `DM_prematurite` | 0/1 | Prematurite (ATCD code) | non (comparatif PR) |

### Bloc 5 — Biologie renale DM

| Variable | Type | Description |
|---|---|---|
| `DM_DFG_disponible` | 0/1 | DFG prescrit OU resultat disponible (fenetre 01/01/2025 - date inclusion) |
| `DM_DFG_valeur` | numeric | Valeur DFG CKD-EPI (mL/min/1.73 m2) |
| `DM_DFG_prescrit_non_realise` | 0/1 | DFG prescrit mais non realise par le patient |
| `DM_RAC` | 0/1 | RAC prescrit ou disponible |
| `DM_prot_creat` | 0/1 | Proteinurie/creatininurie prescrite ou disponible |
| `DM_microalbuminurie` | 0/1 | Microalbuminurie prescrite ou disponible |
| `DM_BU_automate` | 0/1 | BU avec lecture automate prescrite ou disponible |
| `DM_proteinurie` | 0/1 | Proteinurie prescrite ou disponible |

### Bloc 6 — Suivi nephro + MRC

| Variable | Type | Description |
|---|---|---|
| `DM_suivi_nephro` | 0/1 | Courrier/avis/consult nephro (fenetre 01/01/2025 - date inclusion) |
| `DM_MRC_connue` | 0/1 | MRC connue selon criteres HAS/KDIGO |
| `DM_MRC_stade` | factor | Stade MRC si connue (G1-G5, dialyse, greffe) |

### Bloc 7 — Traitements DM

| Variable | Type | Description |
|---|---|---|
| `DM_ttt_antihypertenseur` | 0/1 | Traitement antihypertenseur en cours |
| `DM_ttt_antidiabetique` | 0/1 | Traitement antidiabetique en cours |
| `DM_ttt_AINS` | 0/1 | AINS chroniques ou repetes |
| `DM_ttt_nephroprotecteur` | 0/1 | IEC / ARA2 / iSGLT2 en cours |

---

## Variables derivees (calculees dans `01_codebook.R`)

| Variable | Formule | Usage |
|---|---|---|
| `age` | `ANNEE_ETUDE - DM_annee_naissance` | Tableau 1, sous-groupes |
| `tranche_age` | < 40 / 40-60 / > 60 ans | Sous-groupes |
| `nb_FDR_HAS` | Somme des 12 FDR HAS | Sous-groupes |
| `cat_FDR_HAS` | 0 / 1-2 / >= 3 | Chi2 de tendance |
| `test_urinaire` | `RAC OR prot_creat OR microalbu OR BU_auto OR proteinurie` | Intermediaire |
| `DFG_evalue` | `DM_DFG_disponible == 1 OR DM_DFG_prescrit_non_realise == 1` | Intermediaire |
| `repere` | `(DFG_evalue AND test_urinaire) OR suivi_nephro` | Definition binaire stricte |
| `rattrape` | `PR_couleur in {orange, rouge} AND repere == 0` | **CJP** |
| `PR_bin` | 0 si vert, 1 si orange/rouge | Kappa, concordance |
| `depistage_niveau` | 5 categories hierarchiques (aucun - nephro) | OS5 |
| `MRC_KDIGO` | `DFG < 60 OR RAC >= 3 mg/mmol` (NA si pas de biologie) | OS6 |

---

## Constantes

| Parametre | Valeur | Description |
|---|---|---|
| `ANNEE_ETUDE` | 2026 | Annee de l'etude (calcul de l'age) |
| `SEUIL_CLINIQUE_CJP` | 0.10 | H0 du test binomial pour CJP (<= 10 %) |
| `NIVEAU_CONFIANCE` | 0.95 | IC a 95 % |
