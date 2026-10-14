# Codebook — Etude PREDICT-R

> Dictionnaire des 67 variables de la base et des variables derivees calculees par les scripts.

**Version** : 2.0 | **Date** : 14/10/2026 | **Auteur** : Alexis TEXIER

L'Annexe 13 du manuscrit reproduit le dictionnaire etabli avant le recueil (version 1.0) ; certaines variables ont ete ajustees au cours de l'analyse. Ce fichier decrit les variables telles qu'elles sont lues et calculees par les scripts du depot.

---

## Structure de la base

- **Fichier** : `data/CRD_PredictRVF_data.xlsx`, feuille `donnees` (non diffusee, cf. README)
- **Dimensions** : 139 patients inclus x 67 variables ; 130 analysables (`DM_repere` renseigne)
- **Identifiant** : `id_patient` (format `PRED001`, `PRED002`...)
- **Codage binaire** : 0 = absent, 1 = present, NA = non applicable ou non renseigne
- **Fenetre du dossier medical** : du 01/01/2025 a la date d'inclusion

---

## Variables de la base (67 colonnes)

### Bloc 1 — Identification (2 variables)

| Variable | Type | Description |
|---|---|---|
| `id_patient` | character | Identifiant pseudonymise (`PRED001` a `PRED200`) |
| `date_inclusion` | Date | Date d'inclusion dans l'etude (fin de la fenetre du dossier medical) |

### Bloc 2 — Donnees Predict-R, autoquestionnaire HEKO (23 variables)

| Variable | Type | Description | Points |
|---|---|---|---|
| `PR_age` | numeric | Age declare (calcule par HEKO) | — |
| `PR_prematurite` | 0/1 | Ne(e) prematurement | 1 |
| `PR_diabete` | 0/1 | Diabetique (Q6) | voir anciennete |
| `PR_diabete_anciennete` | categoriel | Anciennete du diabete : `<5_ans`, `>=5_ans`, NA (Q7) | **2** si < 5 ans, **4** si >= 5 ans |
| `PR_HTA` | 0/1 | HTA traitee ou non | 2 |
| `PR_tabagisme` | 0/1 | Tabagisme quotidien | 1 |
| `PR_dyslipidemie` | 0/1 | Anomalie cholesterol / triglycerides | 1 |
| `PR_MCV` | 0/1 | Maladie cardiovasculaire (AVC, IDM, AOMI) | 1 |
| `PR_insuf_cardiaque` | 0/1 | Insuffisance cardiaque | 1 |
| `PR_poids` | numeric | Poids declare (kg) | — |
| `PR_taille` | numeric | Taille declaree (cm) | — |
| `PR_IMC` | numeric | IMC calcule par HEKO (kg/m2) | 1 si > 30 |
| `PR_maladie_uro` | 0/1 | Pathologie urologique (IU recidivantes, calculs, pyelonephrites, nephrectomie, malformation) | **3** |
| `PR_AINS` | 0/1 | AINS plus d'une fois par semaine | 1 |
| `PR_expo_PCI` | 0/1 | Scanner avec injection de produit de contraste iode plus d'une fois par an | 1 |
| `PR_expo_radiotherapie` | 0/1 | Radiotherapie abdominale | 1 |
| `PR_BU_realisee` | 0/1 | Bandelette urinaire optionnelle realisee | — |
| `PR_BU_hematurie` | 0/1/NA | Hematurie a la bandelette (NA si non realisee) | **6** si positive |
| `PR_BU_proteinurie` | 0/1/NA | Proteinurie a la bandelette (NA si non realisee) | **6** si positive |
| `PR_medecin_traitant` | 0/1 | Patient ayant un medecin traitant | — |
| `PR_transmission_MG` | 0/1 | Accepte la transmission du resultat a son medecin (Resultats 12.1) | — |
| `PR_score` | numeric | Score total Predict-R (calcule par HEKO, bareme de l'Annexe 3) | — |
| `PR_couleur` | factor | Niveau de risque : `vert` (< 4), `orange` (4-5), `rouge` (>= 6) | — |

### Bloc 3 — Sociodemographiques, dossier medical (2 variables)

| Variable | Type | Description |
|---|---|---|
| `DM_age` | numeric | Age releve dans le dossier medical (etat civil) |
| `DM_sexe` | factor | Sexe (`M` / `F`) |

### Bloc 4 — Facteurs de risque, dossier medical (17 variables)

| Variable | Type | Description | FDR HAS |
|---|---|---|---|
| `DM_HTA` | 0/1 | HTA (ATCD code ou antihypertenseur) | oui |
| `DM_diabete` | 0/1 | Diabete (ATCD code ou antidiabetique) | oui |
| `DM_obesite` | 0/1 | Obesite (ATCD code ou IMC >= 30) | oui |
| `DM_MCV` | 0/1 | Maladie cardiovasculaire | oui |
| `DM_insuf_cardiaque` | 0/1 | Insuffisance cardiaque | oui |
| `DM_maladie_auto_immune` | 0/1 | Maladie auto-immune / systemique | oui |
| `DM_patho_uro` | 0/1 | Pathologie urologique | oui |
| `DM_ATCD_fam_nephro` | 0/1 | ATCD familial de nephropathie | oui |
| `DM_ATCD_nephro_aigue` | 0/1 | Nephropathie aigue passee | oui |
| `DM_nephrotoxiques` | 0/1 | Medicaments nephrotoxiques chroniques | oui |
| `DM_expo_PCI` | 0/1 | Exposition aux produits de contraste iodes | oui |
| `DM_expo_radiotherapie` | 0/1 | Radiotherapie abdomino-pelvienne | oui |
| `DM_expo_toxiques_pro` | 0/1 | Toxiques professionnels (Pb, Cd, Hg) | oui |
| `DM_maladie_rare_renale` | 0/1 | Maladie renale rare (polykystose, etc.) | non |
| `DM_tabagisme` | 0/1 | Tabagisme actif | non (comparaison avec Predict-R) |
| `DM_dyslipidemie` | 0/1 | Dyslipidemie (ATCD ou hypolipemiant) | non (comparaison avec Predict-R) |
| `DM_prematurite` | 0/1 | Prematurite | non (comparaison avec Predict-R) |

### Bloc 5 — Biologie renale, dossier medical (10 variables)

| Variable | Type | Description |
|---|---|---|
| `DM_DFG_disponible` | 0/1 | Resultat de DFG (creatininemie CKD-EPI) disponible ; exclusif avec `DM_DFG_prescrit_non_realise` |
| `DM_DFG_valeur` | numeric | Valeur du DFG (mL/min/1,73 m2) |
| `DM_DFG_date` | Date | Date du DFG |
| `DM_DFG_prescrit_non_realise` | 0/1 | Bilan complet (DFG + test urinaire) prescrit mais non realise |
| `DM_RAC` | 0/1 | RAC disponible (PRED134 recode a 1, cf. README) |
| `DM_RAC_valeur` | numeric | Valeur du RAC (mg/mmol) |
| `DM_prot_creat` | 0/1 | Rapport proteinurie/creatininurie disponible |
| `DM_microalbuminurie` | 0/1 | Microalbuminurie disponible |
| `DM_BU_automate` | 0/1 | Bandelette urinaire lue par automate disponible |
| `DM_proteinurie` | 0/1 | Proteinurie (24 h ou qualitative au laboratoire) disponible |

### Bloc 6 — Suivi nephrologique et MRC (5 variables)

| Variable | Type | Description |
|---|---|---|
| `DM_suivi_nephro` | 0/1 | Suivi nephrologique documente dans la fenetre (compte rendu, courrier d'adressage ou avis) |
| `DM_suivi_nephro_date` | Date | Date du dernier contact avec le nephrologue |
| `DM_MRC_connue` | 0/1 | MRC connue (ATCD code, DFG confirme, suivi nephrologique pour MRC, albuminurie persistante) ; PRED001, PRED071, PRED129 recodes a 1, cf. README |
| `DM_MRC_stade` | factor | Stade KDIGO (G1-G5, dialyse, greffe, inconnu) |
| `DM_MRC_critere` | factor | Critere ayant defini la MRC connue : `ATCD_code`, `DFG_confirme`, `nephro_MRC`, `alb_persistante` |

### Bloc 7 — Traitements, dossier medical (4 variables)

| Variable | Type | Description |
|---|---|---|
| `DM_ttt_antihypertenseur` | 0/1 | Antihypertenseur en cours (deja integre dans `DM_HTA`) |
| `DM_ttt_antidiabetique` | 0/1 | Antidiabetique en cours (deja integre dans `DM_diabete`) |
| `DM_ttt_AINS` | 0/1 | AINS chroniques ou repetes |
| `DM_ttt_nephroprotecteur` | 0/1 | IEC / ARA2 / iSGLT2 en cours |

### Bloc 8 — Complementaires (4 variables)

| Variable | Type | Description |
|---|---|---|
| `modalite_completion` | factor | Modalite de completion : `AUTO_SMART`, `AUTO_PC`, `AIDE_SMART`, `AIDE_PC`, `INV` |
| `retrait_non_opposition` | 0/1 | Retrait de la non-opposition apres l'inclusion (critere d'exclusion) |
| `notes_investigateur` | character | Observations libres (cas atypiques, discordances, 2e dosage) |
| `DM_repere` | 0/1 | Patient repere dans le dossier medical : (DFG disponible ET au moins un test urinaire) OU bilan complet prescrit OU suivi nephrologique ; vide si le dossier n'a pas pu etre analyse |

---

## Variables derivees (calculees dans les scripts)

| Variable | Formule | Script | Usage |
|---|---|---|---|
| `PR_bin` | 0 si vert, 1 si orange ou rouge | scripts d'analyse | Classement a risque, kappa |
| `rattrape` | `PR_bin == 1` ET `DM_repere == 0` | `analyse_principale.R`, `tableaux_5_6_sous_groupes.R`, `resultats_8_3_8_4_a_posteriori.R` | **Critere de jugement principal** |
| `test_urinaire` | `DM_RAC` OU `DM_prot_creat` OU `DM_microalbuminurie` OU `DM_BU_automate` OU `DM_proteinurie` | `analyse_principale.R`, `tableau_7_figure_5_examens_renaux.R` | Intermediaire |
| `DFG_evalue` | `DM_DFG_disponible` OU `DM_DFG_prescrit_non_realise` (+ OU `DM_DFG_valeur` renseigne dans `tableau_7_figure_5_examens_renaux.R`) | idem | Intermediaire |
| `repere_calcule` | (`DFG_evalue` ET `test_urinaire`) OU `DM_suivi_nephro` | `analyse_principale.R` | Controle de coherence avec `DM_repere` |
| `fdr_expo_contraste` | `DM_expo_PCI` OU `DM_expo_radiotherapie` | `analyse_principale.R`, `tableaux_5_6_sous_groupes.R`, `tableau_7_figure_5_examens_renaux.R` | Compte comme un seul FDR HAS |
| `nb_FDR_HAS` | Somme des 12 FDR HAS du dossier medical : diabete, HTA, MCV, insuffisance cardiaque, obesite, maladie auto-immune, pathologie urologique, ATCD familial, nephropathie aigue, nephrotoxiques, `fdr_expo_contraste`, toxiques professionnels | idem | Comparateur HAS, sous-groupes |
| `au_moins_1_FDR` | `nb_FDR_HAS >= 1` | `analyse_principale.R` | Comparateur HAS (Resultats 7) |
| `cat_FDR_HAS` | 0 / 1-2 / >= 3 | idem | Tableau 6, Figure 5, tendance de Cochran-Armitage |
| `tranche_age` | < 40 / 40-60 / > 60 ans (sur `DM_age`) | `tableaux_5_6_sous_groupes.R` | Tableau 5 |
| `depistage_niveau` | 5 categories exclusives : suivi nephrologique, complet (DFG + urinaire), DFG seul, test urinaire seul, aucun | `tableau_7_figure_5_examens_renaux.R` | Tableau 7 |
| `depistage_complet` | DFG + test urinaire uniquement (definition du manuscrit) | `tableau_7_figure_5_examens_renaux.R` | Tableau 7, Figure 5 |
| `biologie_dispo` | `DM_DFG_valeur` OU `DM_RAC_valeur` renseigne | `tableaux_8_9_performances.R` | Sous-groupe des performances (n = 90) |
| `MRC_KDIGO` | DFG < 60 OU RAC >= 3 mg/mmol (NA si pas de biologie) | `tableaux_8_9_performances.R` | Standard de reference (Tableaux 8 et 9) |
| `pts_*`, `score_recalcule` | Points de chaque item selon le bareme de l'Annexe 3 | `resultats_8_3_8_4_a_posteriori.R` | Items ayant conduit au classement des rattrapes |

---

## Constantes

| Parametre | Valeur | Ou | Description |
|---|---|---|---|
| Seuil du test binomial | 0,10 | `analyse_principale.R` (`binom.test(p = 0.10)`) | H0 du critere de jugement principal ; robustesse a 0,20 a 0,50 |
| Niveau de confiance | 0,95 | tous | IC de Wilson (proportions), de Clopper-Pearson (performances) |
| `SEUIL_ORANGE` | 4 | `resultats_8_3_8_4_a_posteriori.R` | Score a partir duquel un patient est classe a risque |
| `flux` | 908, 175, 13, 123, 81, 155, 11, 144, 143, 4 | `analyse_principale.R` (section 1.6) | Effectifs des exports Maiia et HEKO (Figure 2), non presents dans la base |
