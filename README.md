# L'Audit de l'économie française — Modèle, code & données

Code source reproductible de l'ouvrage **《 L'Audit de l'économie française — Diagnostic, causes et plan de redressement 》** (Atef Khelifi, 2026) et de l'article *World Prices and Business Cycles in a Small Open Input–Output Economy* (Economic Modelling, 2023).

Le modèle est un **équilibre général multisectoriel à structure entrées-sorties** (12 secteurs), calibré sur les données de l'INSEE (2000–2024). Il construit une trajectoire de référence (« le double »), décompose les écarts observés (prix × structure), puis projette les finances publiques selon cinq scénarios de réforme.

---

## 1. Prérequis

- **R ≥ 4.2**
- Packages : `dplyr`, `tidyr`, `tibble`, `readxl`, `writexl`, `stringr`, `stringi`, `purrr`, `forcats`, `ggplot2`, `scales`

```r
install.packages(c("dplyr","tidyr","tibble","readxl","writexl",
                   "stringr","stringi","purrr","forcats","ggplot2","scales"))
```

## 2. Installation des données

Déposez les fichiers de données INSEE (voir §4) dans le dossier **`data/`** à la racine du projet.
Vous pouvez aussi pointer vers un autre dossier :

```r
Sys.setenv(AUDIT_DATA_DIR = "C:/chemin/vers/mes/donnees")
```

## 3. Lancer toute la chaîne

```r
setwd("chemin/vers/audit_modele")
source("R/00_run_all.R", encoding = "UTF-8")
```

Les scripts partagent leurs objets en mémoire : **exécutez-les dans l'ordre, sans vider l'environnement.**

### Ordre d'exécution (imposé par les dépendances)

| # | Script | Rôle | Produit |
|---|--------|------|---------|
| 01 | `01_data_et_modele.R` | Lecture des données INSEE, construction du panel, TES, gaps | `core_data_real_all`, `tes_results_all`, … |
| 02 | `02_capital_productivite_baumol.R` | Capital (inventaire permanent), PGF, prix Baumol | `trend` |
| 03 | `03_benchmark_equilibre.R` | Benchmark récursif à l'équilibre (« le double ») | `sector_bench_2024` |
| 04 | `04_decomposition_ecarts.R` | Décomposition exacte prix × structure | décomposition |
| 05 | `05_cible_lambda_star.R` | **Cible de structure λ\*** (substitution d'imports) | `lambda_star`, `d`, `target` |
| 06 | `06_sentier_dynamique.R` | Sentier du plan année par année | `D14_sentier_courant.xlsx` |
| 07 | `07_projection_scenarios.R` | Dette, déficit, 5 scénarios | `PLAN_*.xlsx`, `RECAP_*.xlsx` |
| 08 | `08_plan_de_marche.R` | Ventilation sectorielle | `PLAN_MARCHE_*.xlsx` |

> ⚠️ **Le script 05 (cible λ\*) doit précéder le script 06 (sentier)** : le sentier a besoin de `lambda_star`. Un lancement direct de 06 renvoie une erreur « objets manquants → lance 05 (D7) ».

## 4. Données requises (à placer dans `data/`)

### a) Tableaux entrées-sorties, un fichier par année
Nommés **`TES_38_YYYY.xlsx`** (nomenclature 38 secteurs), de 2000 à 2024 :

```
TES_38_2000.xlsx, TES_38_2001.xlsx, …, TES_38_2024.xlsx
```

Source : INSEE — Tableaux entrées-sorties (comptes nationaux annuels).

### b) Séries agrégées et sectorielles (un fichier chacune)

| Fichier | Contenu | Source INSEE |
|---------|---------|--------------|
| `Emploi_ALL.xlsx` | Emploi par branche (milliers) | Comptes nationaux — emploi |
| `VA_brute_nominale_ALL.xlsx` | Valeur ajoutée brute nominale par branche | Comptes nationaux |
| `Indices_ALL.xlsx` | Indices de prix / déflateurs sectoriels | Comptes nationaux |
| `Pop_Active_ALL.xlsx` | Population active (projections) | INSEE — Projections de population active |
| `FBCF_branche_ALL.xlsx` | FBCF par branche (depuis 1978), pour le capital | Comptes nationaux — FBCF |

> Les fichiers `DYNAMIC_D1_*`, `DYNAMIC_D2_*`, `D3_*`, `D5_*`, `D6_*`, `D7_*`, `PLAN_*`, `SENTIER_*`, `RECAP_*` sont des **sorties générées** par la chaîne — ne pas les fournir.

## 5. Principaux résultats produits

- `RECAP_PLAN_UNIQUE.xlsx` — dette et déficit 2060 pour les 5 scénarios
- `PLAN_1..5_*.xlsx` — trajectoires financières détaillées par scénario
- `SENTIER_*.xlsx` — sentiers réels (croissance, emploi, réindustrialisation)
- `PLAN_MARCHE_*.xlsx` — ventilation sectorielle du plan
- `DYNAMIC_D2_final_sector_IC_benchmark_and_gaps.xlsx` — diagnostic sectoriel complet

## 6. Paramètres clés (verrouillés)

α = 0,30 (part du capital) · δ = 0,05 (dépréciation) · ε = 0,60 (élasticité de substitution CES) · taux de prélèvement 43 % · base 2024 (PIB, dette, déficit INSEE).

---

## Licence & citation

© 2026 Atef Khelifi. Pour citer ce travail :

> Khelifi, A. (2026). *L'Audit de l'économie française — Diagnostic, causes et plan de redressement.* Books on Demand. ISBN 978-2-322-63358-6.

Contact : voir la page de l'auteur.
