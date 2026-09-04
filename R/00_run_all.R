# ============================================================
#  L'Audit de l'économie française — Chaîne de reproduction
#  Atef Khelifi
# ------------------------------------------------------------
#  Ce script lance l'intégralité de la chaîne, dans l'ordre.
#  Les scripts partagent leurs objets via l'environnement global :
#  ils DOIVENT être exécutés dans cet ordre, sans vider la mémoire.
#
#  ORDRE (les dépendances imposent D7/cible AVANT D4/sentier) :
#    01  Données + modèle (socle : lit les fichiers INSEE)
#    02  Capital, productivité, prix Baumol
#    03  Benchmark récursif à l'équilibre (le « double »)
#    04  Décomposition des écarts (prix × structure)
#    05  Cible de structure lambda*      <-- AVANT 06
#    06  Sentier dynamique du plan        (a besoin de lambda*)
#    07  Projection financière & scénarios (dette, déficit)
#    08  Plan de marche (ventilation sectorielle)
# ============================================================

# --- Où sont les données brutes INSEE (dossier data/ par défaut) ---
# Sur-couche possible : Sys.setenv(AUDIT_DATA_DIR = "chemin/vers/data")

root <- getwd()
Rdir <- file.path(root, "R")

run <- function(f){
  cat("\n=============================================\n")
  cat(" >>> ", f, "\n")
  cat("=============================================\n")
  source(file.path(Rdir, f), encoding = "UTF-8")
}

run("01_data_et_modele.R")
run("02_capital_productivite_baumol.R")
run("03_benchmark_equilibre.R")
run("04_decomposition_ecarts.R")
run("05_cible_lambda_star.R")        # cible lambda*  (AVANT le sentier)
run("06_sentier_dynamique.R")        # sentier        (utilise lambda*)
run("07_projection_scenarios.R")     # dette / déficit / scénarios
run("08_plan_de_marche.R")           # ventilation sectorielle

cat("\n\nTERMINÉ. Résultats dans le dossier DYNAMIC_PANEL_* et sous-dossiers.\n")
