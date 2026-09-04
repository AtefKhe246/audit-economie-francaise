# =====================================================================
#  D6 — DECOMPOSITION DU GAP DE VA NOMINALE SUR LE BENCHMARK D5
#  A coller APRES D5. Consomme sector_bench_2024 (benchmark recursif
#  resolu a l'equilibre : prix Baumol cote offre, quantites repondant
#  cote demande). Remplace l'ancien benchmark "cable" de D3.
#
#  Identite exacte (additive) :
#     gap_j = VA_nom_obs_j - VA_nom_B_j
#           = prix_residuel_j + structure_j
#     prix_residuel_j = (p_obs_j - p_B_j) * VA_real_obs_j
#         -> rencherissement AU-DELA de ce que la productivite justifie
#            (le prix-productivite est deja dans p_B, donc dans le benchmark)
#     structure_j     = p_B_j * (VA_real_obs_j - VA_real_B_j)
#         -> ecart de volume vs le benchmark COHERENT (lambda_B repond aux prix)
#
#  Sorties : sector_gaps_2024 , group_gaps_2024 (pour le waterfall).
# =====================================================================
library(dplyr); library(tidyr); library(tibble); library(writexl)

.need <- c("sector_bench_2024","core_data_real_all","sector_names",
           "END_YEAR","dyn_dir","dynamic_observed_macro","dynamic_benchmark_macro")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("D6 — objets manquants : ", paste(.miss,collapse=", "),
                          "\n-> lance 03_benchmark_equilibre.R d'abord.")
ord <- function(df) df %>% arrange(match(sector, sector_names))
if (!exists("round_numeric_df"))
  round_numeric_df <- function(df,d=6) df %>% mutate(across(where(is.numeric),~round(.x,d)))
b_dir <- file.path(dyn_dir,"D6_DECOMPOSITION"); if(!dir.exists(b_dir)) dir.create(b_dir,recursive=TRUE)

# ---- 1. Joindre observe (END_YEAR) et benchmark D5 ----------------
obs <- ord(core_data_real_all %>% filter(year==END_YEAR)) %>%
  transmute(sector, p_obs=p_model, VA_real_obs, VA_nom_obs=VA_nominal_obs)
bench <- ord(sector_bench_2024) %>%
  transmute(sector, p_B, VA_real_B=VA_real_B_sector, VA_nom_B=VA_nom_B_sector)
d <- obs %>% inner_join(bench, by="sector") %>% ord()

# ---- 2. Decomposition exacte a deux termes ------------------------
d <- d %>% mutate(
  gap_VA_nominal = VA_nom_obs - VA_nom_B,
  prix_residuel  = (p_obs - p_B) * VA_real_obs,
  structure      = gap_VA_nominal - prix_residuel,          # = p_B*(VA_real_obs - VA_real_B)
  residu_additivite = gap_VA_nominal - (prix_residuel + structure)
)

# ---- 3. Regroupement diagnostique (taxonomie validee) -------------
grp <- tibble(sector=sector_names) %>% mutate(diagnostic_group=case_when(
  sector %in% c("Agriculture and food","Extractive, energy and utilities",
                "Traditional manufacturing","Chemicals and materials",
                "Machinery, equipment and transport equipment") ~ "Exposed productive sectors",
  sector %in% c("Trade, transport and hospitality","Construction") ~ "Fragile absorbers",
  sector %in% c("Information and communication","Business services") ~ "Productive internal services",
  TRUE ~ "Asset and institutional sectors"))     # Finance, Public, Cultural/personal

sector_gaps_2024 <- d %>% left_join(grp, by="sector") %>%
  select(sector, diagnostic_group, p_obs, p_B, VA_real_obs, VA_real_B,
         VA_nom_obs, VA_nom_B, gap_VA_nominal, prix_residuel, structure)

group_gaps_2024 <- sector_gaps_2024 %>% group_by(diagnostic_group) %>%
  summarise(gap_VA_nominal=sum(gap_VA_nominal),
            prix_residuel=sum(prix_residuel),
            structure=sum(structure), .groups="drop") %>%
  arrange(desc(gap_VA_nominal))

# ---- 4. CHECKS ----------------------------------------------------
gap_macro <- (dynamic_observed_macro$VA_nominal_obs[dynamic_observed_macro$year==END_YEAR] -
                dynamic_benchmark_macro$VA_nominal_B[dynamic_benchmark_macro$year==END_YEAR])
checks <- tibble(check=c(
  "Max |residu additivite| (gap = prix + structure)",
  "Total gap nominal (somme secteurs)",
  "Gap macro de reference (obs - benchmark)",
  "Ecart total secteurs vs macro"),
  value=c(max(abs(d$residu_additivite)),
          sum(d$gap_VA_nominal), gap_macro,
          sum(d$gap_VA_nominal)-gap_macro))

write_xlsx(list(sector_gaps_2024=sector_gaps_2024, group_gaps_2024=group_gaps_2024,
                checks=checks), file.path(b_dir,"D6_decomposition_outputs.xlsx"))

cat("\n==================== D6 — CHECKS ====================\n")
print(round_numeric_df(checks,6),n=Inf,width=Inf)
cat("\n==================== D6 — DECOMPOSITION PAR SECTEUR (benchmark D5) ====================\n")
print(sector_gaps_2024 %>% select(sector,gap_VA_nominal,prix_residuel,structure) %>%
        arrange(desc(gap_VA_nominal)) %>% round_numeric_df(2), n=Inf, width=Inf)
cat("\n==================== D6 — GAPS PAR GROUPE (pour le waterfall) ====================\n")
print(round_numeric_df(group_gaps_2024,2), n=Inf, width=Inf)
cat("\nObjets pour le waterfall : sector_gaps_2024 , group_gaps_2024\n")
cat("D6 DONE.\n")