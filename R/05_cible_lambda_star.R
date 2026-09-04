# ============================================================
# D7 — CIBLE lambda* : AUTONOMIE INDUSTRIELLE (substitution pure d'imports)
# A lancer APRES D6.
#
# PRINCIPE (structure seule, independante du chomage) :
#   - On ramene la penetration importee a 2000 : m*_j = min(m_2024, m_2000).
#   - Substitution PURE : la production domestique remplace les imports
#     rapatries, SANS expansion generale de la demande.
#       injection Df_j = M_j * (m24_j - m*_j)/m24_j   (imports rapatries du bien j)
#   - Propagation RESEAU (inverse de Leontief) : produire +1 chez soi tire
#     des intrants chez les autres -> emplois DIRECTS + INDIRECTS.
#   - lambda* = parts de VA apres substitution (l'industrie remonte).
#   - Le chomage (reservoir ~4,5 M ETP) est traite SEPAREMENT : on compare
#     seulement, et on donne la version bornee au reservoir si depassement.
# ============================================================
library(dplyr); library(tidyr); library(tibble); library(writexl)

.need <- c("core_data_real_all","tes_results_all","sector_names","END_YEAR","BASE_YEAR","dyn_dir")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("D7 — objets manquants : ", paste(.miss,collapse=", "))
ord <- function(df) df %>% arrange(match(sector, sector_names))
if (!exists("round_numeric_df"))
  round_numeric_df <- function(df,d=6) df %>% mutate(across(where(is.numeric),~round(.x,d)))
J <- length(sector_names); yr <- END_YEAR
b_dir <- file.path(dyn_dir,"D7_LAMBDA_STAR"); if(!dir.exists(b_dir)) dir.create(b_dir,recursive=TRUE)

RESERVOIR_ETP <- 4500      # <<< reservoir A+B+C 2024 en ETP (milliers), editable

# ---- 1. Donnees 2024 + penetration 2000 ---------------------------
d24 <- ord(core_data_real_all %>% filter(year==yr)) %>%
  transmute(sector, M = M_nom_bn, m24 = import_penetration,
            VA_real = VA_real_obs, VA_nom = VA_nominal_obs, p = p_model, L = employment_obs)
m00 <- ord(core_data_real_all %>% filter(year==BASE_YEAR)) %>%
  transmute(sector, m00 = import_penetration)
d <- d24 %>% left_join(m00, by="sector") %>% ord() %>%
  mutate(m_star = pmin(m24, m00))     # retour a 2000, jamais au-dela
# Exemption possible (secteur non substituable) :
# d$m_star[d$sector=="Extractive, energy and utilities"] <- d$m24[...]

# ---- 2. Reseau input-output 2024 : A, va_coef, Leontief -----------
V12 <- tes_results_all[[as.character(yr)]]$V12[sector_names, sector_names]/1000
Qv  <- tes_results_all[[as.character(yr)]]$vectors
Q   <- Qv$Q_nom_TES[match(sector_names, Qv$sector)]/1000
A   <- sweep(V12, 2, ifelse(Q>0,Q,NA), "/"); A[!is.finite(A)] <- 0
va_coef <- pmax(0, 1 - colSums(A))
L_inv   <- solve(diag(J) - A)

# ---- 3. Injection = imports rapatries, propagation reseau ---------
Df    <- ifelse(d$m24>0, d$M*(d$m24 - d$m_star)/d$m24, 0)   # imports du bien j rapatries (Md€ nom)
dx    <- as.numeric(L_inv %*% Df)                           # production totale induite (dir+indir)
dVA_nom  <- va_coef * dx
dVA_real <- dVA_nom / d$p
dL       <- ifelse(d$VA_real>0, d$L/d$VA_real, 0) * dVA_real  # emplois (milliers), prod. observee

# ---- 4. lambda* (autonomie pleine) --------------------------------
VA_real_star <- d$VA_real + dVA_real
lambda_obs   <- d$VA_real / sum(d$VA_real)
lambda_star  <- VA_real_star / sum(VA_real_star)
jobs_full    <- sum(dL)

# ---- 5. Version bornee au reservoir (si depassement) --------------
scale_res <- min(1, RESERVOIR_ETP / jobs_full)
dVA_real_r <- dVA_real * scale_res
dL_r       <- dL * scale_res
VA_real_star_r <- d$VA_real + dVA_real_r
lambda_star_r  <- VA_real_star_r / sum(VA_real_star_r)

# ---- 6. Tables ----------------------------------------------------
target <- tibble(
  sector = sector_names, m24_pct=100*d$m24, m00_pct=100*d$m00, m_star_pct=100*d$m_star,
  lambda_obs_pct=100*lambda_obs, lambda_star_pct=100*lambda_star,
  delta_pt=100*(lambda_star-lambda_obs), emplois_k=dL
) %>% arrange(desc(delta_pt))

# horizon pour resorber le reservoir (emplois plafonnes) au rythme g
horizon <- tibble(rythme=c("2,0 %/an","2,25 %/an","2,5 %/an"), g=c(0.020,0.0225,0.025)) %>%
  mutate(annees_reservoir = NA_real_)   # rempli en Bloc 1 (dynamique) ; ici indicatif

# ---- 7. CHECKS ----------------------------------------------------
NX_surplus <- sum(Df)   # imports en moins = amelioration de NX (surplus a absorber en transition)
checks <- tibble(check=c(
  "Emplois autonomie PLEINE (milliers, dir+indir)","Reservoir ETP (milliers)",
  "Facteur d'echelle si borne au reservoir","Emplois version bornee (milliers)",
  "Somme lambda* pleine (=1)","Min lambda* (>=0)",
  "Imports rapatries = amelioration NX (Md€)","VA reelle suppl. pleine (Md€)"),
  value=c(jobs_full, RESERVOIR_ETP, scale_res, sum(dL_r),
          sum(lambda_star), min(lambda_star), NX_surplus, sum(dVA_real)))

write_xlsx(list(cible=target, checks=checks,
                lambda_bornee=tibble(sector=sector_names,
                   lambda_star_reservoir_pct=100*lambda_star_r, emplois_bornes_k=dL_r),
                detail=d),
           file.path(b_dir,"D7_lambda_star_outputs.xlsx"))

# ---- 8. PRINTS ----------------------------------------------------
cat("\n==================== D7 — CHECKS ====================\n")
print(round_numeric_df(checks,3), n=Inf, width=Inf)
cat("\n==================== D7 — CIBLE lambda* AUTONOMIE PLEINE (tri par delta) ====================\n")
cat("m*_j = penetration importee cible (retour 2000) ; delta_pt = glissement de part de VA\n")
cat("emplois_k = emplois dir+indir impliques (milliers). Industrie doit MONTER (delta>0)\n\n")
print(round_numeric_df(target,3), n=Inf, width=Inf)
cat("\nEmplois autonomie pleine :", round(jobs_full,0), "milliers | Reservoir :", RESERVOIR_ETP,
    "| facteur d'echelle :", round(scale_res,3), "\n")
cat("Imports rapatries (amelioration NX) :", round(NX_surplus,1),
    "Md€ (surplus a absorber par la demande en transition)\n")
cat("\nObjets : target, lambda_star (pleine), lambda_star_r (bornee reservoir), d, dL\n")
cat("D7 DONE.\n")
