# =====================================================================
#  D5 — BENCHMARK RECURSIF RESOLU A L'EQUILIBRE (chemin de reference)
#  A coller APRES D3 (qui fournit l'objet `trend` : g_A_rel par secteur).
#  Prerequis : epsilon_baseline <- 0.60 ; D1/D2/D3 relances.
#
#  Principe :
#   - OFFRE : chaque secteur sur son sentier propre -> prix relatifs Baumol
#       p_rel_j(t) = exp(-(g_A_rel_j) * tau)
#   - DEMANDE : a chaque annee, le systeme CES (eps=0.6) REALLOUE les
#       quantites en reponse aux prix -> on RESOUT lambda_j(t).
#   - Ancrage : calibre en 2000 (a prix=1, lambda_B(2000)=lambda_2000) ;
#       agregat reel pine sur phi ; agregat nominal sur le deflateur observe.
#
#  Resout l'incoherence "prix bougent / quantites figees" : ici les
#  quantites de reference repondent aux prix, comme eps l'exige.
#
#  Sorties : benchmark_path (toutes annees), sector_bench_2024 (pour la
#  decomposition et le waterfall). Solveur = celui du 5S (thetaI vecteur).
# =====================================================================
library(dplyr); library(tidyr); library(tibble); library(writexl)

.need <- c("trend","core_data_real_all","tes_results_all","dynamic_benchmark_macro",
           "sector_names","epsilon_baseline","BASE_YEAR","END_YEAR","phi_dyn","dyn_dir")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("D5 — objets manquants : ", paste(.miss,collapse=", "),
                          "\n-> lance 01 puis 02 (et epsilon_baseline<-0.60) d'abord.")
EPS <- epsilon_baseline
if (abs(EPS-0.60)>1e-9) message("ATTENTION : epsilon_baseline = ", EPS, " (attendu 0.60).")
J <- length(sector_names)
b_dir <- file.path(dyn_dir,"D5_BENCHMARK_RECURSIF"); if(!dir.exists(b_dir)) dir.create(b_dir,recursive=TRUE)
norm1 <- function(x){s<-sum(x,na.rm=TRUE); if(!is.finite(s)||abs(s)<1e-12) x else x/s}
ord   <- function(df) df %>% arrange(match(sector, sector_names))
if (!exists("round_numeric_df"))
  round_numeric_df <- function(df,d=6) df %>% mutate(across(where(is.numeric),~round(.x,d)))

# ---- 1. CALIBRATION 2000 (price-purged, a p=1) ---------------------
base0 <- ord(core_data_real_all %>% filter(year==BASE_YEAR))
Omega0<- tes_results_all[[as.character(BASE_YEAR)]]$Omega_V[sector_names,sector_names]
VAreal0 <- base0$VA_real_obs ; Ybar0 <- sum(VAreal0)
lambda0 <- VAreal0/Ybar0
cstar <- sum(base0$C_real)/Ybar0 ; sstar <- sum(base0$I_real)/Ybar0
mu    <- ifelse(VAreal0>0, base0$IC_sector_real/VAreal0, 0)
nx    <- base0$NX_real/Ybar0
thetaC <- norm1(base0$C_real)                       # a p=1, calib CES = parts
thetaI <- norm1(base0$I_real)                        # panier invest commun (vecteur)
thetaV <- apply(Omega0, 2, norm1)

# ---- 2. SOLVEUR CES (eq.26 ; thetaI vecteur ; structure 2000 fixe) -
ces_index<-function(theta,p,eps){if(abs(eps-1)<1e-8) prod(p^theta) else (sum(theta*p^(1-eps)))^(1/(1-eps))}
build_shares<-function(p){
  PC<-ces_index(thetaC,p,EPS); omegaC<-if(abs(EPS-1)<1e-8) thetaC else thetaC*(p/PC)^(1-EPS)
  PV<-numeric(J); omegaV<-matrix(0,J,J)
  for(j in 1:J){PV[j]<-ces_index(thetaV[,j],p,EPS)
    omegaV[,j]<-if(abs(EPS-1)<1e-8) thetaV[,j] else thetaV[,j]*(p/PV[j])^(1-EPS)}
  denI<-sum(p*thetaI); omegaI<-if(denI>0) p*thetaI/denI else rep(0,J)
  list(PC=PC,omegaC=omegaC,PV=PV,omegaV=omegaV,omegaI=omegaI)
}
assemble<-function(p,sh){M<-matrix(0,J,J)
  for(j in 1:J) M[,j]<-sh$omegaI*sstar*p[j]+sh$omegaV[,j]*sh$PV[j]*mu[j]
  list(A=diag(p*(1+mu))-M, b=sh$omegaC*sh$PC*cstar+p*nx)}
# chi calibre a p=1 pour reproduire lambda0
sh1<-build_shares(rep(1,J)); sy1<-assemble(rep(1,J),sh1)
chi <- as.numeric(sy1$A%*%lambda0) - sy1$b
solve_lambda<-function(p){sh<-build_shares(p); sy<-assemble(p,sh)
  norm1(as.numeric(tryCatch(solve(sy$A,sy$b+chi), error=function(e) qr.solve(sy$A,sy$b+chi))))}

# ---- 3. BOUCLE RECURSIVE : lambda_B(t) en reponse aux prix Baumol --
trend_ord <- ord(trend)                              # g_A_rel par secteur (de D3)
gAr <- trend_ord$g_A_rel
years <- BASE_YEAR:END_YEAR
macro <- dynamic_benchmark_macro %>% filter(year %in% years) %>% arrange(year)

path <- list()
for (Y in years){
  tau <- Y - BASE_YEAR
  p_rel_raw <- exp(-gAr*tau)                          # prix relatifs Baumol (offre)
  p_rel <- p_rel_raw/exp(mean(log(p_rel_raw)))        # geomean=1 (le niveau ne change pas lambda)
  lamB  <- solve_lambda(p_rel)                        # DEMANDE : quantites en reponse aux prix
  VAreal_macro <- macro$VA_real_B[macro$year==Y]
  defl <- macro$VA_deflator_B[macro$year==Y]
  # niveau de prix : value-weighted mean(p)=1 -> agregat nominal = deflateur*reel
  p_lvl <- p_rel / sum(lamB*p_rel)
  path[[as.character(Y)]] <- tibble(
    year=Y, sector=sector_names,
    lambda_B=lamB,
    VA_real_B_sector = lamB*VAreal_macro,
    p_B = defl*p_lvl,
    VA_nom_B_sector  = defl*p_lvl*lamB*VAreal_macro
  )
}
benchmark_path <- bind_rows(path)

# ---- 4. CHECKS ----------------------------------------------------
chk_anchor <- benchmark_path %>% filter(year==BASE_YEAR) %>% ord() %>%
  mutate(lambda_2000=lambda0, ecart=lambda_B-lambda_2000)
chk_sum <- benchmark_path %>% group_by(year) %>% summarise(sum_lambda=sum(lambda_B),.groups="drop")
chk_nom <- benchmark_path %>% group_by(year) %>%
  summarise(VA_nom_B=sum(VA_nom_B_sector),.groups="drop") %>%
  left_join(macro %>% select(year, VA_nominal_B), by="year") %>%
  mutate(ecart_nom = VA_nom_B - VA_nominal_B)

checks <- tibble(check=c(
  "epsilon","Max |lambda_B(2000) - lambda_2000| (ancrage)",
  "Max |sum lambda_B - 1|","Max |agregat nominal B - macro| (echelle preservee)"),
  value=c(EPS, max(abs(chk_anchor$ecart)), max(abs(chk_sum$sum_lambda-1)),
          max(abs(chk_nom$ecart_nom))))

# ---- 5. Structural change engendre : lambda 2000 vs 2024 ----------
struct_change <- benchmark_path %>% filter(year %in% c(BASE_YEAR,END_YEAR)) %>%
  select(year,sector,lambda_B) %>% pivot_wider(names_from=year,values_from=lambda_B) %>%
  ord() %>% rename(lambda_2000=2, lambda_2024=3) %>%
  mutate(variation_part = lambda_2024-lambda_2000,
         p_B_2024 = benchmark_path$p_B[benchmark_path$year==END_YEAR][match(sector,sector_names)])

# objet pour la decomposition / waterfall
sector_bench_2024 <- benchmark_path %>% filter(year==END_YEAR) %>% ord()

write_xlsx(list(benchmark_path=benchmark_path, checks=checks,
                structural_change=struct_change, sector_bench_2024=sector_bench_2024),
           file.path(b_dir,"D5_benchmark_recursif_outputs.xlsx"))

cat("\n==================== D5 — CHECKS ====================\n")
print(round_numeric_df(checks,8),n=Inf,width=Inf)
cat("\n==================== D5 — CHANGEMENT STRUCTUREL DU BENCHMARK (lambda 2000 -> 2024) ====================\n")
cat("(les parts reelles ne sont plus figees : elles repondent aux prix Baumol via eps=0.6)\n")
print(round_numeric_df(struct_change,4),n=Inf,width=Inf)
cat("\nObjets pour l'aval : benchmark_path , sector_bench_2024\n")
cat("D5 DONE.\n")
