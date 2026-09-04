# ============================================================
# D14 UNIFIE — SENTIER REEL DU PLAN DE MARCHE (methode unique)
# ------------------------------------------------------------
# A lancer APRES D7 (memes objets d'entree que l'ancien D14).
#
# METHODE (une seule, documentee) :
#  1. REINDUSTRIALISATION par REALLOCATION SUR LE STOCK : chaque annee, la
#     structure des parts converge vers la cible lambda* d'une fraction KAPPA
#     de l'ecart restant :  lam <- lam + KAPPA*(lambda* - lam).
#     Ce mecanisme est INDEPENDANT du rythme de croissance (la croissance fixe
#     le NIVEAU, la reallocation fixe la REPARTITION). KAPPA est CALE pour que
#     la part industrielle referme 95% de son ecart a la cible en AUTONOMIE_ANS
#     annees (10 ans -> 2037). La penetration importee de l'industrie revient
#     alors a son niveau 2000.
#  2. CROISSANCE en DEUX PHASES :
#     - Phase 1 (reconquete) : rythme DECIDE en crescendo G_START->G_END
#       (1,5% -> 2%), tant que le plein emploi n'est pas atteint.
#     - Phase 2 (croisiere) : des le plein emploi, la croissance = POTENTIEL
#       REEL = productivite + accroissement population active (+ immigration).
#       On ne cree pas d'emplois qui n'existent pas.
#  3. EMPLOI : en phase 1, la croissance puise dans le reservoir de chomeurs
#     jusqu'au plancher frictionnel. En phase 2, l'emploi suit la population
#     active (regimes INSEE) + immigration eventuelle.
#  4. PENETRATION IMPORTEE : par secteur, glisse de son niveau 2024 (m24)
#     vers son niveau 2000 (m_star) au rythme du comblement du gap.
#  5. SOLDE COMMERCIAL (NX) : imports = penetration (decroissante) x demande ;
#     converge vers 0 a mesure de la reindustrialisation.
#
# HYPOTHESES CLES (cochables) :
#  - PRODUCTIVITE : "basse" (0,5%) ou "haute" (crescendo -> PROD_CIBLE).
#  - IMMIGRATION : renfort d'actifs en croisiere (0 si non active).
#  - Population active : regimes INSEE (+0,1% / 0% / -0,15%).
#  - lambda*_public abaisse pour stabiliser l'emploi public.
# ============================================================
library(dplyr); library(tidyr); library(tibble); library(writexl)

.need <- c("lambda_star","d","dVA_real","sector_names","core_data_real_all","END_YEAR")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("D14 — objets manquants : ", paste(.miss,collapse=", "),"\n-> lance 05_cible_lambda_star.R d'abord.")
if (!exists("dyn_dir")) dyn_dir <- getwd()
b_dir <- file.path(dyn_dir,"D14_DYNAMIQUE"); if(!dir.exists(b_dir)) dir.create(b_dir,recursive=TRUE)
if (!exists("round_numeric_df")) round_numeric_df<-function(df,dg=6) df %>% mutate(across(where(is.numeric),~round(.x,dg)))

# ============================================================
# CONFIG — LES LEVIERS
# ============================================================
HORIZON        <- 34            # 2027 -> 2060
G_START        <- 0.0150        # croissance reconquete, depart
G_END          <- 0.0200        # croissance reconquete, plateau
RAMP_YEARS     <- 4             # duree du crescendo
AUTONOMIE_ANS  <- 10            # cible : autonomie industrielle en 10 ans (-> 2037)
PLEIN_EMPLOI_ANS <- 10          # cible : plein emploi en 10 ans (-> 2037), aligne sur l'autonomie
ANNEE0         <- 2027

RESERVOIR      <- 4500          # reservoir de chomeurs 2027 (k)
FRICTIONNEL    <- 1500          # plancher de chomage frictionnel (k)

PRODUCTIVITE   <- "haute"       # "basse" (0,5%) | "haute" (crescendo -> PROD_CIBLE)
PROD_BASE      <- 0.005         # productivite plancher
PROD_CIBLE     <- 0.010         # cible si "haute" (1%)
IMMIGRATION    <- FALSE         # renfort d'actifs en croisiere
IMMIG_SUPP     <- 0.003         # +0,3 pt/an d'actifs si IMMIGRATION=TRUE
IMMIG_DEBUT    <- 2037          # debut de l'appoint migratoire

LAMBDA_PUBLIC_CIBLE <- 0.167    # cible part VA publique (stabilise l'emploi public)

NOM_SCENARIO   <- "S3_reformes_haute"

# ============================================================
# PREPARATION
# ============================================================
industrie4 <- c("Agriculture and food","Traditional manufacturing",
                "Chemicals and materials","Machinery, equipment and transport equipment")
services4  <- c("Information and communication","Finance and real estate",
                "Business services","Trade, transport and hospitality")
grp <- ifelse(sector_names %in% industrie4,"Industrie a relever",
              ifelse(sector_names %in% services4,"Services qui drivent","Socle intermediaire"))
is_ind <- sector_names %in% industrie4

d <- d %>% arrange(match(sector,sector_names))
Y0<-sum(d$VA_real); L0<-sum(d$L)
VA0<-d$VA_real

# cible lambda* (avec public abaisse pour stabiliser l'emploi public)
lam_star <- lambda_star
IDX_PUBLIC <- which(sector_names == "Public, education and health-social services")
delta_pub <- lam_star[IDX_PUBLIC] - LAMBDA_PUBLIC_CIBLE
lam_star[IDX_PUBLIC] <- LAMBDA_PUBLIC_CIBLE
.autres <- setdiff(seq_along(lam_star), IDX_PUBLIC)
lam_star[.autres] <- lam_star[.autres] + delta_pub * lam_star[.autres]/sum(lam_star[.autres])
lam_star <- lam_star/sum(lam_star)
# demande domestique et exports 2024 (reels), pour le NX endogene
dd <- core_data_real_all %>% filter(year==END_YEAR) %>% arrange(match(sector,sector_names))
X0   <- if ("X_real" %in% names(dd)) dd$X_real else rep(0,length(sector_names))
Mabs0<- if ("M_real" %in% names(dd)) dd$M_real else rep(0,length(sector_names))
NX0 <- sum(X0)-sum(Mabs0)

# ---- fonctions de croissance et de demographie ----
# population active : regimes INSEE
n_pop_of <- function(annee) {
  if (annee <= 2036)      0.001
  else if (annee <= 2040) 0.000
  else                   -0.0015
}
# productivite : basse (fixe) ou haute (crescendo jusqu'au plein emploi puis maintenue)
prod_of <- function(annee, annee_PE) {
  if (PRODUCTIVITE == "basse") return(PROD_BASE)
  fin <- if (is.na(annee_PE)) 2034 else annee_PE
  if (annee <= fin) PROD_BASE + (PROD_CIBLE - PROD_BASE) * (annee - ANNEE0) / max(1, fin - ANNEE0)
  else PROD_CIBLE
}
# rythme de reconquete (crescendo)
g_reconquete <- function(t) if(t<=RAMP_YEARS) G_START+(G_END-G_START)*(t-1)/max(1,RAMP_YEARS-1) else G_END

# ---- calage de KAPPA pour autonomie ~ AUTONOMIE_ANS ----
# METHODE : reallocation sur le STOCK. Chaque annee, on referme une fraction
# KAPPA de l'ecart de PART a la cible :  lam <- lam + KAPPA*(lam* - lam).
# Ce mecanisme est INDEPENDANT du rythme de croissance : la reindustrialisation
# (changement de structure) suit son calendrier propre, la croissance le sien.
# KAPPA est cale pour que la part industrielle referme 95% de son ecart a la
# cible en AUTONOMIE_ANS annees.
lam0_v  <- VA0/sum(VA0)
pi0     <- sum(lam0_v[is_ind])          # part industrie au depart
pistar  <- sum(lam_star[is_ind])        # part industrie cible
cal <- function(KAP){
  lam<-lam0_v; yr<-NA
  for (t in 1:HORIZON){
    lam<-lam+KAP*(lam_star-lam); lam<-lam/sum(lam)
    if (is.na(yr) && (sum(lam[is_ind])-pi0)/(pistar-pi0)>=0.95) yr<-t
  }
  if(is.na(yr)) 999 else yr
}
grid<-seq(0.05,0.60,by=0.01); yrs<-sapply(grid,cal)
KAPPA<-grid[which.min(abs(yrs-AUTONOMIE_ANS))]

# ---- calage de ETA pour plein emploi ~ AUTONOMIE_ANS (meme horizon) ----
# METHODE : l'emploi net = ETA x (croissance - productivite) x emploi. ETA<1 car
# une partie de la croissance non-productivite resorbe d'abord le SOUS-EMPLOI
# (temps partiel subi, halo) avant de creer de l'emploi net. ETA est cale pour
# que le reservoir de chomeurs atteigne le plancher frictionnel en PLEIN_EMPLOI_ANS
# annees, dans le scenario de reference (productivite haute). En hypothese basse,
# le plein emploi arrive un peu plus tot (la croissance cree plus d'emploi) : c'est
# une variante assumee, non un recalage.
cal_eta <- function(eta){
  U<-RESERVOIR; L<-L0; yr<-NA
  for (t in 1:HORIZON){
    annee<-ANNEE0+t-1
    if (U<=FRICTIONNEL+1e-6){ if(is.na(yr)) yr<-t; break }
    g<-g_reconquete(t)
    p<-if(PRODUCTIVITE=="basse") PROD_BASE else prod_of(annee, NA)
    dL<-eta*max(0,g-p)*L
    if (U-dL<FRICTIONNEL) dL<-U-FRICTIONNEL
    U<-U-dL; L<-L+dL
    if (U<=FRICTIONNEL+1e-6 && is.na(yr)) yr<-t
  }
  if(is.na(yr)) 999 else yr
}
grid_eta<-seq(0.50,1.00,by=0.01); yrs_eta<-sapply(grid_eta,cal_eta)
ETA<-grid_eta[which.min(abs(yrs_eta-PLEIN_EMPLOI_ANS))]

# ============================================================
# SIMULATION UNIFIEE
# ============================================================
Yt<-Y0; Lt<-L0; Ut<-RESERVOIR; VA<-VA0; rows<-list()
immig_cum<-0; annee_PE<-NA
for (t in 1:HORIZON){
  annee <- ANNEE0+t-1
  plein_emploi <- (Ut <= FRICTIONNEL + 1e-6)
  if (plein_emploi && is.na(annee_PE)) annee_PE <- annee
  
  # --- CROISSANCE : phase 1 (reconquete) ou phase 2 (croisiere) ---
  if (!plein_emploi) {
    g <- g_reconquete(t); phase <- 1
  } else {
    im <- if (IMMIGRATION && annee>=IMMIG_DEBUT) IMMIG_SUPP else 0
    g <- prod_of(annee, annee_PE) + n_pop_of(annee) + im; phase <- 2
  }
  
  # --- REINDUSTRIALISATION : reallocation sur le STOCK (independante de g) ---
  # la structure lam converge vers lam* a taux KAPPA ; la croissance g fixe
  # le NIVEAU, la reallocation fixe la REPARTITION.
  lam <- VA/Yt
  lam <- lam + KAPPA*(lam_star - lam); lam <- lam/sum(lam)
  Yt  <- Yt*(1+g)
  VA  <- lam*Yt
  
  # progres de reindustrialisation (avancement de la part industrie vers la cible)
  prog_ind <- (sum(lam[is_ind]) - pi0)/(pistar - pi0)
  prog_ind <- min(1, max(0, prog_ind))
  # penetration importee : suit l'avancement de la reindustrialisation
  m_t <- d$m24 - prog_ind*(d$m24 - d$m_star)
  pen_ind <- 100*sum((m_t*VA)[is_ind])/sum(VA[is_ind])
  
  # --- EMPLOI ---
  if (!plein_emploi) {
    # phase 1 : l'emploi ne capte que la croissance NON-PRODUCTIVITE, et seule
    # une fraction ETA en devient de l'emploi NET (le reste resorbe d'abord le
    # sous-emploi / temps partiel subi). dL = ETA*(g - productivite)*Lt.
    dL <- ETA * max(0, g - prod_of(annee, annee_PE)) * Lt
    if (Ut-dL < FRICTIONNEL) dL <- Ut-FRICTIONNEL
    Ut <- Ut-dL; Lt <- Lt+dL
  } else {
    # phase 2 : l'emploi suit la population active (regimes) + immigration
    im <- if (IMMIGRATION && annee>=IMMIG_DEBUT) IMMIG_SUPP else 0
    Lt <- Lt*(1 + n_pop_of(annee) + im)
  }
  
  # solde commercial endogene
  NX_t <- NX0 * (1 - prog_ind)
  
  lam<-VA/Yt
  rows[[t]]<-tibble(annee=annee, phase=phase,
                    g_pct=100*g, croissance_cum_pct=100*(Yt/Y0-1),
                    productivite_pct=100*prod_of(annee, annee_PE),
                    reindus_pct=100*prog_ind, penetration_ind_pct=pen_ind,
                    part_ind_pct=100*sum(lam[is_ind]),
                    emploi_total_k=Lt, chomage_k=Ut,
                    immig_cum_k=immig_cum, NX_real=NX_t)
  # detail sectoriel de l'annee
  if (!exists("sect_rows")) sect_rows <- list()
  sect_rows[[t]] <- tibble(annee=annee, sector=sector_names, groupe=grp,
                           lambda_pct=100*lam, lambda_star_pct=100*lam_star,
                           ecart_cible_pt=100*(lam-lam_star),
                           penetration_pct=100*m_t, reindus_pct=100*prog_ind,
                           VA_real=VA, part_emploi_pct=100*lam)  # emploi ~ prorata VA
}
sentier<-bind_rows(rows)
sectoriel<-bind_rows(sect_rows)

gd<-function(cond){i<-which(cond)[1]; if(is.na(i)) NA else sentier$annee[i]}
dates<-tibble(evenement=c("Autonomie industrielle (penetration ~2000)","Plein-emploi"),
              annee=c(gd(sentier$reindus_pct>=95), gd(sentier$chomage_k<=FRICTIONNEL+1)))

write_xlsx(list(sentier=sentier, sectoriel=sectoriel, dates=dates,
                calage=tibble(KAPPA=KAPPA, autonomie_ans=cal(KAPPA),
                              ETA=ETA, plein_emploi_ans=cal_eta(ETA))),
           file.path(b_dir, paste0("D14_unifie_", NOM_SCENARIO, ".xlsx")))

# COPIE au nom NEUTRE FIXE : le rebouclage (D15) lit toujours ce fichier-la,
# quel que soit le scenario. Pas besoin de changer le nom cote D15.
write_xlsx(list(sentier=sentier, sectoriel=sectoriel, dates=dates,
                calage=tibble(KAPPA=KAPPA, autonomie_ans=cal(KAPPA),
                              ETA=ETA, plein_emploi_ans=cal_eta(ETA),
                              scenario=NOM_SCENARIO)),
           file.path(b_dir, "D14_sentier_courant.xlsx"))

cat("\n===== D14 UNIFIE — CALAGE =====\n")
cat("KAPPA =", KAPPA, "-> autonomie industrielle en ~", cal(KAPPA), "ans (",ANNEE0+cal(KAPPA)-1,")\n")
cat("ETA   =", ETA, "-> plein emploi en ~", cal_eta(ETA), "ans (",ANNEE0+cal_eta(ETA)-1,")\n")
cat("Scenario :", NOM_SCENARIO, "| productivite :", PRODUCTIVITE,
    if(PRODUCTIVITE=="haute") paste0("(cible ",100*PROD_CIBLE,"%)") else "(0,5%)",
    "| immigration :", IMMIGRATION, "\n")
cat("\n===== D14 UNIFIE — SENTIER REEL =====\n")
print(round_numeric_df(sentier,1), n=Inf, width=Inf)
cat("\n===== DATES CLES =====\n"); print(dates, n=Inf, width=Inf)
cat("\nPenetration industrie : depart ->", round(tail(sentier$penetration_ind_pct,1),1),
    "% (cible 2000)\n")
cat("Plein-emploi :", dates$annee[2], "| Autonomie :", dates$annee[1], "\n")
cat("Sorties : D14_unifie_", NOM_SCENARIO, ".xlsx  +  D14_sentier_courant.xlsx (nom neutre pour le D15)\n", sep="")
cat("D14 UNIFIE DONE.\n")
