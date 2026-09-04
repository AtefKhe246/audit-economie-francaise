# ============================================================
# D8 — PLAN DE MARCHE SECTORIEL (la "boussole")
# ALIGNE sur LANCEUR_PLAN_UNIQUE (plan unique gradue, prod basse 0,5%).
# A lancer APRES D1..D7 (memes objets : lambda_star / target, d, sector_names)
# ET apres avoir execute le LANCEUR (pour disposer du sentier cible).
#
# CONVENTIONS REPRISES DU LANCEUR (coherence imperative) :
#   - PIB total croit au rythme g du sentier cible (S5, prod basse 0,5%).
#   - SECTEUR PUBLIC (G11 = "Public, education and health-social services") :
#       * part cible FORCEE a LAMBDA_PUBLIC_CIBLE = 0,167 ;
#       * VA publique croit a PHI_PUBLIC (0,50) x croissance HORS PROD (g_hp),
#         PAS au rythme general ; le delta de part libere est redistribue
#         au prorata sur les 11 secteurs marchands.
#   - PIB MARCHAND = PIB total - VA publique ; les 11 marchands se partagent
#     le marchand selon leurs parts glissees lambda_j(t).
#   - "Marche vers les lambda* de PE" : glissement lambda_obs -> lambda* au
#     rythme de reindustrialisation du sentier (reindus_pct), 2027->2037.
#
# SORTIES (dyn_dir/D8_PLAN_MARCHE) :
#   - PLAN_MARCHE_GLOBAL_STRUCTUREL.xlsx
#   - PLAN_MARCHE_GLOBAL_FINANCIER.xlsx
#   - PLAN_MARCHE_SECTORIEL.xlsx (feuille "tous_secteurs" + 12 feuilles)
# ============================================================
library(dplyr); library(tidyr); library(tibble); library(writexl); library(readxl)

# ---- 0. Garde-fous ------------------------------------------------
.need <- c("sector_names","target","d")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("Plan de marche — lancer 01 à 07 d'abord. Manquants : ",
                          paste(.miss, collapse=", "))
J <- length(sector_names)
b_dir <- file.path(if (exists("dyn_dir")) dyn_dir else ".", "D8_PLAN_MARCHE")
if (!dir.exists(b_dir)) dir.create(b_dir, recursive=TRUE)

# ---- 1. CONSTANTES (identiques au LANCEUR) ------------------------
PIB_2024        <- 2919.9
ANNEE_BASE      <- 2024
ANNEE0          <- 2027
ANNEE_FIN_PM    <- 2060
PROD_BASE       <- 0.005
PHI_PUBLIC      <- 0.50
LAMBDA_PUBLIC_CIBLE <- 0.167
TAUX_PO         <- 0.43
DELTA_AMORT     <- 0.08
MOLLE_G         <- 0.009
SEC_PUBLIC      <- "Public, education and health-social services"

# ---- 2. Sentier macro (scenario cible S5) -------------------------
# ATTENTION : on lit le FICHIER (pas l'objet 'sentier' en memoire, qui est le
# DERNIER scenario calcule par le LANCEUR et peut differer : calendrier sans
# annee de mise en place, colonnes differentes). Le fichier
# SENTIER_coordination_prodbonus.xlsx porte le bon calendrier (2027 = mise en
# place a 0,9%) et la colonne de croissance hors productivite.
# On peut forcer le chemin : SENTIER_PATH<-"...".
.find_file <- function(pattern){
  if (exists("SENTIER_PATH") && grepl("SENTIER",pattern) && file.exists(SENTIER_PATH)) return(SENTIER_PATH)
  if (exists("PLAN5_PATH")   && grepl("PLAN_5",pattern) && file.exists(PLAN5_PATH))   return(PLAN5_PATH)
  roots <- unique(c(getwd(), if (exists("dyn_dir")) dyn_dir else NULL,
                    if (exists("out_dir")) out_dir else NULL, "/mnt/project","."))
  for (r0 in roots){
    hit <- list.files(r0, pattern=pattern, full.names=TRUE, recursive=TRUE)
    hit <- hit[file.exists(hit)]
    if (length(hit)>0) return(hit[1])
  }
  NULL
}
sfile <- .find_file("SENTIER_coordination_prodbonus\\.xlsx$")
if (is.null(sfile)) stop("D8 — SENTIER_coordination_prodbonus.xlsx introuvable. ",
                         "Definir SENTIER_PATH<-\"chemin\" puis relancer.")
sraw <- readxl::read_excel(sfile, sheet="sentier")
# Colonne de croissance hors productivite : accepte plusieurs noms possibles.
ghp_col <- intersect(c("g_horsprod_pct","g_hors_prod_pct","g_hp_pct"), names(sraw))
if (length(ghp_col)==0){
  # Reconstruction de secours : hors prod = g - productivite (si dispo).
  if (all(c("g_pct","productivite_pct") %in% names(sraw))){
    sraw$g_horsprod_pct <- sraw$g_pct - sraw$productivite_pct
    ghp_col <- "g_horsprod_pct"
    message("D8 — colonne g_horsprod_pct reconstruite = g_pct - productivite_pct.")
  } else stop("D8 — impossible de determiner la croissance hors productivite dans le sentier.")
}
sent <- sraw %>%
  transmute(annee, g=g_pct/100, g_hp=.data[[ghp_col[1]]]/100,
            reindus=reindus_pct/100, emploi_total_k, chomage_k,
            lam_pub_lanceur = if ("lam_pub" %in% names(sraw)) lam_pub else NA_real_,
            va_pub_lanceur  = if ("va_pub_Mds" %in% names(sraw)) va_pub_Mds else NA_real_) %>%
  filter(annee>=ANNEE0, annee<=ANNEE_FIN_PM) %>% arrange(annee)
USE_LANCEUR_PUB <- all(is.finite(sent$va_pub_lanceur))  # utiliser la VA pub du LANCEUR ?

# ---- 3. Etat sectoriel 2024 (depuis D7) ---------------------------
IDX_PUB <- which(sector_names==SEC_PUBLIC)
if (length(IDX_PUB)!=1) stop("D8 — secteur public introuvable dans sector_names.")
base <- d %>%
  transmute(sector, VA_real0=VA_real, VA_nom0=VA_nom, p0=p, L0=L, M0=M, m24, m_star) %>%
  left_join(target %>% transmute(sector,
                                 lam_obs=lambda_obs_pct/100, lam_star_raw=lambda_star_pct/100),
            by="sector") %>%
  arrange(match(sector, sector_names))

lam_star <- base$lam_star_raw
delta_pub <- lam_star[IDX_PUB] - LAMBDA_PUBLIC_CIBLE
lam_star[IDX_PUB] <- LAMBDA_PUBLIC_CIBLE
march <- setdiff(seq_len(J), IDX_PUB)
w <- lam_star[march]/sum(lam_star[march])
lam_star[march] <- lam_star[march] + delta_pub*w
base$lam_star <- lam_star

VA_real_tot0 <- sum(base$VA_real0)
VApub0       <- base$VA_real0[IDX_PUB]

# ---- 4. Facteurs cumules : PIB total et VA publique ---------------
fac_pib <- function(t){
  f<-1
  for (yr in 2025:2026) f<-f*(1+MOLLE_G)
  for (yr in ANNEE0:t) f<-f*(1+sent$g[sent$annee==yr])
  f
}
fac_vapub <- function(t){
  f<-1
  for (yr in 2025:2026) f<-f*(1+PHI_PUBLIC*(MOLLE_G-PROD_BASE))
  for (yr in ANNEE0:t) f<-f*(1+PHI_PUBLIC*sent$g_hp[sent$annee==yr])
  f
}

# ---- 5. Trajectoire annee par annee -------------------------------
# Deux phases, comme le LANCEUR :
#  * FEUILLE DE ROUTE (jusqu'au PE) : marche des parts marchandes vers lam*
#    au rythme de reindustrialisation ; public via lam_pub du LANCEUR (cible 16,7%).
#  * BGP (apres PE) : parts marchandes FIGEES a leur niveau du PE (tous au rythme
#    n+g) ; public via lam_pub du LANCEUR (qui reflue au rythme demographique).
# Le PE est reconnu comme la 1re annee ou reindus cesse de croitre fortement OU
# ou le chomage atteint son plancher. On le repere sur le sentier (phase / chomage).
# Ici : PE = 1re annee ou reindus >= max(reindus)-0.5pt (plateau) ; robuste.
reindus_max <- max(sent$reindus)
annee_PE <- min(sent$annee[sent$reindus >= reindus_max - 0.005*1])  # ~plateau reindus
# fallback : si chomage_k dispo, PE = 1re annee au plancher
if ("chomage_k" %in% names(sent)) {
  chplanch <- min(sent$chomage_k)
  aPE2 <- min(sent$annee[sent$chomage_k <= chplanch + 1e-6])
  annee_PE <- min(annee_PE, aPE2)
}
message("D8 — plein emploi detecte en : ", annee_PE)

# parts marchandes au PE (figees ensuite)
lam_m_PE <- NULL

rows <- list()
for (t in ANNEE0:ANNEE_FIN_PM){
  rd  <- sent$reindus[sent$annee==t]
  PIB_reel_tot <- VA_real_tot0 * fac_pib(t)
  
  if (USE_LANCEUR_PUB){
    lam_pub_t <- sent$lam_pub_lanceur[sent$annee==t]   # part publique du LANCEUR
    VApub_t   <- lam_pub_t * PIB_reel_tot              # VA publique reelle coherente
  } else {
    VApub_t   <- VApub0 * fac_vapub(t)
  }
  PIB_march_t  <- PIB_reel_tot - VApub_t
  
  if (t <= annee_PE){
    # FEUILLE DE ROUTE : marche vers les cibles
    lam_t <- base$lam_obs + rd*(base$lam_star - base$lam_obs)
    lam_m <- lam_t[march]/sum(lam_t[march])
    if (t == annee_PE) lam_m_PE <<- lam_m       # on memorise les parts au PE
  } else {
    # BGP : parts marchandes FIGEES a leur niveau du PE
    lam_m <- if (!is.null(lam_m_PE)) lam_m_PE else {
      lam_t <- base$lam_obs + rd*(base$lam_star - base$lam_obs); lam_t[march]/sum(lam_t[march])
    }
  }
  
  VA_real_j <- numeric(J)
  VA_real_j[march]   <- lam_m * PIB_march_t
  VA_real_j[IDX_PUB] <- VApub_t
  
  # Emploi — DEUX régimes distincts (principe du modèle) :
  #  * SECTEUR PUBLIC : "suit l'activité, PAS l'efficience". Emploi ∝ VA (aucune
  #    productivité) -> avant comme apres le PE, l'emploi public suit sa VA.
  #  * SECTEURS MARCHANDS : productivité du travail érodée par le gain cumulé
  #    (à VA donnée, il faut moins d'emploi chaque année) -> L ∝ VA/(1+g_A)^t.
  fac_prod   <- (1+PROD_BASE)^(t-ANNEE_BASE)          # marchands seulement
  prod_lab0  <- ifelse(base$L0>0, base$VA_real0/base$L0, NA)
  prod_lab_t <- prod_lab0
  prod_lab_t[march] <- prod_lab0[march]*fac_prod       # érosion : marchands
  # public : prod_lab_t[IDX_PUB] reste = prod_lab0 (pas d'érosion)
  L_j <- ifelse(is.finite(prod_lab_t) & prod_lab_t>0, VA_real_j/prod_lab_t, 0)
  K_ratio <- ifelse(base$VA_real0>0, VA_real_j/base$VA_real0, 1)
  m_t <- base$m24 + rd*(base$m_star - base$m24)
  
  rows[[as.character(t)]] <- tibble(
    annee=t, sector=base$sector, is_public=(seq_len(J)==IDX_PUB),
    VA_real=VA_real_j, L=L_j, K_ratio=K_ratio,
    m_pct=100*m_t, lam_pct=100*VA_real_j/PIB_reel_tot)
}
traj <- bind_rows(rows)

# ---- 5b. CALAGE de l'emploi MARCHAND sur le sentier -----------------
# Le public est laissé tel quel (son emploi suit sa VA, cohérence LANCEUR).
# On cale l'emploi MARCHAND sur (emploi_total - emploi_public) du sentier.
emp_tot_cible <- setNames(sent$emploi_total_k, sent$annee)
traj <- traj %>% group_by(annee) %>%
  mutate(
    .Lpub   = sum(L[is_public]),
    .cible_march = emp_tot_cible[as.character(first(annee))] - .Lpub,
    .Lmarch = sum(L[!is_public]),
    .scale  = ifelse(.Lmarch>0, .cible_march/.Lmarch, 1),
    L = ifelse(is_public, L, L*.scale)
  ) %>% ungroup() %>% select(-.Lpub,-.cible_march,-.Lmarch,-.scale)

# ---- 6. Taux de croissance + investissement -----------------------
traj <- traj %>% group_by(sector) %>% arrange(annee) %>%
  mutate(g_VA_pct = 100*(VA_real/lag(VA_real)-1),
         g_L_pct  = 100*(L/lag(L)-1),
         dK       = K_ratio - lag(K_ratio),
         invest_idx = dK + DELTA_AMORT*lag(K_ratio)) %>%
  ungroup()

# ---- 7. GLOBAL STRUCTUREL (texte) ---------------------------------
global_struct <- sent %>%
  transmute(Annee=annee, `Croissance %`=round(g*100,2),
            `Emploi total (M)`=round(emploi_total_k/1000,2),
            `Chomage (M)`=round(chomage_k/1000,2),
            `Reindustrialisation %`=round(reindus*100,1))
plan5 <- NULL
p5file <- .find_file("PLAN_5_productivite\\.xlsx$")
if (!is.null(p5file)) plan5 <- readxl::read_excel(p5file, sheet="rebouclage")
if (!is.null(plan5)){
  gg <- plan5 %>% filter(annee>=ANNEE0, annee<=ANNEE_FIN_PM) %>%
    transmute(Annee=annee, `Deficit % PIB`=round(deficit_pib_pct,1),
              `Dette % PIB`=round(dette_pib_pct,1))
  global_struct <- left_join(global_struct, gg, by="Annee")
}

# ---- 8. GLOBAL FINANCIER (texte) ----------------------------------
# Colonnes reelles de PLAN_5 (LANCEUR) : recettes_new_Mds, coord_cout_Mds,
# dep_public_Mds, demo_Mds, charge_Mds, solde_glob_Mds, deficit_pib_pct.
if (!is.null(plan5)){
  p5 <- plan5 %>% filter(annee>=ANNEE0, annee<=ANNEE_FIN_PM) %>% arrange(annee)
  global_fin <- p5 %>%
    transmute(Annee=annee,
              `Recettes nouv. (Md)` = round(recettes_new_Mds,1),
              `Cout CC (Md)`        = round(ifelse(is.na(coord_cout_Mds),0,coord_cout_Mds),1),
              `Depenses demo (Md)`  = round(demo_Mds,1),
              `Depenses publ. (Md)` = round(dep_public_Mds,1),
              `Charge dette (Md)`   = round(charge_Mds,1),
              `Solde global (Md)`   = round(solde_glob_Mds,1),
              `Deficit % PIB`       = round(deficit_pib_pct,1))
} else global_fin <- tibble(Annee=ANNEE0:ANNEE_FIN_PM)

# ---- 9. TABLEAUX SECTORIELS (annexe) ------------------------------
mk_sector_tab <- function(sec){
  traj %>% filter(sector==sec) %>% arrange(annee) %>%
    transmute(Annee=annee,
              `VA reelle (Md)`   = round(VA_real,1),
              `VA croiss. %`     = round(g_VA_pct,2),
              `Emploi (k)`       = round(L,0),
              `Emploi croiss. %` = round(g_L_pct,2),
              `Invest. (indice)` = round(invest_idx,3),
              `Penetr. import %` = round(m_pct,1))
}
sector_sheets <- setNames(lapply(sector_names, mk_sector_tab),
                          substr(make.unique(sector_names),1,28))
tous <- traj %>% transmute(Annee=annee, Secteur=sector,
                           VA_Md=round(VA_real,1), VA_g=round(g_VA_pct,2),
                           Emploi_k=round(L,0), Emploi_g=round(g_L_pct,2),
                           Import_pct=round(m_pct,1), Part_VA_pct=round(lam_pct,2))

# ---- 10. Ecriture -------------------------------------------------
write_xlsx(list(global_structurel=global_struct),
           file.path(b_dir,"PLAN_MARCHE_GLOBAL_STRUCTUREL.xlsx"))
write_xlsx(list(global_financier=global_fin),
           file.path(b_dir,"PLAN_MARCHE_GLOBAL_FINANCIER.xlsx"))
write_xlsx(c(list(tous_secteurs=tous), sector_sheets),
           file.path(b_dir,"PLAN_MARCHE_SECTORIEL.xlsx"))

# ---- 11. CONTROLES DE COHERENCE (vs LANCEUR) ----------------------
cat("\n============== D8 — PLAN DE MARCHE (aligne LANCEUR) ==============\n")
cat("Source VA publique :", if (USE_LANCEUR_PUB) "lam_pub du LANCEUR (coherence exacte)"
    else "reconstruite (PHI_PUBLIC x g_hp)", "\n")
cat("GLOBAL STRUCTUREL :\n"); print(as.data.frame(global_struct), row.names=FALSE)
cat("\nGLOBAL FINANCIER :\n"); print(as.data.frame(global_fin), row.names=FALSE)
chk <- traj %>% group_by(annee) %>%
  summarise(PIB_reconstruit=sum(VA_real),
            VA_publique=sum(VA_real[is_public]),
            PIB_marchand=sum(VA_real[!is_public]), .groups="drop") %>%
  mutate(PIB_attendu=VA_real_tot0*sapply(annee,fac_pib),
         ecart_pct=100*(PIB_reconstruit/PIB_attendu-1))
cat("\nCONTROLE PIB (reconstruit vs attendu) :\n")
print(as.data.frame(chk %>% mutate(across(where(is.numeric),~round(.x,2)))), row.names=FALSE)
.pub <- function(a) round(traj$lam_pct[traj$annee==a & traj$is_public],2)
.emp <- function(a) round(sum(traj$L[traj$annee==a])/1000,2)
.sent <- function(a) round(sent$emploi_total_k[sent$annee==a]/1000,2)
cat("\nPart publique 2024 :", round(100*base$lam_obs[IDX_PUB],2),
    "% -> 2037 :", .pub(2037), "% -> 2060 :", .pub(2060), "% (cible 16,7%)\n")
cat("Emploi total (somme secteurs, M) : 2037 =", .emp(2037), "(sentier", .sent(2037), ") ",
    "| 2060 =", .emp(2060), "(sentier", .sent(2060), ")\n")
cat("\nSorties dans :", b_dir, "\nD8 DONE.\n")