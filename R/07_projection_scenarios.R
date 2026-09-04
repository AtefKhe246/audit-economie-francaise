# =====================================================================
#  PLAN UNIQUE GRADUE — 5 COURBES EN CASCADE (productivite basse 0,5%)
# ---------------------------------------------------------------------
#  A lancer APRES D1..D7 (memes objets que D14 : lambda_star, d, ...).
#
#  RECIT : les reformes budgetaires seules (COR, mesurette) sont sur
#  trajectoire MOLLE (0,9%/an, pas de reconquete) -> elles aident mais
#  ne renversent pas. La COORDINATION (courbe 4) allume le moteur de
#  croissance (reconquete + reindustrialisation + plein emploi).
#
#  LES 5 COURBES (cumulatives) :
#   1. Sans reformes        : molle 0,9%           (montree ->2045)
#   2. + COR                : molle + recul 64 + sous-index 0,5%/an
#   3. + Mesurette          : molle + volontaires mi-temps
#   4. + Coordination       : RECONQUETE (croissance) + cout coordination
#   5. + Productivite post-PE : reconquete + 0,5%/an apres plein emploi
#
# =====================================================================
#  DOCUMENTATION DES HYPOTHESES (capitalisation — a jour aout 2026)
# ---------------------------------------------------------------------
#  ANCRAGE : 2024 (deficit 168,6 Md = 5,77% PIB ; dette 3300 Md = 113%).
#            Simulation 2027->2060. Le deficit de depart 2027 ~5,6-5,9%
#            se raccorde au 2024 observe.
#
#  REGIMES DE CROISSANCE (sentier coordination, prod basse 0,5%) :
#    - molle sans plan      : 0,9%/an
#    - reconquete 2027-2030 : 1,5% -> 2,0% (crescendo, croissance decidee)
#    - plateau 2030-~2035   : 2,0% (resorption chomage 4,5M -> 1,5M)
#    - bascule ~2036        : plein emploi atteint (reservoir vide)
#    - BGP post-PE          : g = productivite + croissance pop active
#         prod 0,5% : ~0,57% (->2040) puis ~0,33% ; avec bonus +0,5% : ~1,07% / ~0,83%
#
#  POPULATION ACTIVE (FIGE — INSEE, Projections de population active
#    2022-2070, parue le 30 juin 2022, scenario central) :
#      2027-2040 = +0,07%/an (+20 000 actifs/an)
#      2040-2050 = -0,17%/an (-50 000 actifs/an)
#      2050-2060 = -0,12%/an (-30 a -40 000 actifs/an)
#    (Population TOTALE, pour le texte : INSEE Projections 2026-2070,
#     juin 2026, scenario central = -3,2 M habitants entre 2026 et 2070.)
#
#  COR (courbes 2-5) : sous-indexation -0,5%/an sur 2027-2030 = 2 pts
#    cumules (cible exacte du Comite de suivi des retraites, avis juil.
#    2026) + recul d'age 64 (supprime le surcout de non-recul 13 Md/an).
#    Modele en REEL -> la sous-index prix se traduit direct en -X% reel.
#
#  MESURETTE (courbes 3-5) : volontaires 63-67 ans, mi-temps sur
#    certificat medical. Vivier 3,5M x 12% = 420k a maturite (montee 5 ans).
#    Recette = 43% x salaire mi-temps (22,5k) ~ 4,06 Md/an a maturite.
#    Economie pension = 30% suspendus x pension 17k ~ 2,14 Md/an.
#    TOTAL ~6,2 Md/an a maturite (2031). Idee narrative : produit -> familles.
#
#  COORDINATION (courbes 4-5 uniquement) : allegement cotis patronales
#    sur CREATION NETTE de staff, -50% annees 1-3 puis -30% annees 4-6 de
#    chaque cohorte (s'auto-ventile). Base 42% cotis x 45k -> 9450 €/emploi
#    (1-3), 5670 € (4-6). = AIDE AU TRAVAIL (~130 Md cumules).
#    + AIDE AU CAPITAL (symetrique) : on applique a la part CAPITAL du PIB
#    (ALPHA_K=30%) le MEME taux de rabais que celui accorde a la part TRAVAIL
#    (ALPHA_L=70%). Garantit K/AL neutre (pas de biais travail/capital, pas
#    d'investissement qui "tarde"). Suit la meme dynamique que l'aide travail
#    (~56 Md cumules). Remplace l'ancien suramortissement forfaitaire.
#    COUT TOTAL coordination ~185 Md sur 2027-2043.
#
#  PRODUCTIVITE (courbe 5) : +0,5%/an de productivite APRES le plein
#    emploi (remede au desequilibre entrees-sorties de la vie active).
#    Message : si ce +0,5% n'est pas au RDV -> le compenser par l'immigration.
#
#  REBOUCLAGE : recettes = 43% (taux PO) du surcroit de PIB. Secteur public
#    hors ce taux. Deficit_pib = -solde_glob/PIB. Identites :
#    solde_prim = recettes - demo - 168,6 ; solde_glob = solde_prim - (charge-58).
#
#  RESULTATS (RECAP, dette/deficit 2060) : 1=276%/5,8 ; 2=248%/4,4 ;
#    3=240%/4,0 ; 4=155%/2,5 ; 5=66%/-4,5. Message : reformes budgetaires
#    seules ne renversent pas ; la COORDINATION (courbe 4) fait basculer.
# =====================================================================
library(dplyr); library(tidyr); library(tibble); library(writexl)

.need <- c("lambda_star","d","dVA_real","sector_names","core_data_real_all","END_YEAR")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss) > 0) stop("Objets manquants : ", paste(.miss,collapse=", "),
                            " -> lance 01 à 05 puis relance.")
if (!exists("dyn_dir")) dyn_dir <- getwd()
OUT_DIR <- file.path(dyn_dir, "CASCADE_OUT")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive=TRUE)

# Part publique 2024 REELLE issue du modele (secteur "Public, education, health").
# Sert a caler la VA publique de reference dans tous les sentiers.
.IDX_PUB <- which(sector_names=="Public, education and health-social services")
.d_ord   <- d %>% arrange(match(sector,sector_names))
PART_PUB_2024 <- (.d_ord$VA_real / sum(.d_ord$VA_real))[.IDX_PUB]

# =====================================================================
#  CONSTANTES
# =====================================================================
# -- D14 (reel) --
HORIZON<-34; G_START<-0.0150; G_END<-0.0200; RAMP_YEARS<-4
AUTONOMIE_ANS<-10; PLEIN_EMPLOI_ANS<-10; ANNEE0<-2027; ANNEE_FIN<-2060
RESERVOIR<-4500; FRICTIONNEL<-1500
PROD_BASE<-0.005; PROD_CIBLE<-0.010; LAMBDA_PUBLIC_CIBLE<-0.167
MOLLE_G<-0.009                          # croissance molle 0,9%/an (sans coordination)
# -- D15 (financier de base) --
PIB_2024<-2919.9; DEFICIT_2024<-168.6; DETTE_2024<-3300.0; CHARGE_2024<-58.0
# -- Charge de la dette (parametres sources) --
# TAUX_ANCIEN = taux apparent 2024 : force par la base = charge/dette = 58/3300 = 1,76%
#   (coherent avec les comptes INSEE 2024 ; FIPECO donne un taux apparent de 2,0% en 2025).
# TAUX_NOUVEAU = 2,91% : cout moyen des emissions a moyen-long terme en 2024.
#   Source : Agence France Tresor, rapport d'activite 2024 (juillet 2025).
# MATURITE = 8,5 ans : duree de vie moyenne de la dette negociable, avril 2026.
#   Source : FIPECO / AFT (avril 2026).
TAUX_ANCIEN<-0.0176; TAUX_NOUVEAU<-0.0291; MATURITE<-8.5
TAUX_PO<-0.43
# -- Retraites (base sourcee) --
# 13,9% du PIB 2024 = 406 Md, tous regimes legalement obligatoires + FSV.
# Source : COR, "Evolutions et perspectives des retraites en France", rapport
# annuel juin 2025 (13,9% du PIB en 2024).
RETR_BASE_2024<-406.0
# -- Sante liee au vieillissement (base sourcee) --
# Effet vieillissement : +0,7 pt de PIB entre 2024 et 2060 (scenario central).
# Source : Commission europeenne, "2024 Ageing Report", Institutional Paper 279,
# avril 2024 (depenses publiques de sante, France). Profil : ~3/4 de l'effet
# acquis en 2040 (arrivee du baby-boom aux grands ages, "papy-boom"), 1/4 ensuite
# (gains d'esperance de vie), conformement a France Strategie (2017).
SANTE_EFFET_TOTAL<-0.007*PIB_2024       # +0,7 pt de PIB ~ 20,4 Md a horizon 2060
sante_surcout<-function(annee){
  if(annee<=2040) SANTE_EFFET_TOTAL*0.75*(annee-2024)/16
  else            SANTE_EFFET_TOTAL*(0.75+0.25*(annee-2040)/20)
}
# -- COR --
SOUSINDEX<-0.005                        # 0,5%/an sur 2027-2030 = 2 pts (cible CSR)
COUT_NON_REFORME<-13.0                  # surcout de non-recul d'age (supprime si COR)
# -- PVA : poursuite volontaire d'activite (volontaires 63-67, mi-temps sur certificat medical) --
# Vivier : 5 generations 63-67 ans ~ 4,5 M personnes (INSEE, ~0,9 M/generation
#   autour de 60-67 ans, pyramide des ages 2024), dont on retient 3,5 M mobilisables
#   (net des personnes deja en emploi ou en incapacite) -- hypothese conservatrice.
# Participation : 10% (hypothese assumee, valeur ronde -- aucune donnee n'existe sur
#   l'appetence a un tel dispositif, nouveau).
MES_VIVIER<-3500                        # k personnes 63-67 mobilisables
MES_PARTICIP<-0.10                      # 10% de participation a maturite (hypothese assumee)
MES_RAMP<-5                             # annees de montee en charge
MES_QUOTITE<-0.50                       # mi-temps
MES_SAL_BRUT<-45.0                      # k€/an salaire plein
MES_PENSION<-17.0                       # k€/an pension moyenne
MES_ECO_PENS<-0.30                      # part de pension suspendue en mi-temps
# -- COORDINATION (cout : allegement cotis patronales sur creation NETTE) --
COORD_COTIS<-0.42                       # taux cotis patronales moyen
COORD_SAL<-45.0                         # k€/an salaire brut moyen
COORD_ALLEG_1_3<-0.50                   # -50% annees 1-3
COORD_ALLEG_4_6<-0.30                   # -30% annees 4-6
# -- AIDE AU CAPITAL (symetrique de l'aide au travail, via partage de la valeur) --
#    On applique a la part CAPITAL du PIB le MEME taux de rabais que celui
#    accorde a la part TRAVAIL. K/AL reste neutre (pas de biais travail/capital).
ALPHA_L<-0.70                           # part travail dans la valeur ajoutee (Cobb-Douglas)
ALPHA_K<-0.30                           # part capital
# -- PRODUCTIVITE bonus post plein-emploi --
PROD_BONUS_POST_PE<-0.005               # +0,5%/an de productivite apres le PE
# -- Secteur public --
# Le PIB public croit a PHI_PUBLIC x (croissance HORS PRODUCTIVITE du scenario).
# Hors productivite : un gain de productivite privee ne justifie pas de creer des
# postes publics ; le public suit l'activite/l'emploi, pas l'efficience.
# PHI_PUBLIC=0,50 (hypothese ; une variante sera testee). Recettes : 43% du
# surcroit MARCHAND (public exclu). Depenses publiques : 70% de la hausse de VA
# publique (cout de fonctionnement = salaires nets d'IR, simplifie a 70%).
PHI_PUBLIC<-0.50

# =====================================================================
#  run_D14 — SENTIER RECONQUETE (prod basse) + bonus post-PE optionnel
# =====================================================================
run_D14 <- function(PROD_BONUS, NOM) {
  industrie4 <- c("Agriculture and food","Traditional manufacturing",
                  "Chemicals and materials","Machinery, equipment and transport equipment")
  is_ind <- sector_names %in% industrie4
  d_loc <- d %>% arrange(match(sector,sector_names))
  Y0<-sum(d_loc$VA_real); L0<-sum(d_loc$L); VA0<-d_loc$VA_real
  lam_star <- lambda_star
  IDX <- which(sector_names=="Public, education and health-social services")
  delta<-lam_star[IDX]-LAMBDA_PUBLIC_CIBLE; lam_star[IDX]<-LAMBDA_PUBLIC_CIBLE
  o<-setdiff(seq_along(lam_star),IDX); lam_star[o]<-lam_star[o]+delta*lam_star[o]/sum(lam_star[o])
  lam_star<-lam_star/sum(lam_star)
  
  # Population ACTIVE — INSEE, Projections de population active 2022-2070
  # (30 juin 2022), scenario central : +20k/an ->2040 ; -50k/an 2040-2050 ;
  # -30 a -40k/an 2050-2070.  Sur ~30 M d'actifs :
  n_pop_of <- function(a) if (a<=2040) 0.0007 else if (a<=2050) -0.0017 else -0.0012
  prod_of  <- function(a, aPE){ if (a<=(if(is.na(aPE)) 2034 else aPE))
    PROD_BASE+(PROD_CIBLE-PROD_BASE)*(a-ANNEE0)/max(1,(if(is.na(aPE)) 2034 else aPE)-ANNEE0)
    else PROD_CIBLE }
  # NB : prod "basse" => on GARDE PROD_BASE fixe ; le crescendo prod_of n'est utilise
  #      que si on veut la haute. Ici plan basse => productivite = PROD_BASE.
  prod_basse <- function(a) PROD_BASE
  g_reconq <- function(t) if (t<=RAMP_YEARS) G_START+(G_END-G_START)*(t-1)/max(1,RAMP_YEARS-1) else G_END
  
  lam0<-VA0/sum(VA0); pi0<-sum(lam0[is_ind]); pistar<-sum(lam_star[is_ind])
  cal<-function(K){lam<-lam0;yr<-NA;for(t in 1:HORIZON){lam<-lam+K*(lam_star-lam);lam<-lam/sum(lam)
  if(is.na(yr)&&(sum(lam[is_ind])-pi0)/(pistar-pi0)>=0.95)yr<-t};if(is.na(yr))999 else yr}
  gr<-seq(0.05,0.60,by=0.01);KAPPA<-gr[which.min(abs(sapply(gr,cal)-AUTONOMIE_ANS))]
  cal_eta<-function(e){U<-RESERVOIR;L<-L0;yr<-NA;for(t in 1:HORIZON){
    if(U<=FRICTIONNEL+1e-6){if(is.na(yr))yr<-t;break};g<-g_reconq(t)
    dL<-e*max(0,g-PROD_BASE)*L;if(U-dL<FRICTIONNEL)dL<-U-FRICTIONNEL;U<-U-dL;L<-L+dL
    if(U<=FRICTIONNEL+1e-6&&is.na(yr))yr<-t};if(is.na(yr))999 else yr}
  ge<-seq(0.50,1.00,by=0.01);ETA<-ge[which.min(abs(sapply(ge,cal_eta)-PLEIN_EMPLOI_ANS))]
  
  Yt<-Y0;Lt<-L0;Ut<-RESERVOIR;VA<-VA0;rows<-list();aPE<-NA
  for(t in 1:HORIZON){
    a<-ANNEE0+t-1
    if(a==ANNEE0){
      # 2027 = ANNEE DE MISE EN PLACE (loi de finances, SI fiscal DGFiP, plan
      # de marche sectoriel). Croissance MOLLE 0,9%, pas de reconquete, pas de
      # creation nette (donc pas de cout) : on part du desequilibre herite.
      Yt<-Yt*(1+MOLLE_G); VA<-lam0*Yt
      rows[[t]]<-tibble(annee=a,phase=0,g_pct=100*MOLLE_G,reindus_pct=0,
                        emploi_total_k=Lt,chomage_k=Ut,
                        lam_pub=lam0[IDX], va_pub_Mds=lam0[IDX]*Yt,
                        g_horsprod_pct=100*(MOLLE_G-PROD_BASE))
      next
    }
    tr<-t-1                                    # horloge de reconquete : demarre en 2028
    pe<-(Ut<=FRICTIONNEL+1e-6); if(pe&&is.na(aPE))aPE<-a
    if(!pe){g<-g_reconq(tr);ph<-1; g_hp<-g-PROD_BASE}      # reconquete : hors prod = g - prod tendancielle
    else{g<-prod_basse(a)+PROD_BONUS+n_pop_of(a);ph<-2; g_hp<-n_pop_of(a)}  # post-PE : hors prod = demographie seule
    # --- STRUCTURE lambda ---------------------------------------------
    # FEUILLE DE ROUTE (avant PE) : convergence vers les cibles via KAPPA.
    #   La cible publique 16,7% est compatible avec une VA publique qui croit
    #   ~a 50% du hors-productivite ; les deux se rejoignent au plein emploi.
    # BGP (apres PE) : FIN de la feuille de route. Plus de cible, plus de KAPPA.
    #   "Chacun pour soi" : les parts marchandes sont FIGEES (tous au rythme
    #   n+g), et l'ETAT croit au seul rythme demographique n -> sa part reflue.
    if(!pe){
      lam<-VA/Yt; lam<-lam+KAPPA*(lam_star-lam); lam<-lam/sum(lam)
      Yt<-Yt*(1+g); VA<-lam*Yt
    } else {
      # Etat : croit a n (demographie). Marchands : absorbent le PIB restant,
      # parts relatives figees a leur niveau atteint au PE.
      lam_prev<-VA/sum(VA)                       # parts en fin d'annee precedente
      Yt<-Yt*(1+g)                               # PIB total au rythme BGP (n+g)
      VApub_new<-VA[IDX]*(1+n_pop_of(a))         # Etat : rythme demographique seul
      reste<-Yt-VApub_new                        # PIB marchand restant
      wm<-lam_prev[-IDX]/sum(lam_prev[-IDX])     # parts marchandes figees (relatives)
      VA<-numeric(J); VA[IDX]<-VApub_new; VA[-IDX]<-wm*reste
      lam<-VA/Yt
    }
    prog<-min(1,max(0,(sum(lam[is_ind])-pi0)/(pistar-pi0)))
    if(!pe){dL<-ETA*max(0,g-PROD_BASE)*Lt;if(Ut-dL<FRICTIONNEL)dL<-Ut-FRICTIONNEL;Ut<-Ut-dL;Lt<-Lt+dL}
    else{Lt<-Lt*(1+n_pop_of(a))}
    rows[[t]]<-tibble(annee=a,phase=ph,g_pct=100*g,reindus_pct=100*prog,
                      emploi_total_k=Lt,chomage_k=Ut,
                      lam_pub=lam[IDX], va_pub_Mds=lam[IDX]*Yt, g_horsprod_pct=100*g_hp)
  }
  sentier<-bind_rows(rows)
  write_xlsx(list(sentier=sentier), file.path(OUT_DIR,paste0("SENTIER_",NOM,".xlsx")))
  sentier
}

# =====================================================================
#  run_molle — SENTIER MOLLE (0,9%/an, pas de reconquete)
# =====================================================================
run_molle <- function(NOM){
  a<-ANNEE0:ANNEE_FIN
  # molle 0,9%/an des 2027 (croissance reelle minimale, sans reconquete).
  # Part publique constante (pas de convergence en molle) ; g hors prod = molle - prod tendancielle.
  sentier<-tibble(annee=a, phase=0, g_pct=100*MOLLE_G, reindus_pct=0,
                  emploi_total_k=30800*(1+0.001)^(a-ANNEE0), chomage_k=4000,
                  lam_pub=PART_PUB_2024, va_pub_Mds=NA_real_,
                  g_horsprod_pct=100*(MOLLE_G-PROD_BASE))
  write_xlsx(list(sentier=sentier), file.path(OUT_DIR,paste0("SENTIER_",NOM,".xlsx")))
  sentier
}

# =====================================================================
#  mesurette_of — recette + economie de pension pour une annee
# =====================================================================
mesurette_of <- function(annee){
  t<-annee-ANNEE0+1
  ramp<-min(1, t/MES_RAMP)
  particip<-MES_VIVIER*MES_PARTICIP*ramp            # k participants
  sal_mt<-MES_SAL_BRUT*MES_QUOTITE                  # k€ salaire mi-temps
  recette<-particip*sal_mt*TAUX_PO/1000             # Md (43% du salaire mi-temps)
  eco<-particip*MES_PENSION*MES_ECO_PENS/1000       # Md (30% pension suspendue)
  list(recette=recette, eco=eco)
}

# =====================================================================
#  coord_cost_vector — AIDE AU TRAVAIL par annee (allegement cotisations)
#  2027 = annee molle (pas de creation). La creation nette demarre en 2028 :
#  on ancre sur l'emploi de 2027 (emp[1]), donc dL[2028]=emp[2028]-emp[2027].
# =====================================================================
coord_cost_vector <- function(sentier){
  a<-sentier$annee; emp<-sentier$emploi_total_k
  dL<-c(0, diff(emp))                              # dL[1]=0 (2027 molle), puis creations
  c50<-COORD_ALLEG_1_3*COORD_COTIS*COORD_SAL/1000   # Md par k-emploi (annees 1-3)
  c30<-COORD_ALLEG_4_6*COORD_COTIS*COORD_SAL/1000   # Md par k-emploi (annees 4-6)
  get<-function(i) if(i>=1 && i<=length(dL)) dL[i] else 0
  cost<-numeric(length(a))
  for(i in seq_along(a)){
    s50<-get(i)+get(i-1)+get(i-2)                   # cohortes en annees 1-3
    s30<-get(i-3)+get(i-4)+get(i-5)                 # cohortes en annees 4-6
    cost[i]<-max(0, c50*s50) + max(0, c30*s30)      # aide TRAVAIL, jamais negative
  }
  setNames(cost, a)
}

# =====================================================================
#  run_D15_cascade — rebouclage avec COR / mesurette / coordination
# =====================================================================
run_D15_cascade <- function(sentier, COR, MES_ON, COORD_ON, NOM){
  g_an<-setNames(sentier$g_pct/100, sentier$annee)
  ghp_an<-setNames(sentier$g_horsprod_pct/100, sentier$annee)   # croissance hors productivite
  coord_vec<-if(COORD_ON) coord_cost_vector(sentier) else setNames(rep(0,nrow(sentier)),sentier$annee)
  PIB<-PIB_2024; dette<-DETTE_2024; rows<-list(); pic<-0; pic_an<-NA; an_prim<-NA
  VApub<-PART_PUB_2024*PIB_2024; VApub_2024<-VApub    # VA publique de reference (2024)
  phase_an<-setNames(sentier$phase, sentier$annee)    # 1 = reconquete, 2 = BGP post-PE
  npop_of<-function(a) if (a<=2040) 0.0007 else if (a<=2050) -0.0017 else -0.0012
  for(annee in ANNEE0:ANNEE_FIN){
    t<-annee-ANNEE0+1; g<-g_an[[as.character(annee)]]; PIB<-PIB*(1+g)
    # -- SECTEUR PUBLIC : croissance de la VA publique --------------------
    #  FEUILLE DE ROUTE (phase 1) : ~50% du hors-productivite (compatible avec
    #    la cible 16,7% atteinte au plein emploi).
    #  BGP (phase 2, post-PE) : rythme DEMOGRAPHIQUE plein (n) -- coherent avec
    #    le sentier ("chacun pour soi", l'Etat suit la population).
    g_hp<-ghp_an[[as.character(annee)]]
    ph  <-phase_an[[as.character(annee)]]
    g_pub<-if(is.na(ph) || ph<2) PHI_PUBLIC*g_hp else npop_of(annee)
    VApub<-VApub*(1+g_pub)                               # VA publique en Md courants
    dVApub<-VApub-VApub_2024                             # hausse de VA publique depuis 2024
    # RECETTES : 43% du surcroit de PIB MARCHAND (public exclu, ni recette sur VA pub)
    dPIB_march<-(PIB-PIB_2024)-dVApub
    rec_new<-TAUX_PO*dPIB_march
    # DEPENSES de fonctionnement public : 70% de la hausse de VA publique
    dep_public<-0.70*dVApub
    # Croissance des depenses de retraite calee sur les taux du COR (rapport juin
    # 2025 : +1,2%/an jusqu'en 2030, +0,8% 2031-2050, +0,6% ensuite). On raisonne
    # en MASSE de depenses (pas en cohortes) : le facteur court donc depuis
    # l'annee de base 2024 (croissance appliquee des 2025), exactement comme la
    # sante et comme le COR. Nos niveaux coincident alors avec ceux du COR.
    retr_fac<-1
    for(s in 2025:annee){ p<-if(s<=2030)0.012 else if(s<=2050)0.008 else 0.006; retr_fac<-retr_fac*(1+p) }
    if(COR){ ans<-max(0,min(annee,2030)-2027+1); fac<-(1-SOUSINDEX)^ans
    demo_retr<-RETR_BASE_2024*(retr_fac-1)*fac + RETR_BASE_2024*(fac-1); surcout<-0
    } else { demo_retr<-RETR_BASE_2024*(retr_fac-1); surcout<-COUT_NON_REFORME*min(t,5)/5 }
    demo_sante<-sante_surcout(annee)
    mes<-if(MES_ON) mesurette_of(annee) else list(recette=0,eco=0)
    # AIDE COORDINATION : travail (cotisations) + capital (symetrique)
    aide_L<-if(COORD_ON) coord_vec[[as.character(annee)]] else 0
    taux_rabais_L<-if(ALPHA_L*PIB>0) aide_L/(ALPHA_L*PIB) else 0  # taux effectif sur le travail
    aide_K<-taux_rabais_L*ALPHA_K*PIB                            # meme taux applique a la part capital
    coord<-aide_L+aide_K
    demo<-demo_retr+demo_sante+surcout - mes$eco
    recettes<-rec_new + mes$recette - coord
    part<-1-(1-1/MATURITE)^t; taux<-TAUX_ANCIEN+part*(TAUX_NOUVEAU-TAUX_ANCIEN)
    charge<-taux*dette; d_charge<-charge-CHARGE_2024
    solde_prim<-recettes-demo-dep_public-DEFICIT_2024; solde_glob<-solde_prim-d_charge
    dette<-dette-solde_glob; ratio<-100*dette/PIB
    if(ratio>pic){pic<-ratio;pic_an<-annee}; if(is.na(an_prim)&&solde_prim>0)an_prim<-annee
    rows[[length(rows)+1]]<-data.frame(annee=annee,croissance_pct=round(100*g,2),PIB_Mds=round(PIB,0),
                                       recettes_new_Mds=round(rec_new,1),mesurette_recette_Mds=round(mes$recette,2),
                                       coord_cout_Mds=round(coord,2),coord_aide_L_Mds=round(aide_L,2),coord_aide_K_Mds=round(aide_K,2),
                                       demo_Mds=round(demo,1),dep_public_Mds=round(dep_public,1),charge_Mds=round(charge,1),
                                       solde_prim_Mds=round(solde_prim,1),solde_glob_Mds=round(solde_glob,1),
                                       deficit_pib_pct=round(-100*solde_glob/PIB,2),dette_Mds=round(dette,0),dette_pib_pct=round(ratio,1))
  }
  res<-do.call(rbind,rows)
  write_xlsx(list(rebouclage=res), file.path(OUT_DIR,paste0("PLAN_",NOM,".xlsx")))
  n<-nrow(res)
  cat(sprintf("[%s] dette2060=%.1f%% | pic=%.1f%% (%s) | deficit2060=%.1f%% | exc.prim=%s\n",
              NOM,res$dette_pib_pct[n],pic,pic_an,res$deficit_pib_pct[n],
              if(is.na(an_prim))"jamais" else as.character(an_prim)))
  res
}

# =====================================================================
#  CONSTRUCTION DES 5 COURBES EN CASCADE
# =====================================================================
cat("\n########## PLAN UNIQUE GRADUE (prod basse 0,5%) ##########\n")
cat("# Sorties :", normalizePath(OUT_DIR,mustWork=FALSE), "\n\n")

s_molle <- run_molle("molle")
s_coord <- run_D14(0.0,               "coordination")
s_coord_bonus <- run_D14(PROD_BONUS_POST_PE, "coordination_prodbonus")

r1 <- run_D15_cascade(s_molle,       COR=FALSE, MES_ON=FALSE, COORD_ON=FALSE, NOM="1_sans_reformes")
r2 <- run_D15_cascade(s_molle,       COR=TRUE,  MES_ON=FALSE, COORD_ON=FALSE, NOM="2_COR")
r3 <- run_D15_cascade(s_molle,       COR=TRUE,  MES_ON=TRUE,  COORD_ON=FALSE, NOM="3_mesurette")
r4 <- run_D15_cascade(s_coord,       COR=TRUE,  MES_ON=TRUE,  COORD_ON=TRUE,  NOM="4_coordination")
r5 <- run_D15_cascade(s_coord_bonus, COR=TRUE,  MES_ON=TRUE,  COORD_ON=TRUE,  NOM="5_productivite")

# ---- recap ----
fin<-function(r) r[nrow(r),]
recap<-data.frame(
  courbe=c("1_sans_reformes","2_COR","3_mesurette","4_coordination","5_productivite"),
  dette_2060_pct=sapply(list(r1,r2,r3,r4,r5),function(r)fin(r)$dette_pib_pct),
  deficit_2060_pct=sapply(list(r1,r2,r3,r4,r5),function(r)fin(r)$deficit_pib_pct))
write_xlsx(list(recap=recap), file.path(OUT_DIR,"RECAP_PLAN_UNIQUE.xlsx"))
cat("\n============ RECAP_PLAN_UNIQUE ============\n"); print(recap,row.names=FALSE)
cat("\nTERMINE.", nrow(recap),"courbes. Fichiers PLAN_*.xlsx + SENTIER_*.xlsx + RECAP dans",
    normalizePath(OUT_DIR,mustWork=FALSE),"\n")