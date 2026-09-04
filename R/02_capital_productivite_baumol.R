# =====================================================================
#  D3 — BENCHMARK A PRIX DE REFERENCE BAUMOL + DECOMPOSITION (DEFINITIF)
#  A coller sous D2, apres avoir relance D1/D2 avec END_YEAR <- 2024.
#
#  FONDATIONS : capital sectoriel K_j (inventaire permanent depuis 1978,
#  FBCF par branche), TFP A_j (residu de Solow), tendance g_A_j.
#  Le benchmark garde sa DYNAMIQUE homogene (BGP algebrique, quantites
#  inchangees). Seul le VECTEUR DE PRIX de reference est enrichi :
#     p_B_j = P_commun * exp(-(g_A_j - g_A_agg)*T)   (Baumol / Ngai-Pissarides)
#  normalise pour que la moyenne ponderee (VA reelle B) reste P_commun
#  -> l'agregat nominal du benchmark, et donc le total, sont PRESERVES.
#
#  DECOMPOSITION du gap de VA nominale (identite exacte, bornee) :
#     gap_j = prix_residuel_j + structure_j
#     prix_residuel_j = (p_obs_j - p_B_j) * VA_reelle_obs_j   (rencherissement
#                       au-dela de ce que la productivite justifie)
#     structure_j     = p_B_j * (VA_reelle_obs_j - VA_reelle_B_j), ouvert en
#                       4 canaux reels (demande finale, invest., reseau, externe)
#  La composante prix-PRODUCTIVITE est desormais DANS le benchmark (montree
#  en descriptif), elle n'est plus un canal du gap.
#
#  ROBUSTESSE : changer DELTA (0.04 / 0.05 / 0.06) et relancer.
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(stringr)
library(stringi); library(tibble); library(ggplot2); library(scales); library(writexl)

# ---- CONFIG --------------------------------------------------------
fbcf_branche_path <- file.path(base_dir, "FBCF_branche_ALL.xlsx")
DELTA <- 0.05                                   # <<< robustesse : 0.04 / 0.05 / 0.06
stopifnot(file.exists(fbcf_branche_path))

.need <- c("core_data_real_all","dynamic_benchmark_sector","dynamic_benchmark_macro",
           "sector_map","sector_names","alpha","phi_dyn","base_dir","dyn_dir",
           "BASE_YEAR","END_YEAR")
.miss <- .need[!vapply(.need, exists, logical(1))]
if (length(.miss)>0) stop("D3 — objets manquants : ", paste(.miss,collapse=", "),
                          "\n-> lance d'abord 01_data_et_modele.R (END_YEAR=2024).")
if (!exists("round_numeric_df"))
  round_numeric_df <- function(df,d=6) df %>% mutate(across(where(is.numeric),~round(.x,d)))
b_dir <- file.path(dyn_dir, "D3_BENCHMARK_BAUMOL"); if(!dir.exists(b_dir)) dir.create(b_dir,recursive=TRUE)
norm1 <- function(x){s<-sum(x,na.rm=TRUE); if(!is.finite(s)||abs(s)<1e-12) x else x/s}
.asc  <- function(x) stri_trans_general(as.character(x),"Latin-ASCII")
ord   <- function(df) df %>% arrange(match(sector, sector_names))

# ---- 1. LECTEUR FBCF PAR BRANCHE (identique au diagnostic valide) --
read_fbcf_branche_real <- function(path){
  carac <- read_excel(path, sheet=2, .name_repair="minimal")
  cn <- tolower(.asc(names(carac)))
  ci<-which(grepl("idbank",cn))[1]; ca<-which(grepl("activ",cn))[1]; cu<-which(grepl("unite",cn))[1]
  carac2 <- tibble(idBank=as.character(carac[[ci]]), activite=.asc(carac[[ca]]), unite=.asc(carac[[cu]])) %>%
    filter(grepl("^A38-",activite), grepl("2014",unite)) %>%
    mutate(code=str_match(activite,"^A38-([A-Z0-9]+)")[,2]) %>% filter(!is.na(code))
  vals <- read_excel(path, sheet=1, .name_repair="minimal")
  vn<-names(vals); idcol<-which(grepl("idbank",tolower(.asc(vn))))[1]
  yearcols<-vn[grepl("^[0-9]{4}$",vn)]
  vals %>% transmute(idBank=as.character(.data[[vn[idcol]]]),
                     across(all_of(yearcols), ~suppressWarnings(as.numeric(.x)))) %>%
    pivot_longer(-idBank,names_to="year",values_to="I_mn") %>% mutate(year=as.integer(year)) %>%
    inner_join(carac2 %>% select(idBank,code), by="idBank") %>%
    inner_join(sector_map %>% distinct(code,sector), by="code") %>%
    group_by(year,sector) %>% summarise(I_branch_real=sum(I_mn,na.rm=TRUE)/1000, .groups="drop")
}
fbcf <- read_fbcf_branche_real(fbcf_branche_path)

# ---- 2. CAPITAL K_j (inventaire permanent depuis 1978) + TFP + tendance ----
build_K <- function(df){ df<-df %>% arrange(year); I<-df$I_branch_real; K<-numeric(length(I))
K[1]<-I[1]/(phi_dyn+DELTA); if(length(I)>1) for(t in 2:length(I)) K[t]<-(1-DELTA)*K[t-1]+I[t]
df$K<-K; df }
panelA <- fbcf %>% group_by(sector) %>% group_modify(~build_K(.x)) %>% ungroup() %>%
  inner_join(core_data_real_all %>% transmute(year,sector,VA_real=VA_real_obs,L=employment_obs),
             by=c("year","sector")) %>%
  mutate(A=VA_real/(K^alpha*L^(1-alpha)))
trend <- panelA %>% group_by(sector) %>%
  summarise(g_A=coef(lm(log(A)~year))[["year"]], .groups="drop")
w_end <- core_data_real_all %>% filter(year==END_YEAR) %>% ord() %>%
  transmute(sector, w=VA_nominal_obs/sum(VA_nominal_obs))
g_A_agg <- sum(trend$g_A * w_end$w[match(trend$sector,w_end$sector)])
trend <- trend %>% mutate(g_A_rel=g_A-g_A_agg) %>% ord()

# ---- 3. PRIX DE REFERENCE BAUMOL (normalise -> agregat preserve) ---
obs   <- ord(core_data_real_all       %>% filter(year==END_YEAR))
base0 <- ord(core_data_real_all       %>% filter(year==BASE_YEAR))
benb  <- ord(dynamic_benchmark_sector %>% filter(year==END_YEAR))
macroB<- dynamic_benchmark_macro      %>% filter(year==END_YEAR)
Tspan <- END_YEAR - BASE_YEAR
P_commun <- macroB$VA_nominal_B / macroB$VA_real_B

VA_real_obs<-obs$VA_real_obs; VA_nom_obs<-obs$VA_nominal_obs; p_obs<-obs$p_model
VA_real_B  <-benb$VA_real_B_sector
C_obs<-obs$C_real; I_obs<-obs$I_real; IC_obs<-obs$IC_sector_real; NX_obs<-obs$NX_real

p_B_raw <- P_commun*exp(-trend$g_A_rel*Tspan)
wB      <- VA_real_B/sum(VA_real_B)
p_B     <- p_B_raw*(P_commun/sum(wB*p_B_raw))         # moyenne ponderee VA_reelle_B == P_commun
VA_nom_B_baumol <- p_B*VA_real_B                      # somme == VA_nominal_B agregat  (preserve)

# ---- 4. DECOMPOSITION : prix-residuel + 4 canaux structurels -------
gap_nom         <- VA_nom_obs - VA_nom_B_baumol
prix_residuel   <- (p_obs - p_B)*VA_real_obs
structure_block <- gap_nom - prix_residuel           # = p_B*(VA_real_obs - VA_real_B)

C_B <- norm1(base0$C_real)*macroB$C_B ; I_B <- norm1(base0$I_real)*macroB$I_B
IC_B<- benb$IC_B_sector ; NX_B<- ifelse(base0$VA_real_obs>0, base0$NX_real/base0$VA_real_obs,0)*VA_real_B
rC<-p_B*(C_obs-C_B); rI<-p_B*(I_obs-I_B); rIC<-p_B*(IC_obs-IC_B); rNX<-p_B*(NX_obs-NX_B)
raw<-rC+rI+rIC+rNX; fac<-ifelse(abs(raw)>1e-9, structure_block/raw, 0)
final_demand<-rC*fac; investment_demand<-rI*fac; production_network<-rIC*fac; external_closure<-rNX*fac

prix_productivite_benchmark <- (p_B - P_commun)*VA_real_obs   # DESCRIPTIF (dans le benchmark, pas un canal)

# ---- 5. Objets pour l'aval (waterfall) : gaps Baumol --------------
sector_gaps_baumol <- tibble(
  year=END_YEAR, sector=sector_names,
  VA_nominal_obs=VA_nom_obs, VA_nominal_B_sector=VA_nom_B_baumol,
  VA_real_obs=VA_real_obs, VA_real_B_sector=VA_real_B,
  p_obs=p_obs, p_B=p_B, gap_VA_nominal=gap_nom,
  prix_residuel=prix_residuel,
  final_demand=final_demand, investment_demand=investment_demand,
  production_network=production_network, external_closure=external_closure,
  prix_productivite_benchmark=prix_productivite_benchmark)

# groupes diagnostiques (memes regroupements que D2) pour le waterfall
grp <- tibble(sector=sector_names) %>% mutate(diagnostic_group=case_when(
  sector %in% c("Agriculture and food","Extractive, energy and utilities",
                "Traditional manufacturing","Chemicals and materials",
                "Machinery, equipment and transport equipment") ~ "Exposed productive sectors",
  sector %in% c("Trade, transport and hospitality","Construction") ~ "Fragile absorbers",
  sector %in% c("Information and communication","Business services") ~ "Productive internal services",
  TRUE ~ "Asset and institutional sectors"))
group_gaps_baumol <- sector_gaps_baumol %>% left_join(grp,by="sector") %>%
  group_by(diagnostic_group) %>%
  summarise(gap_VA_nominal=sum(gap_VA_nominal), prix_residuel=sum(prix_residuel),
            structure=sum(final_demand+investment_demand+production_network+external_closure),
            .groups="drop")

# ---- 6. CHECKS ----------------------------------------------------
res <- sector_gaps_baumol %>%
  mutate(structure_total=final_demand+investment_demand+production_network+external_closure,
         somme=prix_residuel+structure_total, residu=gap_VA_nominal-somme) %>%
  arrange(desc(gap_VA_nominal))
checks <- tibble(check=c(
  "delta","Max |reconstruction - gap| (additivite)",
  "Agregat nominal benchmark preserve : sum(VA_nom_B_baumol) - VA_nominal_B",
  "Total gap nominal (somme secteurs)","g_A_agg","P_commun",
  "Amplitude prix : max|p_obs-P_commun| -> max|p_obs-p_B|"),
  value=c(DELTA, max(abs(res$residu)),
          sum(VA_nom_B_baumol)-macroB$VA_nominal_B, sum(gap_nom), g_A_agg, P_commun,
          NA))
amp_av<-max(abs(p_obs-P_commun)); amp_ap<-max(abs(p_obs-p_B))

# ---- 7. Heatmap (1 prix residuel + 4 structure) -------------------
lev<-c("prix_residuel","final_demand","investment_demand","production_network","external_closure")
long5<-res %>% select(sector,all_of(lev)) %>% pivot_longer(-sector,names_to="canal",values_to="c") %>%
  mutate(canal=factor(canal,levels=lev), sector=factor(sector,levels=rev(res$sector)),
         label=ifelse(abs(c)<0.05,"",paste0(ifelse(c>=0,"+",""),formatC(c,format="f",digits=1))))
mx<-max(abs(long5$c),na.rm=TRUE)
heat<-ggplot(long5,aes(canal,sector,fill=c))+geom_tile(color="white",linewidth=.5)+
  geom_text(aes(label=label),size=2.8,fontface="bold",color="grey15")+
  scale_fill_gradient2(low="#D98C8C",mid="white",high="#7FA8C9",midpoint=0,limits=c(-mx,mx),name="Md\u20ac")+
  scale_x_discrete(labels=c("Prix\nresiduel","Demande\nfinale","Demande\ninvest.","Reseau\nproduction","Cloture\nexterne"))+
  labs(title=paste0("Decomposition du gap de VA nominale \u2014 benchmark Baumol \u2014 ",END_YEAR),
       subtitle="Prix-productivite dans le benchmark | prix-residuel = rencherissement au-dela des couts | 4 canaux structurels",
       x=NULL,y=NULL)+theme_minimal(base_size=11)+
  theme(axis.text.x=element_text(size=8.5),panel.grid=element_blank(),plot.title=element_text(face="bold",size=12))
ggsave(file.path(b_dir,"Figure_decomposition_benchmark_Baumol.pdf"),heat,width=12,height=7,units="in",device=cairo_pdf)
ggsave(file.path(b_dir,"Figure_decomposition_benchmark_Baumol.png"),heat,width=12,height=7,units="in",dpi=300)
write_xlsx(list(sector_gaps_baumol=sector_gaps_baumol, group_gaps_baumol=group_gaps_baumol,
                trend_TFP=trend, checks=checks), file.path(b_dir,"D3_benchmark_Baumol_outputs.xlsx"))

cat("\n==================== D3 BAUMOL — CHECKS ====================\n")
print(round_numeric_df(checks,6),n=Inf,width=Inf)
cat("Amplitude prix : agrege max",round(amp_av,3),"-> Baumol max",round(amp_ap,3),
    " (reduction",round(100*(1-amp_ap/amp_av),1),"%)\n")
cat("\n==================== D3 BAUMOL — DECOMPOSITION (secteurs) ====================\n")
print(res %>% select(sector,gap_VA_nominal,prix_residuel,structure_total,
                     final_demand,investment_demand,production_network,external_closure,
                     prix_productivite_benchmark) %>% round_numeric_df(2),n=Inf,width=Inf)
cat("\n==================== D3 BAUMOL — GAPS PAR GROUPE (pour le waterfall) ====================\n")
print(round_numeric_df(group_gaps_baumol,2),n=Inf,width=Inf)
cat("\nObjets pour l'aval : sector_gaps_baumol , group_gaps_baumol\n")
cat("D3 BAUMOL DONE.\n")



# =====================================================================
#  PATCH D3 — EXPORT DU CAPITAL SECTORIEL K_j  (version robuste)
#  A COLLER dans D3, juste avant "# ---- 7. Heatmap", apres panelA et trend.
#  Prend la DERNIERE annee disponible dans panelA (2024 si presente, sinon 2023 :
#  la FBCF par branche s'arrete parfois un an avant).
# =====================================================================

# annee de reference du capital = derniere annee presente dans panelA
an_K <- max(panelA$year, na.rm = TRUE)
if (an_K != END_YEAR)
  cat("\n[info] capital : derniere annee FBCF disponible =", an_K,
      "(END_YEAR =", END_YEAR, ") -> on prend", an_K, "\n")

capital_ref <- panelA %>%
  filter(year == an_K) %>%
  transmute(sector,
            annee_K = year,
            K_ref       = K,
            VA_real_ref = VA_real,
            L_ref       = L,
            A_ref       = A,
            K_sur_VA    = ifelse(VA_real > 0, K / VA_real, NA_real_),
            K_sur_L     = ifelse(L > 0,       K / L,       NA_real_)) %>%
  left_join(trend %>% select(sector, g_A, g_A_rel), by = "sector") %>%
  arrange(match(sector, sector_names))

cat("\n==================== D3 — CAPITAL SECTORIEL K_j (", an_K, ") ====================\n")
cat("Inventaire permanent depuis 1978, DELTA =", DELTA, ", alpha =", alpha, "\n\n")
print(round_numeric_df(capital_ref, 3), n = Inf, width = Inf)

cat("\nControle agrege :\n")
cat("  Nb secteurs   :", nrow(capital_ref), "(attendu 12)\n")
cat("  K total       :", round(sum(capital_ref$K_ref, na.rm=TRUE), 1), "\n")
cat("  VA reelle tot :", round(sum(capital_ref$VA_real_ref, na.rm=TRUE), 1), "\n")
cat("  K/VA agrege   :", round(sum(capital_ref$K_ref, na.rm=TRUE) /
                                 sum(capital_ref$VA_real_ref, na.rm=TRUE), 3),
    "(ordre attendu ~2.5-3.5)\n")

write_xlsx(list(capital_ref = round_numeric_df(capital_ref, 6)),
           file.path(b_dir, "D3_capital_sectoriel.xlsx"))
cat("\nFichier : D3_capital_sectoriel.xlsx\n")