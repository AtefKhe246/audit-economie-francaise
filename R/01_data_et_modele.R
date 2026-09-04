# =====================================================================
#  01 — MODELE : DONNEES + BENCHMARK + GAPS  (pipeline propre, parametre)
#  L'Audit de l'economie francaise — A. Khelifi
#  Refactor de Replication_Code_R_V7.R (blocs D1 + D2). Figure -> script 02.
#
#  CE QUE FAIT CE SCRIPT :
#    D1  lit TES + emploi + VA + indices -> panel core annuel.
#    D2  reconstruit le benchmark de viabilite recursif (base BASE_YEAR)
#        et calcule les ecarts (gaps) macro / sectoriels / par groupe.
#    Exporte le classeur DYNAMIC_D2 dans base_dir/DYNAMIC_PANEL_<BASE>_<END>.
#
#  POUR METTRE A JOUR (ex. 2021 -> 2023) :
#    1) deposer dans base_dir les fichiers INSEE des nouvelles annees :
#       TES_38_2022.xlsx, TES_38_2023.xlsx, + colonnes 2022/2023 dans
#       Emploi_ALL / VA_brute_nominale_ALL / Indices_ALL.
#    2) mettre END_YEAR <- 2023 dans le bloc CONFIG.
#    3) relancer : tout le reste suit automatiquement.
# =====================================================================




# ============================================================
# DYNAMIC BLOCK D1 — BUILD ANNUAL CORE DATA PANEL
# France 2000–2021
#
# Objectif:
# - généraliser le pipeline statique 2000/2021 à toutes les années
# - lire les fichiers ALL: emploi, VA nominale, indices de prix
# - lire automatiquement tous les TES_38_YYYY.xlsx
# - reconstruire VA nominale, VA réelle, prix implicites par secteur
# - produire core_data_all exploitable pour benchmark dynamique
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "tibble", "writexl", "stringi", "ggplot2", "forcats"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(writexl)
library(stringi)
library(ggplot2)
library(forcats)


# ============================================================
# 1. CONFIGURATION  (les seuls reglages a editer)
# ============================================================

BASE_YEAR <- 2000          # annee de reference (base du benchmark) — NE PAS changer
END_YEAR  <- 2024          # <<< ANNEE DE FIN : editer ici a chaque mise a jour INSEE
alpha     <- 0.30          # part du capital dans la VA
epsilon_baseline <- 0.60

# Dossier des donnees brutes INSEE.
# Par defaut : sous-dossier "data/" a la racine du projet.
# Peut etre surcharge par une variable d'environnement AUDIT_DATA_DIR.
base_dir <- Sys.getenv("AUDIT_DATA_DIR", unset = file.path(getwd(), "data"))

emploi_path <- file.path(base_dir, "Emploi_ALL.xlsx")
va_nom_path <- file.path(base_dir, "VA_brute_nominale_ALL.xlsx")
indices_path <- file.path(base_dir, "Indices_ALL.xlsx")

stopifnot(file.exists(emploi_path))
stopifnot(file.exists(va_nom_path))
stopifnot(file.exists(indices_path))

# TES files must be named TES_38_2000.xlsx, ..., TES_38_2021.xlsx
tes_files <- tibble(
  path = list.files(
    base_dir,
    pattern = "^TES_38_[0-9]{4}\\.xlsx$",
    full.names = TRUE
  )
) %>%
  mutate(
    year = as.integer(str_extract(basename(path), "[0-9]{4}"))
  ) %>%
  filter(year >= BASE_YEAR, year <= END_YEAR) %>%
  arrange(year)

expected_years <- BASE_YEAR:END_YEAR
missing_tes_years <- setdiff(expected_years, tes_files$year)

if (length(missing_tes_years) > 0) {
  stop(
    paste0(
      "Missing TES files for years: ",
      paste(missing_tes_years, collapse = ", "),
      "\nExpected files named TES_38_YYYY.xlsx in: ",
      base_dir
    )
  )
}

# (alpha defini dans la CONFIG ci-dessus)
# (epsilon_baseline defini dans la CONFIG ci-dessus)

dyn_dir <- file.path(base_dir, paste0("DYNAMIC_PANEL_", BASE_YEAR, "_", END_YEAR))
if (!dir.exists(dyn_dir)) dir.create(dyn_dir, recursive = TRUE)


# ============================================================
# 2. SECTOR MAPPING
# ============================================================

a38_codes <- c(
  "AZ","BZ","CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM",
  "DZ","EZ","FZ","GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ",
  "OZ","PZ","QA","QB","RZ","SZ","TZ"
)

sector_map <- tribble(
  ~code, ~group12, ~sector,
  "AZ", "G01", "Agriculture and food",
  "CA", "G01", "Agriculture and food",
  "BZ", "G02", "Extractive, energy and utilities",
  "CD", "G02", "Extractive, energy and utilities",
  "DZ", "G02", "Extractive, energy and utilities",
  "EZ", "G02", "Extractive, energy and utilities",
  "CB", "G03", "Traditional manufacturing",
  "CC", "G03", "Traditional manufacturing",
  "CM", "G03", "Traditional manufacturing",
  "CE", "G04", "Chemicals and materials",
  "CF", "G04", "Chemicals and materials",
  "CG", "G04", "Chemicals and materials",
  "CH", "G04", "Chemicals and materials",
  "CI", "G05", "Machinery, equipment and transport equipment",
  "CJ", "G05", "Machinery, equipment and transport equipment",
  "CK", "G05", "Machinery, equipment and transport equipment",
  "CL", "G05", "Machinery, equipment and transport equipment",
  "FZ", "G06", "Construction",
  "GZ", "G07", "Trade, transport and hospitality",
  "HZ", "G07", "Trade, transport and hospitality",
  "IZ", "G07", "Trade, transport and hospitality",
  "JA", "G08", "Information and communication",
  "JB", "G08", "Information and communication",
  "JC", "G08", "Information and communication",
  "KZ", "G09", "Finance and real estate",
  "LZ", "G09", "Finance and real estate",
  "MA", "G10", "Business services",
  "MB", "G10", "Business services",
  "MC", "G10", "Business services",
  "NZ", "G10", "Business services",
  "OZ", "G11", "Public, education and health-social services",
  "PZ", "G11", "Public, education and health-social services",
  "QA", "G11", "Public, education and health-social services",
  "QB", "G11", "Public, education and health-social services",
  "RZ", "G12", "Cultural, personal and household services",
  "SZ", "G12", "Cultural, personal and household services",
  "TZ", "G12", "Cultural, personal and household services"
)

group_order <- sector_map %>%
  distinct(group12, sector) %>%
  arrange(group12)

sector_names <- group_order$sector

macro_map_clean <- tribble(
  ~Code, ~group12, ~sector,
  "A5_AZ",  "G01", "Agriculture and food",
  "A17_C1", "G01", "Agriculture and food",
  "A38_BZ", "G02", "Extractive, energy and utilities",
  "A17_C2", "G02", "Extractive, energy and utilities",
  "A38_DZ", "G02", "Extractive, energy and utilities",
  "A38_EZ", "G02", "Extractive, energy and utilities",
  "A38_CB", "G03", "Traditional manufacturing",
  "A38_CC", "G03", "Traditional manufacturing",
  "A38_CM", "G03", "Traditional manufacturing",
  "A38_CE", "G04", "Chemicals and materials",
  "A38_CF", "G04", "Chemicals and materials",
  "A38_CG", "G04", "Chemicals and materials",
  "A38_CH", "G04", "Chemicals and materials",
  "A38_CI", "G05", "Machinery, equipment and transport equipment",
  "A38_CJ", "G05", "Machinery, equipment and transport equipment",
  "A38_CK", "G05", "Machinery, equipment and transport equipment",
  "A17_C4", "G05", "Machinery, equipment and transport equipment",
  "A5_FZ",  "G06", "Construction",
  "A17_GZ", "G07", "Trade, transport and hospitality",
  "A17_HZ", "G07", "Trade, transport and hospitality",
  "A17_IZ", "G07", "Trade, transport and hospitality",
  "A38_JA", "G08", "Information and communication",
  "A38_JB", "G08", "Information and communication",
  "A38_JC", "G08", "Information and communication",
  "A10_KZ", "G09", "Finance and real estate",
  "A10_LZ", "G09", "Finance and real estate",
  "A38_MA", "G10", "Business services",
  "A38_MB", "G10", "Business services",
  "A38_MC", "G10", "Business services",
  "A38_NZ", "G10", "Business services",
  "A38_OZ", "G11", "Public, education and health-social services",
  "A38_PZ", "G11", "Public, education and health-social services",
  "A38_QA", "G11", "Public, education and health-social services",
  "A38_QB", "G11", "Public, education and health-social services",
  "A38_RZ", "G12", "Cultural, personal and household services",
  "A38_SZ", "G12", "Cultural, personal and household services",
  "A38_TZ", "G12", "Cultural, personal and household services"
)


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

normalize_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x
}

as_num_fr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_replace_all(x, "\u00A0", "")
  x <- str_replace_all(x, " ", "")
  x <- str_replace_all(x, ",", ".")
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

as_num_zero <- function(x) {
  out <- as_num_fr(x)
  out[is.na(out)] <- 0
  out
}

normalize_sum <- function(x) {
  sx <- sum(x, na.rm = TRUE)
  if (is.na(sx) || abs(sx) < 1e-12) return(rep(NA_real_, length(x)))
  x / sx
}

safe_solve <- function(A, b) {
  out <- tryCatch(
    solve(A, b),
    error = function(e) {
      message("solve() failed; using qr.solve(). Error was: ", e$message)
      qr.solve(A, b)
    }
  )
  as.numeric(out)
}

safe_div <- function(x, y, threshold = 1e-12) {
  ifelse(abs(y) > threshold, x / y, NA_real_)
}

round_numeric_df <- function(df, digits = 6) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

read_raw <- function(path, sheet) {
  read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
}

find_sheet <- function(path, pattern) {
  sh <- excel_sheets(path)
  idx <- which(str_detect(normalize_txt(sh), normalize_txt(pattern)))
  if (length(idx) == 0) stop(paste("Sheet not found:", pattern, "in", path))
  sh[idx[1]]
}

find_code_col <- function(raw) {
  counts <- sapply(seq_along(raw), function(j) {
    sum(as.character(raw[[j]]) %in% a38_codes, na.rm = TRUE)
  })
  which.max(counts)
}

find_header_row_with_codes <- function(raw) {
  counts <- apply(raw, 1, function(row) {
    sum(as.character(row) %in% a38_codes, na.rm = TRUE)
  })
  which.max(counts)
}

find_col_by_label <- function(raw, label_pattern) {
  raw_norm <- as.data.frame(lapply(raw, normalize_txt), stringsAsFactors = FALSE)
  target <- normalize_txt(label_pattern)
  pos <- which(apply(raw_norm, 2, function(col) {
    any(str_detect(col, fixed(target)))
  }))
  if (length(pos) == 0) stop(paste("Column label not found:", label_pattern))
  pos[1]
}

extract_tei <- function(path) {
  raw <- read_raw(path, find_sheet(path, "TEI"))
  code_col <- find_code_col(raw)
  header_row <- find_header_row_with_codes(raw)
  
  sector_cols <- which(as.character(unlist(raw[header_row, ])) %in% a38_codes)
  sector_codes_col <- as.character(unlist(raw[header_row, sector_cols]))
  
  row_idx <- which(as.character(raw[[code_col]]) %in% a38_codes)
  row_codes <- as.character(raw[[code_col]][row_idx])
  
  mat_raw <- raw[row_idx, sector_cols]
  mat <- apply(mat_raw, 2, as_num_zero)
  mat <- as.matrix(mat)
  
  rownames(mat) <- row_codes
  colnames(mat) <- sector_codes_col
  
  mat <- mat[a38_codes, a38_codes, drop = FALSE]
  mat[is.na(mat)] <- 0
  mat
}

extract_vector <- function(path, sheet_pattern, col_label) {
  raw <- read_raw(path, find_sheet(path, sheet_pattern))
  code_col <- find_code_col(raw)
  row_idx <- which(as.character(raw[[code_col]]) %in% a38_codes)
  row_codes <- as.character(raw[[code_col]][row_idx])
  value_col <- find_col_by_label(raw, col_label)
  values <- as_num_zero(raw[[value_col]][row_idx])
  
  out <- values
  names(out) <- row_codes
  out <- out[a38_codes]
  out[is.na(out)] <- 0
  out
}

aggregate_matrix_12 <- function(mat38) {
  map_vec <- sector_map$group12
  names(map_vec) <- sector_map$code
  
  row_groups <- map_vec[rownames(mat38)]
  col_groups <- map_vec[colnames(mat38)]
  
  mat_row <- rowsum(mat38, group = row_groups, reorder = FALSE)
  mat_agg <- t(rowsum(t(mat_row), group = col_groups, reorder = FALSE))
  mat_agg <- mat_agg[group_order$group12, group_order$group12, drop = FALSE]
  
  rownames(mat_agg) <- group_order$sector
  colnames(mat_agg) <- group_order$sector
  mat_agg
}

aggregate_vector_12 <- function(vec38) {
  map_vec <- sector_map$group12
  names(map_vec) <- sector_map$code
  groups <- map_vec[names(vec38)]
  out <- tapply(vec38, groups, sum, na.rm = TRUE)
  out <- out[group_order$group12]
  names(out) <- group_order$sector
  out[is.na(out)] <- 0
  out
}


# ============================================================
# 4. READ MULTI-YEAR FILES
# ============================================================

read_clean_multi_year_file <- function(path, value_name) {
  raw <- readxl::read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  
  raw_chr <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(raw_chr) <- paste0("col", seq_along(raw_chr))
  raw_chr[] <- lapply(raw_chr, as.character)
  
  header_row <- which(apply(raw_chr, 1, function(r) {
    sum(str_detect(str_trim(r), "^[0-9]{4}$"), na.rm = TRUE) >= 2
  }))[1]
  
  if (is.na(header_row)) {
    stop(paste("Could not find header row with year columns in:", path))
  }
  
  headers <- as.character(unlist(raw_chr[header_row, ]))
  headers <- str_trim(headers)
  headers[is.na(headers) | headers == ""] <- paste0("X", which(is.na(headers) | headers == ""))
  
  df <- raw_chr[(header_row + 1):nrow(raw_chr), , drop = FALSE]
  colnames(df) <- headers
  
  names(df)[1:2] <- c("Code", "Sector_Name")
  
  year_cols <- names(df)[str_detect(names(df), "^[0-9]{4}$")]
  year_cols <- year_cols[as.integer(year_cols) >= BASE_YEAR & as.integer(year_cols) <= END_YEAR]
  
  if (length(year_cols) == 0) {
    stop(paste0("No year columns ", BASE_YEAR, "-", END_YEAR, " found in:", path))
  }
  
  df %>%
    mutate(
      Code = as.character(Code),
      Code = str_replace_all(Code, "\u00A0", ""),
      Code = str_replace_all(Code, " ", ""),
      Code = str_replace_all(Code, "\\.", "_"),
      Code = str_replace_all(Code, "-", "_"),
      Code = str_trim(Code)
    ) %>%
    filter(!is.na(Code), Code != "", Code != "TOTAL") %>%
    select(Code, Sector_Name, all_of(year_cols)) %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = value_name
    ) %>%
    mutate(
      year = as.integer(year),
      "{value_name}" := as_num_fr(.data[[value_name]])
    )
}

aggregate_mixed_file_12_all <- function(path, value_name) {
  df <- read_clean_multi_year_file(path, value_name)
  
  missing_codes <- anti_join(macro_map_clean, df, by = "Code")
  if (nrow(missing_codes) > 0) {
    cat("\nWARNING: missing codes in", basename(path), "\n")
    print(missing_codes)
  }
  
  df %>%
    inner_join(macro_map_clean, by = "Code") %>%
    group_by(year, group12, sector) %>%
    summarise(
      value = sum(.data[[value_name]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename("{value_name}" := value) %>%
    arrange(year, group12)
}


# ============================================================
# 5. PROCESS ALL TES FILES
# ============================================================

process_tes_year <- function(path, year) {
  V38 <- extract_tei(path)
  
  C38 <- extract_vector(path, "TEF", "DEPENSE TOTALE")
  I38 <- extract_vector(path, "TEF", "FBCF TOTALE")
  X38 <- extract_vector(path, "TEF", "Exportations de biens et de services")
  
  Q38 <- extract_vector(path, "TRP", "Production des produits")
  M38 <- extract_vector(path, "TRP", "Importations de biens et de services")
  
  V12 <- aggregate_matrix_12(V38)
  C12 <- aggregate_vector_12(C38)
  I12 <- aggregate_vector_12(I38)
  X12 <- aggregate_vector_12(X38)
  Q12 <- aggregate_vector_12(Q38)
  M12 <- aggregate_vector_12(M38)
  
  IC_product <- rowSums(V12)
  IC_sector  <- colSums(V12)
  NX12 <- X12 - M12
  
  vectors <- tibble(
    year = year,
    sector = names(Q12),
    Q_nom_TES = as.numeric(Q12),
    M = as.numeric(M12),
    X = as.numeric(X12),
    NX = as.numeric(NX12),
    C = as.numeric(C12),
    I = as.numeric(I12),
    IC_product = as.numeric(IC_product),
    IC_sector = as.numeric(IC_sector),
    import_penetration = ifelse((Q12 + M12) > 0, M12 / (Q12 + M12), NA_real_),
    export_intensity = ifelse(Q12 > 0, X12 / Q12, NA_real_),
    trade_balance_ratio = ifelse(Q12 > 0, NX12 / Q12, NA_real_)
  )
  
  Omega_V <- sweep(V12, 2, colSums(V12), "/")
  Omega_V[, colSums(V12) == 0] <- 0
  Omega_V[is.na(Omega_V) | !is.finite(Omega_V)] <- 0
  
  validation <- tibble(
    year = year,
    object = c(
      "sum_omega_C",
      "sum_omega_I",
      "min_colsum_Omega_V",
      "max_colsum_Omega_V"
    ),
    value = c(
      sum(normalize_sum(C12), na.rm = TRUE),
      sum(normalize_sum(I12), na.rm = TRUE),
      min(colSums(Omega_V)),
      max(colSums(Omega_V))
    )
  )
  
  list(
    year = year,
    V12 = V12,
    Omega_V = Omega_V,
    vectors = vectors,
    validation = validation
  )
}

tes_results_all <- map2(tes_files$path, tes_files$year, process_tes_year)
names(tes_results_all) <- as.character(tes_files$year)

tes_vectors_all <- map_dfr(tes_results_all, "vectors")
tes_validation_all <- map_dfr(tes_results_all, "validation")


# ============================================================
# 6. CPR_CEB PRODUCTION ACCOUNT FOR ALL YEARS
# ============================================================

find_cpr_ceb_sheet <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheets_norm <- normalize_txt(sheets)
  idx <- which(
    sheets_norm == "cpr_ceb" |
      str_detect(sheets_norm, "cpr") |
      str_detect(sheets_norm, "ceb")
  )
  if (length(idx) == 0) stop("No CPR_CEB-like sheet found in: ", path)
  sheets[idx[1]]
}

extract_cpr_ceb_row_38 <- function(path, row_code, occurrence = 1) {
  sheet_name <- find_cpr_ceb_sheet(path)
  raw <- read_raw(path, sheet_name)
  header_row <- find_header_row_with_codes(raw)
  sector_cols <- which(as.character(unlist(raw[header_row, ])) %in% a38_codes)
  sector_codes <- as.character(unlist(raw[header_row, sector_cols]))
  
  row_idx_all <- which(apply(raw, 1, function(r) {
    vals <- as.character(r[1:min(6, length(r))])
    vals <- str_trim(vals)
    any(vals == row_code, na.rm = TRUE)
  }))
  
  if (length(row_idx_all) == 0) {
    stop(paste("Row code not found:", row_code, "in", path))
  }
  if (length(row_idx_all) < occurrence) {
    stop(paste("Occurrence not found for:", row_code, "in", path))
  }
  
  row_idx <- row_idx_all[occurrence]
  values <- as_num_zero(as.character(unlist(raw[row_idx, sector_cols])))
  names(values) <- sector_codes
  values <- values[a38_codes]
  values[is.na(values)] <- 0
  values
}

extract_cpr_ceb_account <- function(path, year_value) {
  P2_38  <- extract_cpr_ceb_row_38(path, "P2", occurrence = 1)
  B1g_38 <- extract_cpr_ceb_row_38(path, "B1g", occurrence = 1)
  P1_38  <- extract_cpr_ceb_row_38(path, "P1", occurrence = 1)
  
  tibble(
    year = year_value,
    sector = group_order$sector,
    P2_nom_CPR = as.numeric(aggregate_vector_12(P2_38)),
    VA_nom_CPR = as.numeric(aggregate_vector_12(B1g_38)),
    P1_nom_CPR = as.numeric(aggregate_vector_12(P1_38))
  ) %>%
    mutate(
      check_P1_minus_P2_VA = P1_nom_CPR - P2_nom_CPR - VA_nom_CPR,
      mu_CPR = ifelse(VA_nom_CPR != 0, P2_nom_CPR / VA_nom_CPR, NA_real_)
    )
}

cpr_ceb_12_all <- map2_dfr(tes_files$path, tes_files$year, extract_cpr_ceb_account)

cpr_check_all <- cpr_ceb_12_all %>%
  group_by(year) %>%
  summarise(
    total_P2 = sum(P2_nom_CPR, na.rm = TRUE),
    total_VA = sum(VA_nom_CPR, na.rm = TRUE),
    total_P1 = sum(P1_nom_CPR, na.rm = TRUE),
    total_account_gap = sum(check_P1_minus_P2_VA, na.rm = TRUE),
    max_abs_account_gap = max(abs(check_P1_minus_P2_VA), na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# 7. EMPLOYMENT, VA, PRICES — ALL YEARS
# ============================================================

employment_12_all <- aggregate_mixed_file_12_all(
  emploi_path,
  "employment_thousand"
)

va_nom_long_all <- read_clean_multi_year_file(
  va_nom_path,
  "VA_nominal_file"
)

indices_long_all <- read_clean_multi_year_file(
  indices_path,
  "price_index_2014_100"
)

va_price_detailed_all <- va_nom_long_all %>%
  select(Code, Sector_Name, year, VA_nominal_file) %>%
  inner_join(
    indices_long_all %>%
      select(Code, year, price_index_2014_100),
    by = c("Code", "year")
  ) %>%
  mutate(
    P_raw_2014 = price_index_2014_100 / 100,
    VA_real_file = VA_nominal_file / P_raw_2014
  )

missing_price_codes_all <- anti_join(
  macro_map_clean,
  va_price_detailed_all,
  by = "Code"
)

if (nrow(missing_price_codes_all) > 0) {
  cat("\nWARNING: missing codes in VA/price reconstruction input:\n")
  print(missing_price_codes_all)
}

va_price_12_file_all <- va_price_detailed_all %>%
  inner_join(macro_map_clean, by = "Code") %>%
  group_by(year, group12, sector) %>%
  summarise(
    VA_nominal_file_12 = sum(VA_nominal_file, na.rm = TRUE),
    VA_real_file_12 = sum(VA_real_file, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, group12)

va_cpr_12_all <- cpr_ceb_12_all %>%
  transmute(
    year,
    sector,
    VA_nominal_TES_CPR = VA_nom_CPR / 1000
  )

# Critical method:
# - aggregate nominal and real VA first
# - normalize nominal VA to CPR/TES level
# - infer implicit 12-sector prices
# - normalize each sector price to 2000 = 1
# - construct VA_real_2000base = VA_nominal / p_model
price_12_all <- va_price_12_file_all %>%
  left_join(
    va_cpr_12_all,
    by = c("year", "sector")
  ) %>%
  mutate(
    normalization_factor =
      ifelse(
        abs(VA_nominal_file_12) > 1e-12,
        VA_nominal_TES_CPR / VA_nominal_file_12,
        NA_real_
      ),
    
    VA_nominal_12_normalized =
      VA_nominal_file_12 * normalization_factor,
    
    VA_real_12_normalized =
      VA_real_file_12 * normalization_factor,
    
    P_implicit_12 =
      VA_nominal_12_normalized / VA_real_12_normalized
  ) %>%
  group_by(sector) %>%
  mutate(
    P_implicit_2000 =
      P_implicit_12[year == BASE_YEAR][1],
    
    p_model =
      P_implicit_12 / P_implicit_2000,
    
    VA_real_2000base =
      VA_nominal_12_normalized / p_model
  ) %>%
  ungroup() %>%
  arrange(year, group12)

price_reconstruction_check_all <- price_12_all %>%
  mutate(
    check_nominal_matches_CPR =
      VA_nominal_12_normalized - VA_nominal_TES_CPR,
    
    check_price_reconstruction =
      VA_nominal_12_normalized -
      P_implicit_12 * VA_real_12_normalized,
    
    check_2000base_reconstruction =
      VA_nominal_12_normalized -
      p_model * VA_real_2000base,
    
    check_2000_real_equals_nominal =
      ifelse(
        year == BASE_YEAR,
        VA_real_2000base - VA_nominal_12_normalized,
        NA_real_
      )
  ) %>%
  group_by(year) %>%
  summarise(
    n_sectors = n(),
    total_VA_nominal_TES_CPR =
      sum(VA_nominal_TES_CPR, na.rm = TRUE),
    total_VA_nominal_12_normalized =
      sum(VA_nominal_12_normalized, na.rm = TRUE),
    total_VA_real_2000base =
      sum(VA_real_2000base, na.rm = TRUE),
    max_abs_nominal_CPR_gap =
      max(abs(check_nominal_matches_CPR), na.rm = TRUE),
    max_abs_price_reconstruction_gap =
      max(abs(check_price_reconstruction), na.rm = TRUE),
    max_abs_2000base_reconstruction_gap =
      max(abs(check_2000base_reconstruction), na.rm = TRUE),
    max_abs_2000_real_equals_nominal_gap =
      max(abs(check_2000_real_equals_nominal), na.rm = TRUE),
    min_p_model = min(p_model, na.rm = TRUE),
    max_p_model = max(p_model, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# 8. CORE DATA PANEL
# ============================================================

core_data_all <- tes_vectors_all %>%
  left_join(
    employment_12_all %>%
      select(year, sector, employment_thousand),
    by = c("year", "sector")
  ) %>%
  left_join(
    price_12_all %>%
      select(
        year,
        sector,
        VA_nominal_TES_CPR,
        VA_real_12_normalized,
        VA_real_2000base,
        P_implicit_12,
        P_implicit_2000,
        p_model,
        normalization_factor
      ),
    by = c("year", "sector")
  ) %>%
  group_by(year) %>%
  mutate(
    lambda_L =
      employment_thousand / sum(employment_thousand, na.rm = TRUE),
    
    lambda_VA_nom_CPR =
      VA_nominal_TES_CPR / sum(VA_nominal_TES_CPR, na.rm = TRUE),
    
    lambda_VA_real_normalized =
      VA_real_12_normalized / sum(VA_real_12_normalized, na.rm = TRUE),
    
    lambda_VA_real_2000base =
      VA_real_2000base / sum(VA_real_2000base, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, match(sector, sector_names))

get_core_year_all <- function(year_value) {
  core_data_all %>%
    filter(year == year_value) %>%
    arrange(match(sector, sector_names))
}


# ============================================================
# 9. VALIDATION PRINTS
# ============================================================

merge_check_all <- core_data_all %>%
  group_by(year) %>%
  summarise(
    n_sectors = n(),
    missing_L = sum(is.na(employment_thousand)),
    missing_VA_nom_CPR = sum(is.na(VA_nominal_TES_CPR)),
    missing_VA_real_2000base = sum(is.na(VA_real_2000base)),
    missing_price = sum(is.na(p_model)),
    
    total_L_thousand =
      sum(employment_thousand, na.rm = TRUE),
    
    total_VA_nom_CPR_billion =
      sum(VA_nominal_TES_CPR, na.rm = TRUE),
    
    total_VA_real_2000base_billion =
      sum(VA_real_2000base, na.rm = TRUE),
    
    sum_lambda_L =
      sum(lambda_L, na.rm = TRUE),
    
    sum_lambda_VA_nom_CPR =
      sum(lambda_VA_nom_CPR, na.rm = TRUE),
    
    sum_lambda_VA_real_2000base =
      sum(lambda_VA_real_2000base, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("\n==================== DYNAMIC D1 — TES FILES ====================\n")
print(tes_files, n = Inf)

cat("\n==================== DYNAMIC D1 — MERGE CHECK ALL YEARS ====================\n")
print(round_numeric_df(merge_check_all, 10), n = Inf, width = Inf)

cat("\n==================== DYNAMIC D1 — PRICE RECONSTRUCTION CHECK ALL YEARS ====================\n")
print(round_numeric_df(price_reconstruction_check_all, 10), n = Inf, width = Inf)

cat("\n==================== DYNAMIC D1 — CPR CHECK ALL YEARS ====================\n")
print(round_numeric_df(cpr_check_all, 10), n = Inf, width = Inf)

cat("\n==================== DYNAMIC D1 — TES VALIDATION ALL YEARS ====================\n")
print(round_numeric_df(tes_validation_all, 10), n = Inf, width = Inf)


# ============================================================
# 10. EXPORTS
# ============================================================

write_xlsx(
  list(
    core_data_all = core_data_all,
    merge_check_all = merge_check_all,
    price_reconstruction_check_all = price_reconstruction_check_all,
    cpr_check_all = cpr_check_all,
    tes_validation_all = tes_validation_all
  ),
  path = file.path(dyn_dir, "DYNAMIC_D1_core_data_all_checks.xlsx")
)

cat("\nDYNAMIC BLOCK D1 DONE.\n")
cat("Output folder:\n", dyn_dir, "\n")










# ============================================================
# DYNAMIC BLOCK D2 — ANNUAL VIABILITY BENCHMARK AND REAL GAPS
# FINAL SECTOR-IC VERSION
# France 2000–2021
#
# Logic:
# - Use the same recursive benchmark logic as the static analysis
# - Do NOT build a new benchmark
# - Deflate TES flows by sectoral p_model before comparing to benchmark
# - IC is treated functionally and sectorally, like investment:
#   a good is IC or K depending on its use, not by nature.
#   IC is the limiting case with full within-period depreciation.
#
# Input from D1:
# - core_data_all
# - sector_names
# - dyn_dir
#
# Output:
# - core_data_real_all
# - dynamic_observed_macro
# - dynamic_benchmark_macro
# - dynamic_macro_gaps
# - dynamic_benchmark_sector
# - dynamic_sector_gaps
# - dynamic_group_gaps
# ============================================================


# ============================================================
# 0. SAFETY CHECKS
# ============================================================

required_objects_D2 <- c(
  "core_data_all",
  "sector_names",
  "dyn_dir"
)

missing_objects_D2 <- required_objects_D2[
  !sapply(required_objects_D2, exists)
]

if (length(missing_objects_D2) > 0) {
  stop(
    paste0(
      "DYNAMIC BLOCK D2 ERROR — missing objects: ",
      paste(missing_objects_D2, collapse = ", "),
      "\nRun Dynamic Block D1 first."
    )
  )
}

years_dyn <- sort(unique(core_data_all$year))

if (!all(BASE_YEAR:END_YEAR %in% years_dyn)) {
  stop(paste0("D2 ERROR — core_data_all manque des annees ", BASE_YEAR, "-", END_YEAR, "."))
}

base_year <- BASE_YEAR
end_year  <- END_YEAR
T_horizon <- end_year - base_year


# ============================================================
# 1. BUILD REAL OBSERVED FLOWS
# ============================================================
# Important:
# TES flows C, I, IC, X, M are nominal flows.
# We deflate them by sectoral p_model to express them at 2000 prices.
#
# IC convention:
# - IC_product_real is kept only as an audit variable.
# - The paper uses IC_sector_real, i.e. IC by user sector.
# - This is consistent with investment: goods are classified by use.
# ============================================================

core_data_real_all <- core_data_all %>%
  mutate(
    # TES monetary variables converted from million to billion euros
    C_nom_bn = C / 1000,
    I_nom_bn = I / 1000,
    
    IC_product_nom_bn = IC_product / 1000,
    IC_sector_nom_bn  = IC_sector / 1000,
    
    X_nom_bn = X / 1000,
    M_nom_bn = M / 1000,
    NX_nom_bn = NX / 1000,
    
    # Product-side real flows at 2000 prices
    C_real = C_nom_bn / p_model,
    I_real = I_nom_bn / p_model,
    
    # Kept for audit only
    IC_product_real = IC_product_nom_bn / p_model,
    
    # Main convention for the paper: sector-user-side IC
    IC_sector_real = IC_sector_nom_bn / p_model,
    
    X_real = X_nom_bn / p_model,
    M_real = M_nom_bn / p_model,
    NX_real = X_real - M_real,
    
    VA_nominal_obs = VA_nominal_TES_CPR,
    VA_real_obs = VA_real_2000base,
    employment_obs = employment_thousand
  )


# ============================================================
# 2. OBSERVED MACRO PANEL
# ============================================================

dynamic_observed_macro <- core_data_real_all %>%
  group_by(year) %>%
  summarise(
    VA_nominal_obs = sum(VA_nominal_obs, na.rm = TRUE),
    VA_real_obs = sum(VA_real_obs, na.rm = TRUE),
    employment_obs = sum(employment_obs, na.rm = TRUE),
    
    C_obs = sum(C_real, na.rm = TRUE),
    I_obs = sum(I_real, na.rm = TRUE),
    
    # Main convention: IC by sector of use
    IC_obs = sum(IC_sector_real, na.rm = TRUE),
    
    # Audit only
    IC_product_obs_audit = sum(IC_product_real, na.rm = TRUE),
    
    X_obs = sum(X_real, na.rm = TRUE),
    M_obs = sum(M_real, na.rm = TRUE),
    NX_obs = sum(NX_real, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    VA_deflator_obs = VA_nominal_obs / VA_real_obs,
    
    output_per_unit_I_obs = VA_real_obs / I_obs,
    
    C_to_VA_obs = C_obs / VA_real_obs,
    I_to_VA_obs = I_obs / VA_real_obs,
    IC_to_VA_obs = IC_obs / VA_real_obs,
    NX_to_VA_obs = NX_obs / VA_real_obs,
    
    IC_product_to_VA_audit =
      IC_product_obs_audit / VA_real_obs
  )


# ============================================================
# 3. RECONSTRUCT THE SAME RECURSIVE BENCHMARK
# ============================================================
# The benchmark is not new.
# It is the annual path implied by the same endpoint discipline:
# - observed aggregate employment growth 2000–2021
# - observed aggregate labour-productivity growth 2000–2021
# - 2000 expenditure ratios
# - benchmark aggregate scale matches 2021 observed scale
# ============================================================

Y0_real <- dynamic_observed_macro$VA_real_obs[
  dynamic_observed_macro$year == base_year
]

Y_end_real <- dynamic_observed_macro$VA_real_obs[
  dynamic_observed_macro$year == end_year
]

L0 <- dynamic_observed_macro$employment_obs[
  dynamic_observed_macro$year == base_year
]

L_end <- dynamic_observed_macro$employment_obs[
  dynamic_observed_macro$year == end_year
]

n_dyn <- (L_end / L0)^(1 / T_horizon) - 1

g_dyn <- (
  (Y_end_real / L_end) /
    (Y0_real / L0)
)^(1 / T_horizon) - 1

phi_dyn <- (1 + n_dyn) * (1 + g_dyn) - 1

C0_real <- dynamic_observed_macro$C_obs[
  dynamic_observed_macro$year == base_year
]

I0_real <- dynamic_observed_macro$I_obs[
  dynamic_observed_macro$year == base_year
]

IC0_real <- dynamic_observed_macro$IC_obs[
  dynamic_observed_macro$year == base_year
]

NX0_real <- dynamic_observed_macro$NX_obs[
  dynamic_observed_macro$year == base_year
]

s_C_2000 <- C0_real / Y0_real
s_I_2000 <- I0_real / Y0_real
s_IC_2000 <- IC0_real / Y0_real
s_NX_2000 <- NX0_real / Y0_real

dynamic_benchmark_macro <- tibble(
  year = years_dyn,
  tau = year - base_year
) %>%
  mutate(
    employment_B = L0 * (1 + n_dyn)^tau,
    VA_real_B = Y0_real * (1 + phi_dyn)^tau,
    
    # Benchmark real absorption ratios inherited from 2000
    C_B = s_C_2000 * VA_real_B,
    I_B = s_I_2000 * VA_real_B,
    IC_B = s_IC_2000 * VA_real_B,
    NX_B = s_NX_2000 * VA_real_B,
    
    output_per_unit_I_B = VA_real_B / I_B,
    
    C_to_VA_B = C_B / VA_real_B,
    I_to_VA_B = I_B / VA_real_B,
    IC_to_VA_B = IC_B / VA_real_B,
    NX_to_VA_B = NX_B / VA_real_B
  ) %>%
  left_join(
    dynamic_observed_macro %>%
      select(year, VA_deflator_obs),
    by = "year"
  ) %>%
  mutate(
    VA_deflator_B = VA_deflator_obs,
    VA_nominal_B = VA_real_B * VA_deflator_B
  ) %>%
  select(
    year,
    tau,
    employment_B,
    VA_real_B,
    VA_nominal_B,
    VA_deflator_B,
    C_B,
    I_B,
    IC_B,
    NX_B,
    output_per_unit_I_B,
    C_to_VA_B,
    I_to_VA_B,
    IC_to_VA_B,
    NX_to_VA_B
  )


# ============================================================
# 4. MACRO GAPS
# ============================================================

dynamic_macro_gaps <- dynamic_observed_macro %>%
  left_join(
    dynamic_benchmark_macro,
    by = "year"
  ) %>%
  transmute(
    year,
    
    VA_nominal_obs,
    VA_nominal_B,
    gap_VA_nominal = VA_nominal_obs - VA_nominal_B,
    
    VA_real_obs,
    VA_real_B,
    gap_VA_real = VA_real_obs - VA_real_B,
    gap_VA_real_pct = 100 * (VA_real_obs / VA_real_B - 1),
    
    employment_obs,
    employment_B,
    gap_employment = employment_obs - employment_B,
    gap_employment_pct = 100 * (employment_obs / employment_B - 1),
    
    C_obs,
    C_B,
    gap_C = C_obs - C_B,
    
    I_obs,
    I_B,
    gap_I = I_obs - I_B,
    
    IC_obs,
    IC_B,
    gap_IC = IC_obs - IC_B,
    
    # Audit variable, not used in text unless needed
    IC_product_obs_audit,
    
    NX_obs,
    NX_B,
    gap_NX = NX_obs - NX_B,
    
    output_per_unit_I_obs,
    output_per_unit_I_B,
    gap_output_per_unit_I_pct =
      100 * (output_per_unit_I_obs / output_per_unit_I_B - 1),
    
    C_to_VA_obs,
    C_to_VA_B,
    gap_C_to_VA = C_to_VA_obs - C_to_VA_B,
    
    I_to_VA_obs,
    I_to_VA_B,
    gap_I_to_VA = I_to_VA_obs - I_to_VA_B,
    
    IC_to_VA_obs,
    IC_to_VA_B,
    gap_IC_to_VA = IC_to_VA_obs - IC_to_VA_B,
    
    NX_to_VA_obs,
    NX_to_VA_B,
    gap_NX_to_VA = NX_to_VA_obs - NX_to_VA_B
  )


# ============================================================
# 5. SECTOR BENCHMARK
# ============================================================

core_2000 <- core_data_real_all %>%
  filter(year == base_year) %>%
  arrange(match(sector, sector_names))

lambda_L_2000 <- core_2000$employment_obs /
  sum(core_2000$employment_obs, na.rm = TRUE)

lambda_real_2000 <- core_2000$VA_real_obs /
  sum(core_2000$VA_real_obs, na.rm = TRUE)

lambda_nom_2000 <- core_2000$VA_nominal_obs /
  sum(core_2000$VA_nominal_obs, na.rm = TRUE)

# Main convention:
# sector-side intermediate-input intensity at 2000 prices
mu_IC_sector_2000 <- core_2000$IC_sector_real / core_2000$VA_real_obs
mu_IC_sector_2000[!is.finite(mu_IC_sector_2000)] <- 0

sector_base_weights <- tibble(
  sector = sector_names,
  lambda_L_2000 = as.numeric(lambda_L_2000),
  lambda_real_2000 = as.numeric(lambda_real_2000),
  lambda_nom_2000 = as.numeric(lambda_nom_2000),
  mu_IC_sector_2000 = as.numeric(mu_IC_sector_2000)
)

dynamic_benchmark_sector <- tidyr::expand_grid(
  year = years_dyn,
  sector = sector_names
) %>%
  left_join(sector_base_weights, by = "sector") %>%
  left_join(dynamic_benchmark_macro, by = "year") %>%
  mutate(
    employment_B_sector = lambda_L_2000 * employment_B,
    VA_real_B_sector = lambda_real_2000 * VA_real_B,
    VA_nominal_B_sector = lambda_nom_2000 * VA_nominal_B,
    
    # Sector-side IC benchmark
    IC_B_sector = mu_IC_sector_2000 * VA_real_B_sector
  ) %>%
  select(
    year,
    sector,
    employment_B_sector,
    VA_real_B_sector,
    VA_nominal_B_sector,
    IC_B_sector,
    lambda_L_2000,
    lambda_real_2000,
    lambda_nom_2000,
    mu_IC_sector_2000
  )


# ============================================================
# 6. SECTOR GAPS
# ============================================================

base_import_penetration <- core_data_real_all %>%
  filter(year == base_year) %>%
  select(
    sector,
    import_penetration_base = import_penetration
  )

dynamic_sector_gaps <- core_data_real_all %>%
  transmute(
    year,
    sector,
    
    employment_obs,
    VA_nominal_obs,
    VA_real_obs,
    
    C_obs = C_real,
    I_obs = I_real,
    
    # Main convention: sector-side IC
    IC_obs = IC_sector_real,
    
    # Audit only
    IC_product_obs_audit = IC_product_real,
    
    NX_obs = NX_real,
    X_obs = X_real,
    M_obs = M_real,
    
    p_model,
    import_penetration_obs = import_penetration,
    trade_balance_ratio_obs = trade_balance_ratio
  ) %>%
  left_join(
    dynamic_benchmark_sector,
    by = c("year", "sector")
  ) %>%
  left_join(
    base_import_penetration,
    by = "sector"
  ) %>%
  mutate(
    gap_employment = employment_obs - employment_B_sector,
    gap_VA_real = VA_real_obs - VA_real_B_sector,
    gap_VA_nominal = VA_nominal_obs - VA_nominal_B_sector,
    gap_IC = IC_obs - IC_B_sector,
    gap_import_penetration =
      import_penetration_obs - import_penetration_base
  ) %>%
  arrange(year, match(sector, sector_names))


# ============================================================
# 7. GROUP GAPS
# ============================================================

sector_grouping <- tibble(
  sector = sector_names,
  diagnostic_group = case_when(
    sector %in% c(
      "Agriculture and food",
      "Extractive, energy and utilities",
      "Traditional manufacturing",
      "Chemicals and materials",
      "Machinery, equipment and transport equipment"
    ) ~ "Exposed productive sectors",
    
    sector %in% c(
      "Trade, transport and hospitality",
      "Construction"
    ) ~ "Fragile absorbers",
    
    sector %in% c(
      "Information and communication",
      "Business services"
    ) ~ "Productive internal services",
    
    sector %in% c(
      "Finance and real estate",
      "Public, education and health-social services",
      "Cultural, personal and household services"
    ) ~ "Asset and institutional sectors",
    
    TRUE ~ "Other"
  )
)

dynamic_group_gaps <- dynamic_sector_gaps %>%
  left_join(sector_grouping, by = "sector") %>%
  group_by(year, diagnostic_group) %>%
  summarise(
    gap_VA_nominal = sum(gap_VA_nominal, na.rm = TRUE),
    gap_VA_real = sum(gap_VA_real, na.rm = TRUE),
    gap_employment = sum(gap_employment, na.rm = TRUE),
    gap_IC = sum(gap_IC, na.rm = TRUE),
    
    avg_import_penetration =
      weighted.mean(
        import_penetration_obs,
        w = pmax(VA_nominal_obs, 0),
        na.rm = TRUE
      ),
    
    avg_gap_import_penetration =
      weighted.mean(
        gap_import_penetration,
        w = pmax(VA_nominal_obs, 0),
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ============================================================
# 8. VALIDATION CHECKS
# ============================================================

dynamic_validation_D2 <- dynamic_sector_gaps %>%
  group_by(year) %>%
  summarise(
    sum_sector_gap_VA_nominal =
      sum(gap_VA_nominal, na.rm = TRUE),
    macro_gap_VA_nominal =
      dynamic_macro_gaps$gap_VA_nominal[
        match(year[1], dynamic_macro_gaps$year)
      ],
    reconstruction_gap_VA_nominal =
      sum_sector_gap_VA_nominal - macro_gap_VA_nominal,
    
    sum_sector_gap_VA_real =
      sum(gap_VA_real, na.rm = TRUE),
    macro_gap_VA_real =
      dynamic_macro_gaps$gap_VA_real[
        match(year[1], dynamic_macro_gaps$year)
      ],
    reconstruction_gap_VA_real =
      sum_sector_gap_VA_real - macro_gap_VA_real,
    
    sum_sector_gap_employment =
      sum(gap_employment, na.rm = TRUE),
    macro_gap_employment =
      dynamic_macro_gaps$gap_employment[
        match(year[1], dynamic_macro_gaps$year)
      ],
    reconstruction_gap_employment =
      sum_sector_gap_employment - macro_gap_employment,
    
    sum_sector_gap_IC =
      sum(gap_IC, na.rm = TRUE),
    macro_gap_IC =
      dynamic_macro_gaps$gap_IC[
        match(year[1], dynamic_macro_gaps$year)
      ],
    reconstruction_gap_IC =
      sum_sector_gap_IC - macro_gap_IC,
    
    .groups = "drop"
  )

endpoint_check_D2 <- dynamic_macro_gaps %>%
  filter(year %in% c(BASE_YEAR, END_YEAR)) %>%
  select(
    year,
    VA_real_obs,
    VA_real_B,
    gap_VA_real,
    employment_obs,
    employment_B,
    gap_employment,
    C_obs,
    C_B,
    gap_C,
    I_obs,
    I_B,
    gap_I,
    IC_obs,
    IC_B,
    gap_IC,
    NX_obs,
    NX_B,
    gap_NX,
    output_per_unit_I_obs,
    output_per_unit_I_B,
    gap_output_per_unit_I_pct
  )


# ============================================================
# 9. EXPORTS
# ============================================================

write_xlsx(
  list(
    core_data_real_all = core_data_real_all,
    dynamic_observed_macro = dynamic_observed_macro,
    dynamic_benchmark_macro = dynamic_benchmark_macro,
    dynamic_macro_gaps = dynamic_macro_gaps,
    dynamic_benchmark_sector = dynamic_benchmark_sector,
    dynamic_sector_gaps = dynamic_sector_gaps,
    dynamic_group_gaps = dynamic_group_gaps,
    dynamic_validation_D2 = dynamic_validation_D2,
    endpoint_check_D2 = endpoint_check_D2
  ),
  path = file.path(
    dyn_dir,
    "DYNAMIC_D2_final_sector_IC_benchmark_and_gaps.xlsx"
  )
)


# ============================================================
# 10. PRINTS
# ============================================================

cat("\n==================== DYNAMIC D2 FINAL — PARAMETERS ====================\n")
cat("n_dyn   =", round(n_dyn, 8), "\n")
cat("g_dyn   =", round(g_dyn, 8), "\n")
cat("phi_dyn =", round(phi_dyn, 8), "\n")

cat("\n==================== DYNAMIC D2 FINAL — ENDPOINT CHECK ====================\n")
print(round_numeric_df(endpoint_check_D2, 4), n = Inf, width = Inf)

cat("\n==================== DYNAMIC D2 FINAL — MACRO GAPS ====================\n")
print(round_numeric_df(dynamic_macro_gaps, 4), n = Inf, width = Inf)

cat("\n==================== DYNAMIC D2 FINAL — GROUP GAPS (annee de fin) ====================\n")
print(
  dynamic_group_gaps %>%
    filter(year == END_YEAR) %>%
    round_numeric_df(4),
  n = Inf,
  width = Inf
)




