# Combined and Refactored R Script

# --- I. SETUP ---

# Clear the environment (optional: uncomment if needed)
rm(list = ls())

# --- START OF MODIFIED CODE FOR OS-DEPENDENT WORKING DIRECTORY ---
# Set working directory based on OS
if (Sys.info()['sysname'] == "Windows") {
  setwd("C:\\Users\\WKSTN\\OneDrive - University of Warwick\\Macroprudential Paper October 24\\2025\\r_output")
  cat("Operating System: Windows. Working directory set to Windows path.\n")
} else if (Sys.info()['sysname'] == "Darwin") { # Darwin is for macOS
  setwd("/Users/ciaran/Library/CloudStorage/OneDrive-UniversityofWarwick/Macroprudential Paper October 24/2025/r_output")
  cat("Operating System: macOS (Darwin). Working directory set to macOS path.\n")
} else {
  cat("Warning: Operating system not Windows or macOS. Manually set working directory if needed.\n")
  cat("Current R working directory:", getwd(), "\n")
}
# The script assumes 'demgov_int.xlsx', 'demgov_eur.xlsx', and 'auth.xlsx' are in this directory.
# --- END OF MODIFIED CODE FOR OS-DEPENDENT WORKING DIRECTORY ---

# Install packages if not already installed (run once if needed)
# if (!requireNamespace("devtools", quietly = TRUE)) { install.packages("devtools") }
# if (!requireNamespace("vdemdata", quietly = TRUE)) { devtools::install_github("vdeminstitute/vdemdata", quiet = TRUE) }
# if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
# if (!requireNamespace("tidyr", quietly = TRUE)) { install.packages("tidyr") }
# if (!requireNamespace("tibble", quietly = TRUE)) { install.packages("tibble") }
# if (!requireNamespace("ggplot2", quietly = TRUE)) { install.packages("ggplot2") }
# if (!requireNamespace("ggrepel", quietly = TRUE)) { install.packages("ggrepel") }
# if (!requireNamespace("readxl", quietly = TRUE)) { install.packages("readxl") }
# if (!requireNamespace("forcats", quietly = TRUE)) { install.packages("forcats") }
# if (!requireNamespace("plotrix", quietly = TRUE)) { install.packages("plotrix") }
# if (!requireNamespace("ggthemes", quietly = TRUE)) { install.packages("ggthemes") }
# if (!requireNamespace("scales", quietly = TRUE)) { install.packages("scales") }
# if (!requireNamespace("corrplot", quietly = TRUE)) { install.packages("corrplot") }
# if (!requireNamespace("svglite", quietly = TRUE)) { install.packages("svglite") }


# Load libraries
library(devtools) # For install_github if vdemdata is not installed via CRAN
library(vdemdata)
library(dplyr)
library(tidyr) # For gather/pivot_longer
library(tibble) # For tribble
library(ggplot2)
library(ggrepel) # For geom_text_repel
library(readxl) # For read_excel
library(forcats) # For factor manipulation
library(plotrix) # For plotrix::radial.plot (though not explicitly used in ggplot sections)
library(ggthemes) # For ggplot2 themes
library(scales) # For scales in ggplot2 (e.g., percent_format)
library(corrplot) # For correlation plots
library(svglite) # For saving plots as SVG
library(zoo)    # For rollmean()


# Define standard plot saving parameters
academic_single_width <- 7 # inches (Block 3 definition)
academic_single_height <- 5 # inches (Block 3 definition)
academic_wide_width <- 10 # inches (Block 3 definition)
academic_very_wide_width <- 12 # inches (Block 3 definition)
academic_plot_dpi <- 300 # Dots per inch

cat("Setup complete. Libraries loaded and parameters set.\n")

# --- II. HELPER FUNCTION DEFINITIONS ---

# Function 1: Aggregate sub-indicators
aggregate_indicators <- function(df) {
  df %>%
    transmute(
      network = network, # Ensure network column is present
      in_part = in_part,
      in_rep = in_rep1 + in_rep2 + in_rep3 + in_rep4,
      in_shb = in_shb1 + in_shb2,
      in_epi = in_epi,
      through_tr = through_tr1 + through_tr2 + through_tr3 + through_tr4,
      through_leg = through_leg,
      through_acc = through_acc1 + through_acc2 + through_acc3 + through_acc4,
      through_reas = through_reas1 + through_reas2,
      out_effec = out_effec,
      out_effic = out_effic
      # Add 'region' here if it's present in the raw_df and needs to be carried through this step
      # However, 'region' is typically added later for the combined dataset in this workflow.
    )
}

# Function 2: Scale indicators to 0-1 range
scale_indicators <- function(df_aggregated) {
  df_aggregated %>%
    mutate(
      in_part01 = in_part,
      in_rep01 = round(in_rep / 4, digits = 2),
      in_shb01 = round(in_shb / 2, digits = 2),
      in_epi01 = in_epi,
      through_tr01 = round(through_tr / 4, digits = 2),
      through_leg01 = through_leg,
      through_acc01 = round(through_acc / 4, digits = 2),
      through_reas01 = round(through_reas / 2, digits = 2),
      out_effec01 = out_effec,
      out_effic01 = out_effic
    )
}

# Function 3: Convert scaled indicators to long format
pivot_scaled_indicators_long <- function(df_scaled_wide) {
  cols_to_keep <- "network"
  if ("region" %in% names(df_scaled_wide)) { # Keep region if it was added before pivoting
    cols_to_keep <- c(cols_to_keep, "region")
  }
  
  df_scaled_wide %>%
    select(all_of(cols_to_keep), ends_with("01")) %>%
    pivot_longer(cols = ends_with("01"), 
                 names_to = "indicator", 
                 values_to = "value") %>%
    mutate(indicator = factor(indicator)) # Convert to factor
}

# Function 4: Add governance dimension column
add_governance_dimension <- function(df_long) {
  df_long %>%
    mutate(dimension = factor(case_when(
      indicator %in% c("in_part01", "in_rep01", "in_shb01", "in_epi01") ~ "input",
      indicator %in% c("through_tr01", "through_leg01", "through_acc01", "through_reas01") ~ "throughput",
      indicator %in% c("out_effec01", "out_effic01") ~ "output"
    ), levels = c("input", "throughput", "output")))
}

# Function 5: Add 'region' column based on network name
add_region_column <- function(df_input) { # Changed df_long to df_input
  df_input %>%
    mutate(region = factor(case_when(
      network %in% c("BCBS", "IADI", "IAIS", "IOPS", "CPMI", "IOSCO", "FSB", "BIS", "NGFS", "IFRS") ~ "Global",
      network %in% c("EIOPA", "EBA", "ESMA", "ESRB", "SSB", "CEBS", "CEIOPS", "CESR") ~ "European"
    ), levels = c("Global", "European")))
}

# Function 6: Calculate average dimension score per network (and optionally region)
calculate_average_dimension_score <- function(df_with_dimensions) {
  grouping_vars <- "network" # Start with network
  if ("region" %in% names(df_with_dimensions)) {
    grouping_vars <- c(grouping_vars, "region")
  }
  grouping_vars <- c(grouping_vars, "dimension") # Always group by dimension too
  
  df_with_dimensions %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(dim_value = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
}

# Function 7: Calculate total democratic governance score per network
calculate_dem_gov_score <- function(df_long_scaled_with_region_and_dim) {
  # Assumes df_long_scaled_with_region_and_dim is the data frame like 'dataTGN2_long_calculated'
  # which has 'network', 'value' (scaled indicator values), and 'region'
  if ("region" %in% names(df_long_scaled_with_region_and_dim)) {
    df_long_scaled_with_region_and_dim %>%
      group_by(network, region) %>% # Group by region as well
      summarise(dem_gov = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
  } else { # For international-only data (like mydata4)
    df_long_scaled_with_region_and_dim %>% # region column might not exist
      group_by(network) %>%
      summarise(dem_gov = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
  }
}

# Function 8: Categorize democratic governance score
categorize_dem_gov_score <- function(df_with_dem_gov) {
  df_with_dem_gov %>%
    mutate(score = factor(case_when(
      dem_gov < 0.4 ~ "low",
      dem_gov >= 0.4 & dem_gov <= 0.59 ~ "medium",
      dem_gov > 0.59 ~ "high"
    ), levels = c("low", "medium", "high")))
}

# Function 9: Master processing function for governance data
process_governance_data <- function(raw_df, dataset_name, is_combined_data = FALSE) {
  cat("\nProcessing dataset:", dataset_name, "\n")
  
  df_aggregated <- aggregate_indicators(raw_df)
  cat("Dimensions after aggregate_indicators: ", paste(dim(df_aggregated)), "\n")
  
  df_scaled_wide <- scale_indicators(df_aggregated)
  cat("Dimensions after scale_indicators: ", paste(dim(df_scaled_wide)), "\n")
  
  # Add region column for combined data BEFORE pivoting, if it's not already there from raw_df
  if (is_combined_data) {
    if(!("region" %in% names(df_scaled_wide))) { # Check if region needs to be added
      df_scaled_wide <- add_region_column(df_scaled_wide)
      cat("Dimensions after add_region_column: ", paste(dim(df_scaled_wide)), "\n")
    }
  }
  
  df_long_scaled <- pivot_scaled_indicators_long(df_scaled_wide)
  cat("Dimensions after pivot_scaled_indicators_long: ", paste(dim(df_long_scaled)), "\n")
  
  df_with_dimensions <- add_governance_dimension(df_long_scaled)
  cat("Dimensions after add_governance_dimension: ", paste(dim(df_with_dimensions)), "\n")
  
  # Avg dimension score
  # calculate_average_dimension_score groups by network, dimension and region (if present)
  df_avg_dim_score <- calculate_average_dimension_score(df_with_dimensions)
  cat("Dimensions for avg_dim_score (e.g. mydata3.1_long or part of dataTGN2.1_long): ", paste(dim(df_avg_dim_score)), "\n")
  
  # Dem gov score
  # calculate_dem_gov_score groups by network and region (if present)
  df_dem_gov <- calculate_dem_gov_score(df_with_dimensions) # df_with_dimensions has region if is_combined_data
  df_dem_gov_categorized <- categorize_dem_gov_score(df_dem_gov)
  cat("Dimensions for dem_gov_categorized (e.g. mydata4 or dataTGN3_calculated): ", paste(dim(df_dem_gov_categorized)), "\n")
  
  results <- list(
    data_long_scaled_with_dimensions = df_with_dimensions, # e.g., mydata3_long, dataTGN2_long_calculated
    data_avg_dim_scores = df_avg_dim_score, # e.g., mydata3.1_long, part of dataTGN2.1_long_calculated
    data_dem_gov_categorized = df_dem_gov_categorized # e.g., mydata4, dataTGN3_calculated
  )
  
  # For combined data, also create the summary by region AND dimension (like dataTGN2.1_long_calculated)
  # and proportions (like dataTGN4_calculated)
  if (is_combined_data) {
    results$data_avg_dim_scores_by_region_and_dim <- df_avg_dim_score # This already includes region if present
    
    results$data_proportions_by_region_score <- df_dem_gov_categorized %>%
      group_by(region, score) %>%
      summarise(n = n(), .groups = 'drop') %>%
      mutate(prop = n / sum(n, na.rm=TRUE)) # Ensure sum(n) is correct if only one group
    cat("Dimensions for data_proportions_by_region_score (e.g. dataTGN4_calculated): ", paste(dim(results$data_proportions_by_region_score)), "\n")
  }
  
  return(results)
}


# Function 10: Process auth data (for Appendix 4) - Reflects Block 3 logic (pool/5)
process_auth_data <- function(auth_df_raw) {
  auth_df <- auth_df_raw %>%
    mutate(
      del_calc = del_body * ((del_budget + del_org + del_pol) / 3),
      pool_calc = ((pool_memb * pool_vote) + pool_bind) / 2,
      del = round(del_calc, digits = 2),
      pool = round(pool_calc, digits = 2)
    ) %>%
    mutate(
      del01 = round(del / 4, digits = 2), # Assuming max del is 4
      pool01 = round(pool / 5, digits = 2) # CORRECTED: Max pool is 5 as per Block 3
    ) %>%
    select(network, del, pool, del01, pool01)
  return(auth_df)
}

# Function 11: Identify outliers (utility function from Block 1/3)
is_outlier <- function(x, na.rm = TRUE) {
  if (!is.numeric(x) || all(is.na(x))) return(rep(FALSE, length(x)))
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  if(any(is.na(qnt))) return(rep(FALSE, length(x))) # Handle cases with insufficient non-NA data
  H <- 1.5 * IQR(x, na.rm = na.rm)
  if(is.na(H)) return(rep(FALSE, length(x))) # Handle if IQR is NA
  (x < (qnt[1] - H)) | (x > (qnt[2] + H))
}

# Function 12: Process V-Dem Data and Calculate Network Averages (from Block 3)
process_vdem_and_network_scores <- function(membership_df_from_tribble, vdem_raw_data) {
  country_name_mapping_internal <- c(
    "United States" = "United States", "United Kingdom" = "United Kingdom", "Korea" = "South Korea",
    "Chinese Taipei" = "Taiwan", "Hong Kong SAR" = "China", "Macau SAR" = "China", "Russia" = "Russia",
    "North Macedonia" = "North Macedonia", "Albania" = "Albania", "Algeria" = "Algeria", "Angola" = "Angola",
    "Argentina" = "Argentina", "Australia" = "Australia", "Austria" = "Austria", "Azerbaijan" = "Azerbaijan",
    "Bangladesh" = "Bangladesh", "Belarus" = "Belarus", "Belgium" = "Belgium", "Bermuda" = "Bermuda",
    "Bolivia" = "Bolivia", "Bosnia and Herzegovina" = "Bosnia and Herzegovina", "Brazil" = "Brazil",
    "Bulgaria" = "Bulgaria", "Cambodia" = "Cambodia", "Canada" = "Canada", "Chile" = "Chile", "China" = "China",
    "Colombia" = "Colombia", "Costa Rica" = "Costa Rica", "Croatia" = "Croatia", "Cuba" = "Cuba",
    "Cyprus" = "Cyprus", "Czech Republic" = "Czechia", "Denmark" = "Denmark", "Dominican Republic" = "Dominican Republic",
    "Ecuador" = "Ecuador", "Egypt" = "Egypt", "El Salvador" = "El Salvador", "Estonia" = "Estonia",
    "Ethiopia" = "Ethiopia", "Fiji" = "Fiji", "Finland" = "Finland", "France" = "France", "Georgia" = "Georgia",
    "Germany" = "Germany", "Ghana" = "Ghana", "Greece" = "Greece", "Guatemala" = "Guatemala", "Honduras" = "Honduras",
    "Hungary" = "Hungary", "Iceland" = "Iceland", "India" = "India", "Indonesia" = "Indonesia", "Iran" = "Iran",
    "Ireland" = "Ireland", "Israel" = "Israel", "Italy" = "Italy", "Ivory Coast" = "Ivory Coast",
    "Jamaica" = "Jamaica", "Japan" = "Japan", "Jordan" = "Jordan", "Kazakhstan" = "Kazakhstan", "Kenya" = "Kenya",
    "Kosovo" = "Kosovo", "Kuwait" = "Kuwait", "Laos" = "Laos", "Latvia" = "Latvia", "Lebanon" = "Lebanon",
    "Liberia" = "Liberia", "Liechtenstein" = "Liechtenstein", "Lithuania" = "Lithuania", "Luxembourg" = "Luxembourg",
    "Malaysia" = "Malaysia", "Malta" = "Malta", "Mauritania" = "Mauritania", "Mauritius" = "Mauritius",
    "Mexico" = "Mexico", "Moldova" = "Moldova", "Monaco" = "Monaco", "Mongolia" = "Mongolia",
    "Montenegro" = "Montenegro", "Morocco" = "Morocco", "Mozambique" = "Mozambique", "Myanmar" = "Myanmar",
    "Namibia" = "Namibia", "Nepal" = "Nepal", "Netherlands" = "Netherlands", "New Zealand" = "New Zealand",
    "Nicaragua" = "Nicaragua", "Nigeria" = "Nigeria", "Norway" = "Norway", "Oman" = "Oman",
    "Pakistan" = "Pakistan", "Panama" = "Panama", "Paraguay" = "Paraguay", "Peru" = "Peru",
    "Philippines" = "Philippines", "Poland" = "Poland", "Portugal" = "Portugal", "Qatar" = "Qatar",
    "Romania" = "Romania", "Rwanda" = "Rwanda", "San Marino" = "San Marino", "Saudi Arabia" = "Saudi Arabia",
    "Senegal" = "Senegal", "Serbia" = "Serbia", "Singapore" = "Singapore", "Slovakia" = "Slovakia",
    "Slovenia" = "Slovenia", "South Africa" = "South Africa", "Spain" = "Spain", "Sri Lanka" = "Sri Lanka",
    "Sweden" = "Sweden", "Switzerland" = "Switzerland", "Syria" = "Syria", "Tanzania" = "Tanzania",
    "Thailand" = "Thailand", "Togo" = "Togo", "Trinidad and Tobago" = "Trinidad and Tobago",
    "Tunisia" = "Tunisia", "Turkey" = "Turkey", "Uganda" = "Uganda", "Ukraine" = "Ukraine",
    "United Arab Emirates" = "United Arab Emirates", "Uruguay" = "Uruguay", "Venezuela" = "Venezuela",
    "Vietnam" = "Vietnam", "Zambia" = "Zambia", "Zimbabwe" = "Zimbabwe"
  )
  eu_member_states_internal <- c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
    "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
    "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
    "Slovenia", "Spain", "Sweden"
  )
  eu_network_entities_internal <- c(
    "European Central Bank", "European Commission", "EBA", "EIOPA", "ESMA", "ESRB", "SSB",
    "CEBS", "CEIOPS", "CESR"
  )
  
  latest_vdem_year <- max(vdem_raw_data$year, na.rm = TRUE)
  cat(paste0("Processing V-Dem data for year: ", latest_vdem_year, "\n"))
  
  vdem_democracy_scores <- vdem_raw_data %>%
    filter(year == latest_vdem_year) %>%
    select(country_name, v2x_polyarchy) %>%
    distinct(country_name, .keep_all = TRUE)
  
  mapped_jurisdictions <- tibble(
    Jurisdiction = names(country_name_mapping_internal),
    V_Dem_Country = unname(country_name_mapping_internal)
  )
  
  membership_with_vdem_names <- membership_df_from_tribble %>%
    left_join(mapped_jurisdictions, by = "Jurisdiction")
  
  eu_scores <- vdem_democracy_scores %>%
    filter(country_name %in% eu_member_states_internal)
  eu_composite_score <- mean(eu_scores$v2x_polyarchy, na.rm = TRUE)
  cat(paste0("Calculated composite V-Dem polyarchy score for EU: ", round(eu_composite_score, 3), "\n"))
  
  final_membership_scores <- membership_with_vdem_names %>%
    left_join(vdem_democracy_scores, by = c("V_Dem_Country" = "country_name")) %>%
    mutate(
      v2x_polyarchy = case_when(
        Jurisdiction == "European Union" ~ eu_composite_score,
        Jurisdiction %in% eu_network_entities_internal ~ NA_real_,
        Jurisdiction == "IFRS Foundation" ~ NA_real_,
        TRUE ~ v2x_polyarchy
      )
    )
  
  global_networks_internal <- c("FSB", "BCBS", "CPMI", "IADI", "IAIS", "IOSCO", "IOPS", "BIS", "NGFS", "IFRS")
  eu_networks_proper_internal <- c("EBA", "EIOPA", "ESMA", "ESRB", "SSB")
  eu_networks_historical_internal <- c("CEBS", "CEIOPS", "CESR")
  all_financial_organizations <- unique(c(global_networks_internal, eu_networks_proper_internal, eu_networks_historical_internal))
  
  network_avg_vdem_scores <- tibble(
    network = character(), network_vdem_score = numeric()
  )
  for (org_col_name in all_financial_organizations) {
    if (org_col_name %in% names(final_membership_scores)) {
      members_polyarchy_scores <- final_membership_scores %>%
        filter(!!sym(org_col_name) == TRUE) %>%
        pull(v2x_polyarchy)
      
      avg_score <- if(length(members_polyarchy_scores[!is.na(members_polyarchy_scores)]) > 0) {
        mean(members_polyarchy_scores, na.rm = TRUE)
      } else {
        NA_real_
      }
      network_avg_vdem_scores <- network_avg_vdem_scores %>%
        add_row(network = org_col_name, network_vdem_score = avg_score)
    } else {
      cat(paste0("Warning: Network column '", org_col_name, "' not found in membership data. Skipping V-Dem average calculation for it.\n"))
    }
  }
  cat("Calculated Network Average V-Dem Scores:\n")
  print(network_avg_vdem_scores %>% arrange(desc(network_vdem_score)))
  return(network_avg_vdem_scores)
}

cat("Helper functions defined.\n")

# --- III. DATA LOADING AND INITIAL PREPARATION ---

# 1. Load primary data from Excel files (as per Block 3)
raw_mydata_intl <- read_excel('demgov_int.xlsx')
raw_eur_TGN <- read_excel("demgov_eur.xlsx")
raw_auth_data <- read_excel("auth.xlsx") # For Appendix 4

cat("\nPrimary Excel data loaded.\n")
cat("Initial dimensions of raw_mydata_intl: ", paste(dim(raw_mydata_intl)), "\n")
cat("Initial dimensions of raw_eur_TGN: ", paste(dim(raw_eur_TGN)), "\n")
cat("Initial dimensions of raw_auth_data: ", paste(dim(raw_auth_data)), "\n")

# 2. Create df_updated_perspective from Block 4
# --- START OF MODIFIED df_organization_metadata DEFINITION ---
csv_data_updated <- 'Acronym,FullName,MembershipCategory
BCBS,"Basel Committee on Banking Supervision",Both
BIS,"Bank for International Settlements",Central bank
CEBS,"Committee of European Banking Supervisors",Both
CEIOPS,"Committee of European Insurance and Occupational Pensions Supervisors",Regulator
CESR,"Committee of European Securities Regulators",Regulator
CPMI,"Committee on Payments and Market Infrastructures",Central bank
EBA,"European Banking Authority",Both
EIOPA,"European Insurance and Occupational Pensions Authority",Regulator
ESMA,"European Securities and Markets Authority",Regulator
ESRB,"European Systemic Risk Board",Both
FSB,"Financial Stability Board",Both
IADI,"International Association of Deposit Insurers",Regulator
IAIS,"International Association of Insurance Supervisors",Regulator
IFRS,"International Financial Reporting Standards Foundation",Regulator
IOPS,"International Organisation of Pension Supervisors",Regulator
IOSCO,"International Organization of Securities Commissions",Regulator
NGFS,"Network for Greening the Financial System",Both
SSB,"Supervisory Board (of the European Central Bank)",Both'
# --- END OF MODIFIED df_organization_metadata DEFINITION ---

df_organization_metadata <- read.csv(text = csv_data_updated, stringsAsFactors = FALSE)
df_organization_metadata$MembershipCategory <- as.factor(df_organization_metadata$MembershipCategory)
cat("\nOrganization metadata (df_organization_metadata) created with updated membership types.\n")
# print(df_organization_metadata)
# str(df_organization_metadata)

# 3. Prepare V-Dem membership_data (tribble from Block 2/3)
vdem_membership_data_tribble <- tribble(
  ~Jurisdiction, ~FSB, ~BCBS, ~CPMI, ~IADI, ~IAIS, ~IOSCO, ~IOPS, ~BIS, ~NGFS, ~IFRS, ~EBA, ~EIOPA, ~ESMA, ~ESRB, ~SSB, ~CEBS, ~CEIOPS, ~CESR,
  "Albania", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Algeria", "", "", "", "", "X", "", "", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Angola", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Argentina", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Australia", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Austria", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Azerbaijan", "", "", "", "X", "X", "", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Bangladesh", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Belarus", "", "", "", "X", "X", "", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Belgium", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Bermuda", "", "", "", "", "X", "", "", "", "", "X", "", "", "", "", "", "", "", "",
  "Bolivia", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Bosnia and Herzegovina", "", "", "", "X", "X", "", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Brazil", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Bulgaria", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Cambodia", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Canada", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Chile", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "China", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Chinese Taipei", "", "", "", "X", "", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Colombia", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Costa Rica", "", "", "", "", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Croatia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Cuba", "", "", "", "", "X", "", "", "", "", "X", "", "", "", "", "", "", "", "",
  "Cyprus", "", "", "", "X", "X", "X", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Czech Republic", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Denmark", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Dominican Republic", "", "", "", "", "X", "", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Ecuador", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Egypt", "", "", "", "X", "X", "X", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "El Salvador", "", "", "", "", "X", "X", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Estonia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Ethiopia", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "European Central Bank", "X", "X", "X", "", "", "", "", "X", "X", "", "X", "X", "X", "X", "X", "X", "X", "X",
  "European Commission", "X", "", "", "", "", "", "", "", "", "X", "X", "X", "X", "X", "", "X", "X", "X",
  "European Union", "", "X", "", "", "", "", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Fiji", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Finland", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "France", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Georgia", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Germany", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Ghana", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Greece", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Guatemala", "", "", "", "", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Honduras", "", "", "", "", "X", "X", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Hong Kong SAR", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Hungary", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Iceland", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", 
  "India", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Indonesia", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Iran", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Ireland", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Israel", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Italy", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Ivory Coast", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Jamaica", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Japan", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Jordan", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Kazakhstan", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Kenya", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Korea", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Kosovo", "", "", "", "X", "", "", "", "", "", "X", "", "", "", "", "", "", "", "",
  "Kuwait", "", "", "", "X", "X", "X", "", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Laos", "", "", "", "", "X", "", "", "", "", "X", "", "", "", "", "", "", "", "",
  "Latvia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Lebanon", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Liberia", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Liechtenstein", "", "", "", "X", "X", "X", "", "", "X", "X", "X", "X", "X", "X", "", "", "", "", 
  "Lithuania", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Luxembourg", "", "X", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Macau SAR", "", "", "", "", "X", "", "", "", "", "X", "", "", "", "", "", "", "", "",
  "Malaysia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Malta", "", "", "", "X", "X", "X", "X", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Mauritania", "", "", "", "X", "", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Mauritius", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Mexico", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Moldova", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Monaco", "", "", "", "", "X", "X", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Mongolia", "", "", "", "", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Montenegro", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Morocco", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Mozambique", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Myanmar", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Namibia", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Nepal", "", "", "", "", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Netherlands", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "New Zealand", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Nicaragua", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Nigeria", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "North Macedonia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Norway", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", 
  "Oman", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Pakistan", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Panama", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Paraguay", "", "", "", "", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Peru", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Philippines", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Poland", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Portugal", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Qatar", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Romania", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Russia", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Rwanda", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "San Marino", "", "", "", "X", "X", "X", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Saudi Arabia", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Senegal", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Serbia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Singapore", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Slovakia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Slovenia", "", "", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "South Africa", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Spain", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Sri Lanka", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Sweden", "", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "Switzerland", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Syria", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Tanzania", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Thailand", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Togo", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Trinidad and Tobago", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", "",
  "Tunisia", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Turkey", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Uganda", "", "", "", "X", "X", "", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Ukraine", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "United Arab Emirates", "", "", "", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "United Kingdom", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
  "United States", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "", "", "", "", "",
  "Uruguay", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Venezuela", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Vietnam", "", "", "", "X", "X", "X", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Zambia", "", "", "", "", "X", "", "X", "", "X", "X", "", "", "", "", "", "", "", "",
  "Zimbabwe", "", "", "", "", "X", "", "", "", "X", "X", "", "", "", "", "", "", "", ""
) %>%
  mutate(across(-Jurisdiction, ~ifelse(. == "X", TRUE, FALSE)))

# 4. Load raw V-Dem data (downloads if not present locally, might take time first run)
raw_vdem_data <- vdemdata::vdem 
cat("\nRaw V-Dem data loaded.\n")


# --- III. DATA LOADING AND INITIAL PREPARATION (Continued) ---
# ... (your existing code in section III) ...

# --- START: NEW SUB-SECTION for Media Salience/Politicization Score Data ---
cat("\n--- Loading and Processing Media Salience Data ---\n")

# --- Part A: Load and Process Financial Media Data (financial_data) ---

# A.1 Define the path to your financial media Excel file
# This path is from our previous script. Ensure it's correct for your setup.
media_excel_file_path <- "/Users/ciaran/Library/CloudStorage/OneDrive-UniversityofWarwick/Macroprudential Paper October 24/2025/r_output/financial_tgn_media_nexis.xlsx"
cat("Media data Excel file path:", media_excel_file_path, "\n")

# A.2 Read the media data Excel file
financial_data <- tryCatch({
  read_excel(media_excel_file_path)
}, error = function(e) {
  cat("Error reading financial media Excel file:", e$message, "\n")
  return(NULL)
})

if (!is.null(financial_data) && nrow(financial_data) > 0) {
  cat("Successfully loaded financial_data. Dimensions:", dim(financial_data)[1], "rows,", dim(financial_data)[2], "cols.\n")
  
  # A.3 Identify the year column (assuming it's the first column) and network columns
  year_column_name_media <- names(financial_data)[1]
  network_columns_media <- setdiff(names(financial_data), year_column_name_media)
  cat("Identified media year column as:", year_column_name_media, "\n")
  # cat("Identified media network columns:", paste(network_columns_media, collapse=", "), "\n") # Optional detail
  
  # A.4 Calculate 3-year rolling average for each network column
  cat("Calculating 3-year rolling averages for media data...\n")
  for (col_name in network_columns_media) {
    if (is.numeric(financial_data[[col_name]])) {
      new_col_name <- paste0(col_name, "_roll_avg_3yr")
      financial_data[[new_col_name]] <- zoo::rollmean(financial_data[[col_name]],
                                                      k = 3,
                                                      fill = NA,
                                                      align = "right",
                                                      na.rm = TRUE)
      financial_data[[new_col_name]][is.nan(financial_data[[new_col_name]])] <- NA
    } else {
      # cat("Warning: Media column", col_name, "is not numeric. Skipped for rolling average.\n") # Optional detail
    }
  }
  cat("Rolling averages calculated.\n")
  # print(head(financial_data, 3)) # Optional: view data with rolling averages
  
} else {
  warning("Failed to load or financial_data is empty. Subsequent steps involving it will be affected.")
  # Create an empty financial_data with expected structure if needed for downstream robustness, or handle error.
  financial_data <- data.frame() # Ensure it exists but is empty to avoid errors if referenced
}

# --- Part B: Load Charter Date Information and Create charter_info_df ---

# B.1 Define the charter date Excel file name
# This assumes 'charter_year.xlsx' is in the working directory set by your main script.
charter_file_name <- "charter_year.xlsx"
cat("Charter date Excel file name:", charter_file_name, "\n")

# B.2 Read the charter date Excel file
# This will be named 'charter_info_df' which the addendum expects
charter_info_df <- tryCatch({
  read_excel(charter_file_name)
}, error = function(e) {
  cat("Error reading charter_date.xlsx:", e$message, "\n")
  cat("Please ensure '", charter_file_name, "' exists in the working directory: ", getwd(), "\n")
  return(NULL)
})

if (!is.null(charter_info_df) && nrow(charter_info_df) > 0 && nrow(financial_data) > 0) {
  cat("Successfully loaded charter_info_df. Dimensions:", dim(charter_info_df)[1], "rows,", dim(charter_info_df)[2], "cols.\n")
  # print(head(charter_info_df)) # Optional: view loaded charter data
  
  # B.3 Define expected column names in charter_info_df (as per your file structure)
  expected_network_col_charter <- "network"
  expected_year_col_charter <- "charter_year_recent"
  
  if (!(expected_network_col_charter %in% names(charter_info_df))) {
    stop(paste("Column '", expected_network_col_charter, "' not found in", charter_file_name))
  }
  if (!(expected_year_col_charter %in% names(charter_info_df))) {
    stop(paste("Column '", expected_year_col_charter, "' not found in", charter_file_name))
  }
  
  # B.4 Initialize the 'avg_media_at_charter_year' column
  charter_info_df$avg_media_at_charter_year <- NA_real_
  cat("Linking charter years with rolling media averages...\n")
  
  for (i in 1:nrow(charter_info_df)) {
    network_name_in_charter <- charter_info_df[[expected_network_col_charter]][i]
    charter_year_val <- charter_info_df[[expected_year_col_charter]][i]
    
    # Construct the rolling average column name (e.g., bcbs_roll_avg_3yr)
    # Network names in financial_data's columns are as they appear in that Excel file.
    # The charter_info_df$network column should match these (e.g. "bcbs", not "BCBS" if media data is lowercase)
    # The addendum later handles conversion to UPPERCASE for merging with df_appendix.
    rolling_avg_col_name_lookup <- paste0(network_name_in_charter, "_roll_avg_3yr")
    
    if (rolling_avg_col_name_lookup %in% names(financial_data)) {
      year_match_row <- financial_data[financial_data[[year_column_name_media]] == charter_year_val, ]
      
      if (nrow(year_match_row) == 1) {
        charter_info_df$avg_media_at_charter_year[i] <- year_match_row[[rolling_avg_col_name_lookup]]
      } else {
        # cat("Warning: Charter year", charter_year_val, "for network", network_name_in_charter, "not found or multiple matches in financial_data.\n") # Optional detail
      }
    } else {
      # cat("Warning: Rolling avg column", rolling_avg_col_name_lookup, "not found in financial_data.\n") # Optional detail
    }
  }
  cat("Finished linking. charter_info_df now contains 'avg_media_at_charter_year'.\n")
  # print(head(charter_info_df)) # Optional: view final charter_info_df
  
} else {
  if (is.null(charter_info_df) || nrow(charter_info_df) == 0) {
    warning(paste(charter_file_name, "could not be loaded or is empty. 'charter_info_df' will not be properly created."))
  }
  if (nrow(financial_data) == 0) {
    warning("Financial_data is empty, so charter_info_df cannot be processed correctly.")
  }
  # Ensure charter_info_df exists, even if empty, to prevent errors if addendum checks for its existence
  if (!exists("charter_info_df") || is.null(charter_info_df)) charter_info_df <- data.frame()
}
cat("--- End of Media Salience Data Processing ---\n")



# --- IV. CORE DATA PROCESSING ---

# 1. Process International Democratic Governance Data
intl_gov_results <- process_governance_data(raw_mydata_intl, "International Regulators")
mydata4 <- intl_gov_results$data_dem_gov_categorized # For global FR bar plot, etc.
mydata3_long <- intl_gov_results$data_long_scaled_with_dimensions # For global loli indicator plots
mydata3.1_long <- intl_gov_results$data_avg_dim_scores # For global loli dimension plots

# 2. Process Combined (All TGN) Democratic Governance Data
raw_all_TGN <- bind_rows(raw_eur_TGN, raw_mydata_intl)
cat("\nDimensions of raw_all_TGN (combined data): ", paste(dim(raw_all_TGN)), "\n")

all_gov_results <- process_governance_data(raw_all_TGN, "All Financial Regulators", is_combined_data = TRUE)
dataTGN3_calculated <- all_gov_results$data_dem_gov_categorized # Main df for most combined plots
dataTGN2_long_calculated <- all_gov_results$data_long_scaled_with_dimensions # For combined loli plots
dataTGN2.1_long_calculated <- all_gov_results$data_avg_dim_scores_by_region_and_dim # For combined loli dimension plots by region
dataTGN4_calculated <- all_gov_results$data_proportions_by_region_score # For stack plots
dataTGN2.2_long_calculated <- dataTGN2.1_long_calculated %>% # For average dimension scores by region only (j_plot)
  group_by(region,dimension) %>%
  summarise(dim_value2= round(mean(dim_value, na.rm = TRUE), digits =2), .groups = 'drop')


# 3. Process V-Dem Data to get Network Averages
network_vdem_scores <- process_vdem_and_network_scores(vdem_membership_data_tribble, raw_vdem_data)

# 4. Process Delegation and Pooling Data (auth_data)
auth_data_processed <- process_auth_data(raw_auth_data)
cat("\nDimensions of auth_data_processed (Appendix 4): ", paste(dim(auth_data_processed)), "\n")

# 5. Create Final Merged Dataframe for Appendix 4 and V-Dem Analysis
df_appendix <- merge(x = auth_data_processed, y = dataTGN3_calculated, by = "network", all.x = TRUE)
df_appendix <- df_appendix %>%
  left_join(network_vdem_scores, by = "network")

# Factor 'score' and 'region' in df_appendix
if ("score" %in% names(df_appendix)) {
  df_appendix$score <- factor(df_appendix$score, levels = c("low", "medium", "high"), ordered = TRUE)
}
if ("region" %in% names(df_appendix)) {
  df_appendix$region <- factor(df_appendix$region, levels = c("Global", "European"), ordered = TRUE)
}
cat("\nDimensions of df_appendix (merged for Appendix 4 & V-Dem plots): ", paste(dim(df_appendix)), "\n")
# print(head(df_appendix))

# Create Global and European subsets for df_appendix
df_appendix_global <- filter(df_appendix, region == "Global")
df_appendix_eur <- filter(df_appendix, region == "European")

# Calculate combined_pd_score in df_appendix (as it's used later)
# This logic ensures NA if both pool01 and del01 are NA, otherwise sums available values.
df_appendix <- df_appendix %>% mutate(
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)
df_appendix_global <- df_appendix_global %>% mutate( # Also update subset if used independently with this score
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)
df_appendix_eur <- df_appendix_eur %>% mutate( # Also update subset
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)

# START OF ADDED CODE FOR NEW SCATTERPLOT DATA PREPARATION
# Prepare data for the new scatterplot (DemGov vs Authority by MembershipType)
# Ensure df_organization_metadata has 'network' column for joining
df_organization_metadata_renamed <- df_organization_metadata %>%
  rename(network = Acronym)

df_for_new_scatterplot <- df_appendix %>%
  left_join(df_organization_metadata_renamed, by = "network")

cat("\nCreated df_for_new_scatterplot by joining df_appendix with membership category.\n")
# print(head(df_for_new_scatterplot %>% select(network, dem_gov, combined_pd_score, MembershipCategory)))
# END OF ADDED CODE FOR NEW SCATTERPLOT DATA PREPARATION


# 6. Create specific network datasets from raw_mydata_intl (as in Block 1)
# These are based on the raw international data, not the processed V-Dem or combined sets.
# If these need to be based on processed data, adjust the source dataframe.
data.IOPS <- raw_mydata_intl[which(raw_mydata_intl$network=='IOPS'), ]
data.IADI <- raw_mydata_intl[which(raw_mydata_intl$network=='IADI'), ]
data.BCBS <- raw_mydata_intl[which(raw_mydata_intl$network=='BCBS'), ]
data.CPMI <- raw_mydata_intl[which(raw_mydata_intl$network=='CPMI'), ]
data.IAIS <- raw_mydata_intl[which(raw_mydata_intl$network=='IAIS'), ]
data.IOSCO <- raw_mydata_intl[which(raw_mydata_intl$network=='IOSCO'), ]
data.NGFS <- raw_mydata_intl[which(raw_mydata_intl$network=='NGFS'), ]
data.IFRS <- raw_mydata_intl[which(raw_mydata_intl$network=='IFRS'), ]
cat("\nIndividual network datasets from raw_mydata_intl created.\n")

# --- V. PLOTTING ---
cat("\n--- Starting Plot Generation ---\n")

# Item names for plot labels
items.names <- c("participation", "representation", "stakeholder", "epistemic", "transparency", "legality", "accountability","reasoning", "efficiency", "effectiveness")
items.names.1 <- c("input", "throughput", "output") # For dimension plots
score_colors_demgov <- c("high"="darkgreen", "medium"="orange", "low"="red") # For dem_gov score
score_colors_appendix <- c("high"="chartreuse4", "medium"="coral2", "low"="darkgoldenrod1", "NA"="grey") # For Appendix Del/Pool plots
custom_shapes <- c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14, 18, 19, 20, 21, 22, 23, 24, 25) # For scatterplot shapes


# A. Plots Related to Democratic Governance Scores (Primarily from Block 1 & 3)

# Bar plot for Global Financial Regulators (Democratic Governance Score) - Uses mydata4
if (exists("mydata4") && nrow(mydata4) > 0) {
  plot_global_fr_bar <- ggplot(mydata4, aes(x= reorder(network, -dem_gov), y= dem_gov) )+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Global Financial Regulators (International Sample)")+ # Clarified sample
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_economist() +
    theme(plot.title = element_text(hjust = 0.5))
  # print(plot_global_fr_bar)
  ggsave("plot_global_fr_bar.svg", plot = plot_global_fr_bar, width = academic_wide_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_global_fr_bar.svg\n")
}

# Bar plot for European Financial Regulators (Democratic Governance Score) - Uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(filter(dataTGN3_calculated, region == "European")) > 0) {
  plot_european_fr_bar <- ggplot(dataTGN3_calculated %>% filter(region == "European"), aes(x= reorder(network, -dem_gov), y= dem_gov) )+
    geom_col(width = 0.5, fill = "skyblue") + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "European Financial Regulators")+
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_economist() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  # print(plot_european_fr_bar)
  ggsave("plot_european_fr_bar.svg", plot = plot_european_fr_bar, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_european_fr_bar.svg\n")
}

# Bar plot for All Financial Regulators (Democratic Governance Score), colored by region - Uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_bar <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill=region))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") + labs(title = "Financial Regulators")+
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          legend.position = c(0.8, 0.8), legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y= element_text(margin = margin(r=10)))
  # print(plot_all_fr_bar)
  ggsave("plot_all_fr_bar.svg", plot = plot_all_fr_bar, width = academic_very_wide_width, height = academic_single_height * 1.5, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_bar.svg\n")
}

# Bar plot for All Financial Regulators faceted by region (Eur Vs Global) - Uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_faceted_region <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill= region))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Financial Regulators by Region")+
    geom_text(aes(label= dem_gov, vjust= -0.2), size=3)+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size=8), strip.text = element_text(face="bold"),
          axis.title.y= element_text(margin = margin(r=10)))+
    facet_wrap(~region, scales = "free_x")
  # print(plot_all_fr_faceted_region)
  ggsave("plot_all_fr_faceted_region.svg", plot = plot_all_fr_faceted_region, width = academic_wide_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_faceted_region.svg\n")
}

# Bar plot for All Financial Regulators faceted by region and colored by Dem. Gov. Score - Uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_faceted_score <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill= score))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Financial Regulators by Region and Score")+
    geom_text(aes(label= dem_gov, vjust= -0.2), size=3)+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=8), strip.text = element_text(face="bold"),
          axis.title.y= element_text(margin = margin(r=10)))+
    scale_fill_manual(name= "Democratic \nGovernance Score", values = score_colors_demgov,
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    facet_wrap(~region, scales = "free_x")
  # print(plot_all_fr_faceted_score)
  ggsave("plot_all_fr_faceted_score.svg", plot = plot_all_fr_faceted_score, width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_faceted_score.svg\n")
}

# Frequency plot - uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_freq_all <- ggplot(dataTGN3_calculated, aes(x= fct_relevel(score, "low", "medium", "high"), fill =score))+
    geom_bar(width= 0.5, aes(y=after_stat(count)/sum(after_stat(count)))) +
    labs(title = "Financial Regulators: Governance Score Distribution")+
    xlab("Democratic Governance Score Category") + ylab("Percent") + theme_minimal()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_fill_manual(name = "Dem. \nGovernance", values = score_colors_demgov,
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text.x = element_text(size=10), legend.position = "right",
          axis.title.y= element_text(margin = margin(r=10)))
  # print(plot_freq_all)
  ggsave("plot_freq_all.svg", plot = plot_freq_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_freq_all.svg\n")
}

# Stack plots - uses dataTGN4_calculated
if (exists("dataTGN4_calculated") && nrow(dataTGN4_calculated) > 0) {
  region.name.levels <- levels(dataTGN4_calculated$region) # Get levels if factor, or unique values
  if(is.null(region.name.levels)) region.name.levels <- unique(dataTGN4_calculated$region)
  
  plot_stack_economist <- ggplot(dataTGN4_calculated, aes(x = factor(region, levels=region.name.levels), y = prop, fill = score))+
    geom_col(position = "fill") + coord_flip() +
    geom_text(aes(label = scales::percent(prop, accuracy=1)), position = position_fill(vjust = 0.5), color = "white", size=3.5, na.rm=TRUE) +
    ggtitle("Financial Regulators", subtitle = "Democratic Governance Score Proportions by Region")+
    xlab("") + ylab("Proportion") + theme_economist()+
    scale_x_discrete(labels = region.name.levels) +
    scale_fill_manual(name = "Score", values = score_colors_demgov,
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          plot.subtitle = element_text(size = 10, hjust = 0), legend.title = element_text(size=10))
  # print(plot_stack_economist)
  ggsave("plot_stack_economist.svg", plot = plot_stack_economist, width = academic_single_width, height = academic_single_height * 0.8, dpi = academic_plot_dpi)
  cat("Saved plot_stack_economist.svg\n")
  
  plot_stack_minimal <- ggplot(dataTGN4_calculated, aes(x = factor(region, levels=region.name.levels), y = prop, fill = score))+
    geom_col(position = "fill") + coord_flip() +
    geom_text(aes(label = scales::percent(prop, accuracy=1)), position = position_fill(vjust = 0.5), color = "white", size=3.5, na.rm=TRUE) +
    ggtitle("Financial Regulators", subtitle = "Democratic Governance Score Proportions by Region")+
    xlab("") + ylab("Proportion") + theme_minimal()+
    scale_x_discrete(labels = region.name.levels) +
    scale_fill_manual(name = "Score", values = score_colors_demgov,
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5), axis.line.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          plot.subtitle = element_text(size = 10, hjust = 0.5), legend.title = element_text(size=10))
  # print(plot_stack_minimal)
  ggsave("plot_stack_minimal.svg", plot = plot_stack_minimal, width = academic_single_width, height = academic_single_height * 0.8, dpi = academic_plot_dpi)
  cat("Saved plot_stack_minimal.svg\n")
}

# Box plot - uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_box_all <- ggplot(dataTGN3_calculated, aes(x= region, y= dem_gov, fill=region)) +
    geom_boxplot(na.rm=TRUE) + xlab("") + ylab("Democratic Governance Score") +
    ggtitle("Financial Regulators: Governance Score by Region")+ theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.line.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size=10), legend.position = "none")
  # print(plot_box_all)
  ggsave("plot_box_all.svg", plot = plot_box_all, width = academic_single_width * 0.8, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_box_all.svg\n")
}

# Dot plot (jitter) - uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_dot_all <- ggplot(dataTGN3_calculated, aes(x= region, y= dem_gov, color = region)) +
    geom_jitter(width = 0.1, alpha = 0.7, na.rm=TRUE) +
    stat_summary(geom="point", fun = "mean", size= 3, shape = 24, fill="red")+
    xlab("") + ylab("Democratic Governance Score") + ggtitle("Financial Regulators: Governance Scores")+ theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.position = "none")
  # print(plot_dot_all)
  ggsave("plot_dot_all.svg", plot = plot_dot_all, width = academic_single_width * 0.8, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_dot_all.svg\n")
}

# Density plots - uses dataTGN3_calculated
if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_density_all <- ggplot(dataTGN3_calculated, aes(x= dem_gov, fill=region))+
    geom_density(alpha=.4, na.rm=TRUE) + xlim(0,1)+ xlab("Democratic Governance Score") +
    ggtitle("Density of Governance Scores by Region")+ ylab("Density") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.title = element_blank(), legend.position = c(0.85, 0.85))
  # print(plot_density_all)
  ggsave("plot_density_all.svg", plot = plot_density_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_density_all.svg\n")
  
  plot_hist_all <- ggplot(dataTGN3_calculated, aes(x= dem_gov, fill=region))+
    geom_histogram(alpha=.5, position="identity", binwidth = 0.1, na.rm=TRUE) + xlim(0,1)+
    xlab("Democratic Governance Score") + ggtitle("Histogram of Governance Scores by Region")+ ylab("Count") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.title = element_blank(), legend.position = c(0.85, 0.85))
  # print(plot_hist_all)
  ggsave("plot_hist_all.svg", plot = plot_hist_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_hist_all.svg\n")
}

# Lollipop plots for indicator scores
# Global - uses mydata3_long (from intl_gov_results)
if (exists("mydata3_long") && nrow(mydata3_long) > 0) {
  num_networks_global_loli <- length(unique(mydata3_long$network))
  loli_global_height <- academic_single_height * (1 + num_networks_global_loli %/% 3) * 0.8
  
  plot_loli_global <- ggplot(mydata3_long, aes(x= fct_rev(indicator), y= value, color = dimension)) +
    geom_point(size=1.5, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= indicator, xend= indicator, y=0, yend= value), na.rm=TRUE) +
    facet_wrap(~network, ncol=3) + xlab("") + ylab("Indicator Value") +
    scale_x_discrete(labels = rev(items.names)) +
    labs(colour= "Democratic \nGovernance Dimension", title="Global Financial Regulators: Indicator Scores (Int'l Sample)") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8),
          axis.text.y = element_text(size=7), legend.position = "bottom")
  # print(plot_loli_global)
  ggsave("plot_loli_global.svg", plot = plot_loli_global, width = academic_wide_width, height = loli_global_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_global.svg\n")
}
# European - uses dataTGN2_long_calculated filtered for European
if (exists("dataTGN2_long_calculated") && nrow(filter(dataTGN2_long_calculated, region == "European")) > 0) {
  eur_data_loli <- dataTGN2_long_calculated %>% filter(region == "European")
  num_networks_eur_loli <- length(unique(eur_data_loli$network))
  loli_eur_height <- academic_single_height * (1 + num_networks_eur_loli %/% 2) * 0.7
  
  plot_loli_eur <- ggplot(eur_data_loli, aes(x= fct_rev(indicator), y= value, color = dimension)) +
    geom_point(size=1.5, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= indicator, xend= indicator, y=0, yend= value), na.rm=TRUE) +
    facet_wrap(~network, ncol=2) + xlab("") + ylab("Indicator Value") +
    scale_x_discrete(labels = rev(items.names)) +
    labs(colour= "Democratic \nGovernance Dimension", title="European Financial Regulators: Indicator Scores") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8),
          axis.text.y = element_text(size=7), legend.position = "bottom")
  # print(plot_loli_eur)
  ggsave("plot_loli_eur.svg", plot = plot_loli_eur, width = academic_wide_width *0.8, height = loli_eur_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_eur.svg\n")
}

# Lollipop plots for dimension scores
# Global - uses mydata3.1_long (from intl_gov_results)
if (exists("mydata3.1_long") && nrow(mydata3.1_long) > 0) {
  num_networks_global_dims_loli <- length(unique(mydata3.1_long$network))
  loli_global_dims_height <- academic_single_height * (1 + num_networks_global_dims_loli %/% 3) * 0.6
  
  plot_loli_global_dims <- ggplot(mydata3.1_long, aes(x= factor(dimension, levels=items.names.1), y= dim_value, color = dimension)) +
    geom_point(size=2, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value), na.rm=TRUE) +
    facet_wrap(~network, ncol=3) + xlab("") + ylab("Dimension Value") +
    scale_x_discrete(labels = items.names.1) + labs(title="Global Financial Regulators: Dimension Scores (Int'l Sample)") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8), legend.position = "none")
  # print(plot_loli_global_dims)
  ggsave("plot_loli_global_dims.svg", plot = plot_loli_global_dims, width = academic_wide_width, height = loli_global_dims_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_global_dims.svg\n")
}
# European - uses dataTGN2.1_long_calculated filtered for European
if (exists("dataTGN2.1_long_calculated") && nrow(filter(dataTGN2.1_long_calculated, region == "European")) > 0) {
  eur_data_dims <- dataTGN2.1_long_calculated %>% filter(region == "European")
  num_networks_eur_dims_loli <- length(unique(eur_data_dims$network))
  loli_eur_dims_height <- academic_single_height * (1 + num_networks_eur_dims_loli %/% 2) * 0.6
  
  plot_loli_eur_dims <- ggplot(eur_data_dims, aes(x= factor(dimension, levels=items.names.1), y= dim_value, color = dimension)) +
    geom_point(size=2, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value), na.rm=TRUE) +
    facet_wrap(~network, ncol=2) + xlab("") + ylab("Dimension Value") +
    scale_x_discrete(labels = items.names.1) + labs(title="European Financial Regulators: Dimension Scores") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8), legend.position = "none")
  # print(plot_loli_eur_dims)
  ggsave("plot_loli_eur_dims.svg", plot = plot_loli_eur_dims, width = academic_wide_width * 0.8, height = loli_eur_dims_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_eur_dims.svg\n")
}

# Region (Eur vs Global) across 3 dimensions - uses dataTGN2.2_long_calculated
if(exists("dataTGN2.2_long_calculated") && nrow(dataTGN2.2_long_calculated) > 0) {
  j_plot <- ggplot(dataTGN2.2_long_calculated, aes(x= fct_relevel(dimension, "output", "throughput", "input"), y= dim_value2, color = dimension)) +
    geom_point(size=2.5, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value2), na.rm=TRUE) +
    facet_wrap(~region) + xlab("") + ylab("Average Dimension Score") +
    labs(color = "Dimension", title="Average Dimension Scores by Region") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold"), legend.position = "none")
  # print(j_plot)
  ggsave(file="LolliPlot_All_by_region_dimensions.svg", plot=j_plot, width=academic_wide_width, height=academic_single_height * 0.9, dpi = academic_plot_dpi)
  cat("Saved LolliPlot_All_by_region_dimensions.svg\n")
}

# All Financial Regulators across three dimension (faceted by network, colored by dimension, fill by region)
# Uses dataTGN2.1_long_calculated (which has network, region, dimension, dim_value)
if (exists("dataTGN2.1_long_calculated") && nrow(dataTGN2.1_long_calculated) > 0) {
  dataTGN2.1_long_temp_calc <- dataTGN2.1_long_calculated %>%
    mutate(region = factor(region, levels = c("Global", "European"))) 
  
  factor_order_calc <- dataTGN2.1_long_temp_calc %>%
    filter(!is.na(network) & !is.na(region)) %>%
    distinct(region, network) %>% arrange(region, network) %>% pull(network)
  
  i_plot_ncols_calc <- 4
  i_plot_nrows_calc <- ceiling(length(factor_order_calc) / i_plot_ncols_calc)
  i_plot_height_calc <- academic_single_height * 0.6 * i_plot_nrows_calc # Adjust height
  
  i_plot <- ggplot(dataTGN2.1_long_temp_calc %>% filter(!is.na(network) & !is.na(region)), 
                   aes(x = fct_relevel(dimension, "output", "throughput", "input"), y = dim_value)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = region), alpha = 0.1, inherit.aes = FALSE) +
    geom_point(aes(color = dimension), size=2, na.rm=TRUE) + coord_flip()+ ylim(0, 1) +
    geom_segment(aes(x = dimension, xend= dimension, y=0, yend= dim_value, color = dimension), na.rm=TRUE) +
    facet_wrap(~factor(network, levels = factor_order_calc), ncol = i_plot_ncols_calc, dir = "h") +
    xlab("") + ylab("Score") + labs(title="Dimension Scores by Network and Region") + theme_linedraw() +
    scale_fill_manual(name = "Region", values = c("Global" = "lightblue", "European" = "lightpink"), labels = c("Global" = "Global", "European" = "European"), na.value="grey") +
    scale_color_manual(name = "Dem. Gov. \nDimension", values = c("input" = "blue", "throughput" = "orange", "output" = "green"), na.value="grey") +
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines"), strip.text.x = element_text(size = 8, face = "bold"),
          axis.text = element_text(size = 8), axis.text.y = element_text(size=7),
          legend.text = element_text(size = 8), legend.title = element_text(size = 9),
          legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.25), order=1), color = guide_legend(order=2))
  # print(i_plot)
  ggsave(file="LolliPlot_All_by_type_and_dimensions.svg", plot=i_plot, width=academic_very_wide_width, height=i_plot_height_calc, dpi = academic_plot_dpi, limitsize = FALSE)
  cat("Saved LolliPlot_All_by_type_and_dimensions.svg\n")
}

# Correlation plot for indicators - uses dataTGN_processed (from Block 1, now combined logic)
# dataTGN_processed was created in the process_governance_data for "All TGN"
# but the helper function needs to return the `df_scaled_wide` part for this.
# Let's re-create dataTGN_for_corr from raw_all_TGN for clarity here, or ensure helper returns it.
temp_all_gov_aggregated <- aggregate_indicators(raw_all_TGN)
temp_all_gov_scaled_wide <- scale_indicators(temp_all_gov_aggregated)

if (all(paste0(c("in_part", "in_rep", "in_shb", "in_epi", "through_tr", "through_leg", "through_acc", "through_reas", "out_effec", "out_effic"), "01") %in% names(temp_all_gov_scaled_wide))) {
  dataTGN_for_corr <- temp_all_gov_scaled_wide %>%
    select(ends_with("01"))
  
  dataTGN_for_corr <- dataTGN_for_corr %>% mutate(across(everything(), as.numeric))
  
  mydata.cor <- cor(dataTGN_for_corr, use="pairwise.complete.obs") # Calculate correlation matrix
  
  # Correct way to save a base R plot (like one from corrplot)
  svglite("correlation_plot_indicators.svg", width = 7, height = 7) # Open the SVG graphics device
  corrplot(mydata.cor, type="upper", order="hclust", tl.col = "black", tl.srt = 45,
           title = "Correlation Matrix of Governance Indicators", mar=c(0,0,1,0)) # Draw the plot
  dev.off() # Close the graphics device
  cat("Saved correlation_plot_indicators.svg\n")
  
} else {
  warning("Not all '01' indicator columns found for correlation plot.")
}

# B. Appendix 4 Plots (Delegation, Pooling, and V-Dem integration) - Uses df_appendix
# Scatterplot: All financial regulators - Delegation vs. Pooling, colored by Governance Score
if (nrow(df_appendix) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix))) {
  num_networks_all_app4 <- length(unique(df_appendix$network[!is.na(df_appendix$network)]))
  available_shapes <- rep(custom_shapes, length.out = num_networks_all_app4)
  
  plot_scatter_del_pool_all <- ggplot(df_appendix, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm=TRUE) + # Ensure color is NULL for smooth
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) +
    scale_shape_manual(name = "Network", values = available_shapes, drop = FALSE, na.value = 4) + # Use na.value for shape
    geom_text_repel(color= "black", size = 3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + labs(title = "Delegation vs. Pooling by Governance Score") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"), legend.box = "horizontal")
  # print(plot_scatter_del_pool_all)
  ggsave(file="ScatterPlot_All_Appendix.svg", plot=plot_scatter_del_pool_all, width=academic_wide_width, height=academic_single_height * 1.3, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_All_Appendix.svg\n")
}

# Scatterplot for European Regulators (Appendix 4)
if (nrow(df_appendix_eur) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix_eur))) {
  num_networks_eur_app4 <- length(unique(df_appendix_eur$network[!is.na(df_appendix_eur$network)]))
  available_shapes_eur <- rep(custom_shapes, length.out = num_networks_eur_app4)
  
  plot_scatter_del_pool_eur <- ggplot(df_appendix_eur, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm=TRUE) +
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop=FALSE) +
    scale_shape_manual(name = "Network", values = available_shapes_eur, drop = FALSE, na.value = 4) +
    geom_text_repel(color= "black", size = 3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + ggtitle("European Regulators: Delegation vs. Pooling") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=12, face="bold"), legend.box = "horizontal")
  # print(plot_scatter_del_pool_eur)
  ggsave(file="ScatterPlot_Eur_Appendix.svg", plot=plot_scatter_del_pool_eur, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_Eur_Appendix.svg\n")
}

# Scatterplot for Global Regulators (Appendix 4)
if (nrow(df_appendix_global) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix_global))) {
  num_networks_global_app4 <- length(unique(df_appendix_global$network[!is.na(df_appendix_global$network)]))
  available_shapes_global <- rep(custom_shapes, length.out = num_networks_global_app4)
  
  plot_scatter_del_pool_global <- ggplot(df_appendix_global, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm=TRUE) +
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) +
    scale_shape_manual(name = "Network", values = available_shapes_global, drop = FALSE, na.value = 4) +
    geom_text_repel(color= "black", size=3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + ggtitle("Global Regulators: Delegation vs. Pooling") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=12, face="bold"), legend.box = "horizontal")
  # print(plot_scatter_del_pool_global)
  ggsave(file="ScatterPlot_Global_Appendix.svg", plot=plot_scatter_del_pool_global, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_Global_Appendix.svg\n")
}

# Barplot for Delegation scores (Appendix 4)
if (nrow(df_appendix) > 0 && all(c("network", "del01", "score", "region") %in% names(df_appendix))) {
  plot_bar_delegation_appendix <- ggplot(df_appendix, aes(x= fct_reorder(network, -del01), y= del01, fill= score))+
    geom_col(width = 0.5, na.rm=TRUE) + xlab("") + ylab("Delegation (Scaled)") +
    labs(title = "Financial Regulators: Delegation by Governance Score")+
    geom_text(aes(label= del01, vjust= -0.2), size=2.5, na.rm=TRUE)+
    scale_y_continuous(limits = c(0, max(df_appendix$del01, na.rm=TRUE) * 1.15), expand = expansion(mult = c(0, 0.05))) + # Ensure y-axis starts at 0
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=8),
          axis.title.y= element_text(margin = margin(r=10)),
          strip.text = element_text(face="bold"))+
    scale_fill_manual(name= "Democratic \nGovernance Score", values=score_colors_appendix,
                      labels = c("high"="High", "medium"="Medium", "low"="Low", "NA"="Not Scored"),
                      na.value = "grey", drop = FALSE) +
    facet_wrap(~region, scales = "free_x")
  # print(plot_bar_delegation_appendix)
  ggsave(file="BarPlot_Auth_All_Appendix.svg", plot=plot_bar_delegation_appendix, width=academic_wide_width, height=academic_single_height*1.3, dpi = academic_plot_dpi)
  cat("Saved BarPlot_Auth_All_Appendix.svg\n")
}

# Barplot for Pooling scores (Appendix 4)
if (nrow(df_appendix) > 0 && all(c("network", "pool01", "score", "region") %in% names(df_appendix))) {
  plot_bar_pooling_appendix <- ggplot(df_appendix, aes(x= fct_reorder(network, -pool01), y= pool01, fill= score))+
    geom_col(width = 0.5, na.rm=TRUE) + xlab("") + ylab("Pooling (Scaled)") +
    labs(title = "Financial Regulators: Pooling by Governance Score")+
    geom_text(aes(label= pool01, vjust= -0.2), size=2.5, na.rm=TRUE)+
    scale_y_continuous(limits = c(0, max(df_appendix$pool01, na.rm=TRUE) * 1.15), expand = expansion(mult = c(0, 0.05))) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=8),
          axis.title.y= element_text(margin = margin(r=10)),
          strip.text = element_text(face="bold"))+
    scale_fill_manual(name= "Democratic \nGovernance Score", values=score_colors_appendix,
                      labels = c("high"="High", "medium"="Medium", "low"="Low", "NA"="Not Scored"),
                      na.value = "grey", drop = FALSE) +
    facet_wrap(~region, scales = "free_x")
  # print(plot_bar_pooling_appendix)
  ggsave(file="BarPlot_Pool_All_Appendix.svg", plot=plot_bar_pooling_appendix, width=academic_wide_width, height=academic_single_height*1.3, dpi = academic_plot_dpi)
  cat("Saved BarPlot_Pool_All_Appendix.svg\n")
}

# Dot plot for Delegation scores by region (Appendix 4)
if (nrow(df_appendix) > 0 && all(c("region", "del01", "network") %in% names(df_appendix))) {
  plot_dot_delegation_appendix <- ggplot(df_appendix, aes(x= region, y= del01, color = region)) +
    geom_jitter(width=0.1, alpha=0.6, na.rm=TRUE)+
    geom_text_repel(aes(label = network), size=3, max.overlaps = 15, segment.alpha = 0.5, na.rm = TRUE) +
    stat_summary(geom="point", fun = "mean", size= 4, shape = 24, fill="red")+
    xlab("") + ylab("Delegation (Scaled)") + ggtitle("Financial Regulators: Delegation Scores by Region")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none")
  # print(plot_dot_delegation_appendix)
  ggsave(file="DotPlot_Auth_All_Appendix.svg", plot=plot_dot_delegation_appendix, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved DotPlot_Auth_All_Appendix.svg\n")
}

# Dot plot for Pooling scores by region (Appendix 4)
if (nrow(df_appendix) > 0 && all(c("region", "pool01", "network") %in% names(df_appendix))) {
  plot_dot_pooling_appendix <- ggplot(df_appendix, aes(x= region, y= pool01, color = region)) +
    geom_jitter(width=0.1, alpha=0.6, na.rm=TRUE)+
    geom_text_repel(aes(label = network), size=3, max.overlaps = 15, segment.alpha = 0.5, na.rm = TRUE) +
    stat_summary(geom="point", fun = "mean", size= 4, shape = 24, fill="red")+
    xlab("") + ylab("Pooling (Scaled)") + ggtitle("Financial Regulators: Pooling Scores by Region")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none")
  # print(plot_dot_pooling_appendix)
  ggsave(file="DotPlot_Pool_All_Appendix.svg", plot=plot_dot_pooling_appendix, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved DotPlot_Pool_All_Appendix.svg\n")
}

# Box Plot for Combined P&D Scores - All Regulators (Appendix 4)
# combined_pd_score is already calculated in df_appendix in section IV
if (nrow(df_appendix) > 0 && all(c("score", "combined_pd_score") %in% names(df_appendix))) {
  df_outliers_all_app4 <- df_appendix %>%
    filter(!is.na(score) & !is.na(combined_pd_score)) %>% group_by(score) %>%
    mutate(outlier_label = ifelse(is_outlier(combined_pd_score), as.character(network), NA_character_)) %>%
    ungroup() %>% filter(!is.na(outlier_label))
  
  plot_box_combined_pd_all <- ggplot(df_appendix %>% filter(!is.na(score) & !is.na(combined_pd_score)),
                                     aes(x = score, y = combined_pd_score, fill = score)) +
    geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, color = "black", size = 2) +
    geom_text_repel(data = df_outliers_all_app4, aes(label = outlier_label), color = "black", size = 2.5,
                    na.rm = TRUE, box.padding = unit(0.5, "lines"), point.padding = unit(0.3, "lines"),
                    segment.color = 'grey50', min.segment.length = unit(0, "lines"), max.overlaps = Inf) +
    scale_fill_manual(name= "Democratic \nGovernance Score", values = score_colors_demgov,
                      labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop=FALSE) +
    xlab("Democratic Governance Score") + ylab("Combined Pooling & Delegation Score (Scaled)") +
    ggtitle("Combined P&D Score by Governance Score (All Regulators)") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  # print(plot_box_combined_pd_all)
  ggsave(file="BoxPlot_All_Combined_PD_Outliers.svg", plot=plot_box_combined_pd_all, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved BoxPlot_All_Combined_PD_Outliers.svg\n")
}

# C. V-Dem Integration Plots (from Block 3, using df_appendix)
# Scatter Plot: Network V-Dem Score vs. Overall Democratic Governance Score (dem_gov)
if (nrow(df_appendix) > 0 && all(c("network_vdem_score", "dem_gov", "region", "network", "score") %in% names(df_appendix))) {
  plot_scatter_vdem_vs_demgov <- ggplot(df_appendix, aes(x = network_vdem_score, y = dem_gov, label = network)) +
    geom_point(aes(color = score, shape = region), size = 3.5, alpha = 0.8, na.rm=TRUE) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey40", linetype = "dashed", na.rm = TRUE) +
    geom_text_repel(size = 3, color = "black", max.overlaps = 10, segment.color = 'grey70', na.rm = TRUE) +
    xlab("Network Average V-Dem Polyarchy Score") +
    ylab("Overall Democratic Governance Score (Calculated)") +
    labs(title = "V-Dem Score vs. Democratic Governance Score",
         subtitle = "Color by DemGov Score Category, Shape by Region",
         color = "DemGov Score", shape = "Region") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom", legend.box = "horizontal") +
    scale_color_manual(values = score_colors_demgov, drop = FALSE, na.value="grey") +
    scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1))
  # print(plot_scatter_vdem_vs_demgov)
  ggsave(file = "ScatterPlot_VDEM_vs_DemGov_Calculated.svg", plot = plot_scatter_vdem_vs_demgov, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_VDEM_vs_DemGov_Calculated.svg\n")
}

# Scatter Plot: Network V-Dem Score vs. Input/Throughput/Output Dimension Scores
# data_vdem_vs_dimensions uses dataTGN2.1_long_calculated (all_gov_results$data_avg_dim_scores_by_region_and_dim)
# and network_vdem_scores
if(exists("all_gov_results") && "data_avg_dim_scores_by_region_and_dim" %in% names(all_gov_results) && exists("network_vdem_scores")) {
  data_vdem_vs_dimensions <- all_gov_results$data_avg_dim_scores_by_region_and_dim %>%
    left_join(network_vdem_scores, by = "network")
  
  if (nrow(data_vdem_vs_dimensions) > 0 && all(c("network_vdem_score", "dim_value", "dimension", "network", "region") %in% names(data_vdem_vs_dimensions))) {
    plot_scatter_vdem_vs_dimensions_calc <- ggplot(data_vdem_vs_dimensions, aes(x = network_vdem_score, y = dim_value, label = network)) +
      geom_point(aes(color = dimension, shape = region), size = 3, alpha = 0.7, na.rm=TRUE) +
      geom_smooth(aes(color = dimension, group = dimension), method = "lm", se = FALSE, linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(size = 2.5, color = "black", max.overlaps = 7, segment.color = 'grey80', na.rm = TRUE,
                      data = . %>% distinct(network, dimension, .keep_all = TRUE)) + # Label once per network per dimension
      xlab("Network Average V-Dem Polyarchy Score") +
      ylab("Democratic Governance Dimension Score (Calculated)") +
      labs(title = "V-Dem Score vs. Democratic Governance Dimensions",
           subtitle = "Points colored by Dimension, shaped by Region. Lines are per-dimension trends.",
           color = "Dimension", shape = "Region") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.position = "bottom", legend.box = "vertical",
            strip.text = element_text(face = "bold", size = 10)) +
      scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
      facet_wrap(~dimension, ncol = 3)
    # print(plot_scatter_vdem_vs_dimensions_calc)
    ggsave(file = "ScatterPlot_VDEM_vs_Dimensions_Calculated.svg", plot = plot_scatter_vdem_vs_dimensions_calc, width = academic_very_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_VDEM_vs_Dimensions_Calculated.svg\n")
  } else {
    cat("Skipping ScatterPlot_VDEM_vs_Dimensions_Calculated.svg due to missing data or columns after merge.\n")
  }
} else {
  cat("Skipping ScatterPlot_VDEM_vs_Dimensions_Calculated.svg because prerequisite dataframes are missing.\n")
}

# Correlation Analysis Plots from Block 2 (using df_appendix)
if (nrow(df_appendix) > 0 && "network_vdem_score" %in% names(df_appendix)) {
  # Correlation between Network V-Dem Score and Delegation (Scaled)
  if("del01" %in% names(df_appendix)) {
    cor_del_vdem <- cor.test(df_appendix$network_vdem_score, df_appendix$del01, use = "pairwise.complete.obs")
    cat("\nCorrelation between Network V-Dem Score and Delegation (Scaled):\n")
    print(cor_del_vdem)
    
    plot_vdem_del_all <- ggplot(df_appendix, aes(x = network_vdem_score, y = del01, color = score, label = network)) +
      geom_point(aes(shape = network), size = 3, na.rm=TRUE) +
      geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange = TRUE, linetype = "solid", color = "black", na.rm = TRUE) +
      scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) +
      scale_shape_manual(name = "Network", values = rep(custom_shapes, length.out = length(unique(df_appendix$network[!is.na(df_appendix$network)]))), drop=FALSE, na.value=4) +
      geom_text_repel(color= "black", size = 3, max.overlaps = Inf, segment.color = 'grey50', na.rm=TRUE) +
      xlab("Network Average V-Dem Polyarchy Score") + ylab("Delegation (Scaled)") +
      labs(title = "Network V-Dem Score vs. Delegation (All Regulators)") + theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"))
    # print(plot_vdem_del_all)
    ggsave(file="ScatterPlot_Vdem_Del_All.svg", plot=plot_vdem_del_all, width=academic_single_width, height=academic_single_height * 1.1, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_Vdem_Del_All.svg\n")
  }
  
  # Correlation between Network V-Dem Score and Pooling (Scaled)
  if("pool01" %in% names(df_appendix)) {
    cor_pool_vdem <- cor.test(df_appendix$network_vdem_score, df_appendix$pool01, use = "pairwise.complete.obs")
    cat("\nCorrelation between Network V-Dem Score and Pooling (Scaled):\n")
    print(cor_pool_vdem)
    
    plot_vdem_pool_all <- ggplot(df_appendix, aes(x = network_vdem_score, y = pool01, color = score, label = network)) +
      geom_point(aes(shape = network), size = 3, na.rm=TRUE) +
      geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange = TRUE, linetype = "solid", color = "black", na.rm = TRUE) +
      scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) +
      scale_shape_manual(name = "Network", values = rep(custom_shapes, length.out = length(unique(df_appendix$network[!is.na(df_appendix$network)]))), drop=FALSE, na.value=4) +
      geom_text_repel(color= "black", size = 3, max.overlaps = Inf, segment.color = 'grey50', na.rm=TRUE) +
      xlab("Network Average V-Dem Polyarchy Score") + ylab("Pooling (Scaled)") +
      labs(title = "Network V-Dem Score vs. Pooling (All Regulators)") + theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"))
    # print(plot_vdem_pool_all)
    ggsave(file="ScatterPlot_Vdem_Pool_All.svg", plot=plot_vdem_pool_all, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_Vdem_Pool_All.svg\n")
  }
}

# START OF ADDED CODE FOR NEW SCATTERPLOT
# D. Scatterplot: Democratic Governance vs. Authority by Membership Type

if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  
  # Ensure necessary columns are numeric and handle potential NAs for plotting
  df_plot_ready <- df_for_new_scatterplot %>%
    mutate(
      dem_gov = as.numeric(dem_gov),
      combined_pd_score = as.numeric(combined_pd_score)
    ) %>%
    filter(!is.na(dem_gov) & !is.na(combined_pd_score) & !is.na(MembershipCategory)) # Ensure no NAs in core aesthetics
  
  if (nrow(df_plot_ready) > 0) {
    plot_demgov_authority_memtype <- ggplot(df_plot_ready, aes(x = dem_gov, y = combined_pd_score, color = MembershipCategory)) + 
      geom_point(size = 3.5, alpha = 0.7, na.rm = TRUE) + 
      geom_text_repel(aes(label = network), size = 2.5, max.overlaps = 10, na.rm = TRUE, 
                      segment.color = 'grey70', segment.alpha = 0.5,
                      box.padding = unit(0.35, "lines"),
                      point.padding = unit(0.2, "lines")) + 
      labs(title = "Democratic Governance vs. Combined Pooling & Delegation",
           subtitle = "Networks by Membership Type",
           x = "Democratic Governance Score (dem_gov)",
           y = "Combined Pooling & Delegation Score (scaled)",
           color = "Membership Type") +
      theme_minimal(base_size = 11) + 
      theme(legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.title = element_text(face="bold"),
            legend.box = "horizontal") +
      scale_x_continuous(limits = c(0, max(df_plot_ready$dem_gov, na.rm = TRUE) * 1.05), expand = expansion(mult = c(0, .05))) +
      scale_y_continuous(limits = c(0, max(df_plot_ready$combined_pd_score, na.rm = TRUE) * 1.05), expand = expansion(mult = c(0, .05))) +
      guides(color = guide_legend(override.aes = list(size=4))) 
    
    # print(plot_demgov_authority_memtype) # Uncomment to display plot in R environment
    ggsave("plot_demgov_authority_memtype.svg", plot = plot_demgov_authority_memtype, width = academic_wide_width, height = academic_single_height * 1.4, dpi = academic_plot_dpi) 
    cat("Saved plot_demgov_authority_memtype.svg\n")
  } else {
    cat("Skipping new scatterplot: No data available after filtering NAs for critical columns.\n")
  }
} else {
  cat("Skipping new scatterplot: df_for_new_scatterplot is not available or empty.\n")
}
# END OF ADDED CODE FOR NEW SCATTERPLOT
# --- V. PLOTTING (Continued) ---

# ... (all your existing plot code from A, B, C, D) ...


# --- E. Plots by Membership Category ---
cat("\n--- Starting Plots by Membership Category ---\n")

# Ensure df_for_new_scatterplot is available and has the necessary columns
if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  
  # Convert relevant columns to numeric and MembershipCategory to factor just in case
  df_membership_analysis <- df_for_new_scatterplot %>%
    mutate(
      dem_gov = as.numeric(dem_gov),
      combined_pd_score = as.numeric(combined_pd_score),
      network_vdem_score = as.numeric(network_vdem_score),
      MembershipCategory = as.factor(MembershipCategory)
    )
  
  # Define a color palette for MembershipCategory if needed (ggplot will pick defaults otherwise)
  membership_colors <- c("Regulator" = "#1f77b4", "Central bank" = "#ff7f0e", 
                         "Both" = "#2ca02c", "Mixed" = "#d62728", "Other" = "grey50") # Added "Other" just in case
  
  # E.1: Democratic Governance vs. Combined Authority by Membership Category (Faceted Scatter)
  plot_data_e1 <- df_membership_analysis %>%
    filter(!is.na(dem_gov) & !is.na(combined_pd_score) & !is.na(MembershipCategory))
  
  if(nrow(plot_data_e1) > 0) {
    plot_demgov_vs_auth_by_memtype_facet <- ggplot(plot_data_e1, aes(x = dem_gov, y = combined_pd_score)) +
      geom_point(aes(color = MembershipCategory), size = 2.5, alpha = 0.8, na.rm = TRUE) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(aes(label = network), size = 2.5, max.overlaps = 10, na.rm = TRUE,
                      segment.color = 'grey70', segment.alpha = 0.5) +
      facet_wrap(~MembershipCategory, scales = "fixed") + # Use "free" if scales differ significantly
      labs(title = "Democratic Governance vs. Combined Authority",
           subtitle = "Faceted by Membership Type (Trend line for each facet)",
           x = "Democratic Governance Score",
           y = "Combined Pooling & Delegation Score") +
      scale_color_manual(values = membership_colors, name = "Membership Type") +
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none", # Color is shown in facet titles or can be added if preferred
            strip.text = element_text(face = "bold"))
    
    # print(plot_demgov_vs_auth_by_memtype_facet)
    ggsave("plot_demgov_vs_auth_by_memtype_facet.svg", plot = plot_demgov_vs_auth_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi) # Adjusted size
    cat("Saved plot_demgov_vs_auth_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_demgov_vs_auth_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
  # E.2: V-Dem Score vs. Democratic Governance by Membership Category (Faceted Scatter)
  plot_data_e2 <- df_membership_analysis %>%
    filter(!is.na(network_vdem_score) & !is.na(dem_gov) & !is.na(MembershipCategory))
  
  if(nrow(plot_data_e2) > 0) {
    plot_vdem_vs_demgov_by_memtype_facet <- ggplot(plot_data_e2, aes(x = network_vdem_score, y = dem_gov)) +
      geom_point(aes(color = MembershipCategory), size = 2.5, alpha = 0.8, na.rm = TRUE) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(aes(label = network), size = 2.5, max.overlaps = 10, na.rm = TRUE,
                      segment.color = 'grey70', segment.alpha = 0.5) +
      facet_wrap(~MembershipCategory, scales = "fixed") +
      labs(title = "Network V-Dem Score vs. Democratic Governance Score",
           subtitle = "Faceted by Membership Type (Trend line for each facet)",
           x = "Network Average V-Dem Polyarchy Score",
           y = "Democratic Governance Score") +
      scale_color_manual(values = membership_colors, name = "Membership Type") +
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            strip.text = element_text(face = "bold")) +
      scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1))
    
    # print(plot_vdem_vs_demgov_by_memtype_facet)
    ggsave("plot_vdem_vs_demgov_by_memtype_facet.svg", plot = plot_vdem_vs_demgov_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_vdem_vs_demgov_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_vdem_vs_demgov_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
  # E.3: V-Dem Score vs. Combined Authority by Membership Category (Faceted Scatter)
  plot_data_e3 <- df_membership_analysis %>%
    filter(!is.na(network_vdem_score) & !is.na(combined_pd_score) & !is.na(MembershipCategory))
  
  if(nrow(plot_data_e3) > 0) {
    plot_vdem_vs_auth_by_memtype_facet <- ggplot(plot_data_e3, aes(x = network_vdem_score, y = combined_pd_score)) +
      geom_point(aes(color = MembershipCategory), size = 2.5, alpha = 0.8, na.rm = TRUE) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(aes(label = network), size = 2.5, max.overlaps = 10, na.rm = TRUE,
                      segment.color = 'grey70', segment.alpha = 0.5) +
      facet_wrap(~MembershipCategory, scales = "fixed") +
      labs(title = "Network V-Dem Score vs. Combined Authority",
           subtitle = "Faceted by Membership Type (Trend line for each facet)",
           x = "Network Average V-Dem Polyarchy Score",
           y = "Combined Pooling & Delegation Score") +
      scale_color_manual(values = membership_colors, name = "Membership Type") +
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            strip.text = element_text(face = "bold")) +
      scale_x_continuous(limits = c(0,1))
    
    # print(plot_vdem_vs_auth_by_memtype_facet)
    ggsave("plot_vdem_vs_auth_by_memtype_facet.svg", plot = plot_vdem_vs_auth_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_vdem_vs_auth_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_vdem_vs_auth_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
  # E.4: Box Plot - Distribution of Democratic Governance Scores by Membership Category
  plot_data_e4 <- df_membership_analysis %>% filter(!is.na(dem_gov) & !is.na(MembershipCategory))
  if(nrow(plot_data_e4) > 0) {
    plot_boxplot_demgov_by_memtype <- ggplot(plot_data_e4, aes(x = MembershipCategory, y = dem_gov, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) + # Hide default outliers
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Democratic Governance Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Democratic Governance Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # print(plot_boxplot_demgov_by_memtype)
    ggsave("plot_boxplot_demgov_by_memtype.svg", plot = plot_boxplot_demgov_by_memtype, 
           width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_demgov_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_demgov_by_memtype: Not enough data after NA removal.\n")
  }
  
  # E.5: Box Plot - Distribution of Combined Authority Scores by Membership Category
  plot_data_e5 <- df_membership_analysis %>% filter(!is.na(combined_pd_score) & !is.na(MembershipCategory))
  if(nrow(plot_data_e5) > 0) {
    plot_boxplot_auth_by_memtype <- ggplot(plot_data_e5, aes(x = MembershipCategory, y = combined_pd_score, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Combined Authority Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Combined Pooling & Delegation Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # print(plot_boxplot_auth_by_memtype)
    ggsave("plot_boxplot_auth_by_memtype.svg", plot = plot_boxplot_auth_by_memtype, 
           width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_auth_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_auth_by_memtype: Not enough data after NA removal.\n")
  }
  
  # E.6: Box Plot - Distribution of Network V-Dem Scores by Membership Category
  plot_data_e6 <- df_membership_analysis %>% filter(!is.na(network_vdem_score) & !is.na(MembershipCategory))
  if(nrow(plot_data_e6) > 0) {
    plot_boxplot_vdem_by_memtype <- ggplot(plot_data_e6, aes(x = MembershipCategory, y = network_vdem_score, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Network V-Dem Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Network Average V-Dem Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1))
    
    # print(plot_boxplot_vdem_by_memtype)
    ggsave("plot_boxplot_vdem_by_memtype.svg", plot = plot_boxplot_vdem_by_memtype, 
           width = academic_wide_width, height = academic_single_height*1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_vdem_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_vdem_by_memtype: Not enough data after NA removal.\n")
  }
  
  # E.7: Bar Plots of Average Scores by Membership Category
  # E.7.1: Average Democratic Governance Score
  avg_demgov_by_memtype <- df_membership_analysis %>%
    filter(!is.na(MembershipCategory) & !is.na(dem_gov)) %>%
    group_by(MembershipCategory) %>%
    summarise(avg_score = mean(dem_gov, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(avg_score))
  
  if(nrow(avg_demgov_by_memtype) > 0){
    plot_avg_demgov_by_memtype <- ggplot(avg_demgov_by_memtype, aes(x = reorder(MembershipCategory, -avg_score), y = avg_score, fill = MembershipCategory)) +
      geom_col(width=0.7, na.rm=TRUE) +
      geom_text(aes(label = round(avg_score, 2)), vjust = -0.5, na.rm=TRUE) +
      labs(title = "Average Democratic Governance Score",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Average Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1)) # Assuming scores are 0-1
    
    # print(plot_avg_demgov_by_memtype)
    ggsave("plot_avg_demgov_by_memtype.svg", plot = plot_avg_demgov_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_demgov_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_demgov_by_memtype: Not enough data for averaging.\n")
  }
  
  # E.7.2: Average Combined Authority Score
  avg_auth_by_memtype <- df_membership_analysis %>%
    filter(!is.na(MembershipCategory) & !is.na(combined_pd_score)) %>%
    group_by(MembershipCategory) %>%
    summarise(avg_score = mean(combined_pd_score, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(avg_score))
  
  if(nrow(avg_auth_by_memtype) > 0){
    plot_avg_auth_by_memtype <- ggplot(avg_auth_by_memtype, aes(x = reorder(MembershipCategory, -avg_score), y = avg_score, fill = MembershipCategory)) +
      geom_col(width=0.7, na.rm=TRUE) +
      geom_text(aes(label = round(avg_score, 2)), vjust = -0.5, na.rm=TRUE) +
      labs(title = "Average Combined Authority Score",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Average Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # print(plot_avg_auth_by_memtype)
    ggsave("plot_avg_auth_by_memtype.svg", plot = plot_avg_auth_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_auth_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_auth_by_memtype: Not enough data for averaging.\n")
  }
  
  # E.7.3: Average Network V-Dem Score
  avg_vdem_by_memtype <- df_membership_analysis %>%
    filter(!is.na(MembershipCategory) & !is.na(network_vdem_score)) %>%
    group_by(MembershipCategory) %>%
    summarise(avg_score = mean(network_vdem_score, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(avg_score))
  
  if(nrow(avg_vdem_by_memtype) > 0){
    plot_avg_vdem_by_memtype <- ggplot(avg_vdem_by_memtype, aes(x = reorder(MembershipCategory, -avg_score), y = avg_score, fill = MembershipCategory)) +
      geom_col(width=0.7, na.rm=TRUE) +
      geom_text(aes(label = round(avg_score, 2)), vjust = -0.5, na.rm=TRUE) +
      labs(title = "Average Network V-Dem Score",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Average Score") +
      scale_fill_manual(values = membership_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1)) # V-Dem scores are 0-1
    
    # print(plot_avg_vdem_by_memtype)
    ggsave("plot_avg_vdem_by_memtype.svg", plot = plot_avg_vdem_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_vdem_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_vdem_by_memtype: Not enough data for averaging.\n")
  }
  
} else {
  cat("Skipping Plots by Membership Category: df_for_new_scatterplot is not available or empty.\n")
}

cat("\n--- End of Plots by Membership Category ---\n")
# --- I. SETUP ---
# Clear the environment (optional: uncomment if needed)
# rm(list = ls())

# --- START OF MODIFIED CODE FOR OS-DEPENDENT WORKING DIRECTORY ---
# Set working directory based on OS
if (Sys.info()['sysname'] == "Windows") {
  setwd("C:\\Users\\WKSTN\\OneDrive - University of Warwick\\Macroprudential Paper October 24\\2025\\r_output")
  cat("Operating System: Windows. Working directory set to Windows path.\n")
} else if (Sys.info()['sysname'] == "Darwin") { # Darwin is for macOS
  setwd("/Users/ciaran/Library/CloudStorage/OneDrive-UniversityofWarwick/Macroprudential Paper October 24/2025/r_output")
  cat("Operating System: macOS (Darwin). Working directory set to macOS path.\n")
} else {
  cat("Warning: Operating system not Windows or macOS. Manually set working directory if needed.\n")
  cat("Current R working directory:", getwd(), "\n")
}
# --- END OF MODIFIED CODE FOR OS-DEPENDENT WORKING DIRECTORY ---

# Load libraries
# Ensure all necessary libraries from your original script are loaded here.
# I'm including the ones explicitly used in the modified sections.
library(readxl)
library(dplyr)
library(tidyr) # For pivot_longer, pivot_wider
library(zoo)   # For rollmean
library(ggplot2)
library(ggrepel)

cat("Setup complete. Libraries loaded.\n")

# --- III. DATA LOADING AND INITIAL PREPARATION (Sections related to media salience) ---

cat("\n--- Loading and Processing Media Salience Data ---\n")

# --- Part A: Load and Process Financial Media Data (financial_data) ---

# A.1 Define the path to your financial media Excel file
media_excel_file_path <- "financial_tgn_media_nexis.xlsx" # Assuming it's in the working directory
cat("Media data Excel file path:", media_excel_file_path, "\n")

# A.2 Read the media data Excel file
financial_data_wide <- tryCatch({
  read_excel(media_excel_file_path)
}, error = function(e) {
  cat("Error reading financial media Excel file:", e$message, "\n")
  return(NULL)
})

financial_data_long_for_lookup <- NULL # Initialize

if (!is.null(financial_data_wide) && nrow(financial_data_wide) > 0) {
  cat("Successfully loaded financial_data_wide. Dimensions:", dim(financial_data_wide)[1], "rows,", dim(financial_data_wide)[2], "cols.\n")
  
  year_column_name_media <- names(financial_data_wide)[1] # Assuming first column is Year
  network_raw_columns <- setdiff(names(financial_data_wide), year_column_name_media)
  
  # A.3 Pivot raw mentions to long format
  financial_data_mentions_long <- financial_data_wide %>%
    select(all_of(year_column_name_media), all_of(network_raw_columns)) %>%
    pivot_longer(cols = all_of(network_raw_columns),
                 names_to = "network_raw_col_name",
                 values_to = "mentions") %>%
    # Assuming network acronyms are the column names (e.g., "BCBS", "EBA")
    # If they have suffixes like "_mentions", adjust here.
    # For this example, I'll assume column names are direct network acronyms.
    # Let's ensure network names are consistent (e.g., lowercase for internal processing)
    mutate(network = tolower(network_raw_col_name)) %>%
    select(!!sym(year_column_name_media), network, mentions)
  
  # A.4 Calculate 3-year rolling average for each network using the long format
  cat("Calculating 3-year rolling averages for media data (long format)...\n")
  financial_data_long_for_lookup <- financial_data_mentions_long %>%
    group_by(network) %>%
    arrange(!!sym(year_column_name_media), .by_group = TRUE) %>%
    mutate(
      mentions_roll_avg_3yr = zoo::rollmean(mentions,
                                            k = 3,
                                            fill = NA,
                                            align = "right",
                                            na.rm = TRUE) # na.rm = TRUE inside rollmean handles NAs in the window
    ) %>%
    ungroup()
  
  # Replace NaN produced by rollmean (if all values in window are NA) with NA
  financial_data_long_for_lookup$mentions_roll_avg_3yr[is.nan(financial_data_long_for_lookup$mentions_roll_avg_3yr)] <- NA
  
  cat("Rolling averages calculated and financial_data_long_for_lookup created.\n")
  # print(head(financial_data_long_for_lookup, 10)) # Optional: view
  
} else {
  warning("Failed to load or financial_data_wide is empty. Subsequent steps will be affected.")
  financial_data_long_for_lookup <- tibble(
    Year = integer(), # Adjust type if year is not integer
    network = character(), 
    mentions = numeric(),
    mentions_roll_avg_3yr = numeric()
  )
}


# --- Part B: Load Charter Date Information and Create Network Event Scores ---

# B.1 Define the charter date Excel file name
charter_file_name <- "charter_year.xlsx" # Assuming it's in the working directory
cat("Charter date Excel file name:", charter_file_name, "\n")

# B.2 Read the charter date Excel file
charter_dates_raw <- tryCatch({
  read_excel(charter_file_name)
}, error = function(e) {
  cat("Error reading charter_year.xlsx:", e$message, "\n")
  return(NULL)
})

network_event_scores <- tibble() # Initialize an empty tibble

if (!is.null(charter_dates_raw) && nrow(charter_dates_raw) > 0 && !is.null(financial_data_long_for_lookup)) {
  cat("Successfully loaded charter_dates_raw. Dimensions:", dim(charter_dates_raw)[1], "rows,", dim(charter_dates_raw)[2], "cols.\n")
  
  # B.3 Define expected column names in charter_dates_raw
  # These should be exactly as in your Excel file: network, charter_year_recent, charter_year_initial
  expected_network_col_charter <- "network" 
  expected_initial_year_col_charter <- "charter_year_initial"
  expected_recent_year_col_charter <- "charter_year_recent"
  
  # Validate column names
  if (!all(c(expected_network_col_charter, expected_initial_year_col_charter, expected_recent_year_col_charter) %in% names(charter_dates_raw))) {
    stop(paste("One or more expected columns ('",expected_network_col_charter,"', '",expected_initial_year_col_charter,"', '",expected_recent_year_col_charter,"') not found in", charter_file_name))
  }
  
  # Rename columns for clarity and ensure network is lowercase for matching
  charter_events_processed <- charter_dates_raw %>%
    mutate(network_lc = tolower(!!sym(expected_network_col_charter))) %>% # network to lowercase
    select(network_lc,
           initial_year = !!sym(expected_initial_year_col_charter),
           recent_year = !!sym(expected_recent_year_col_charter)) %>%
    pivot_longer(cols = c("initial_year", "recent_year"),
                 names_to = "event_type_raw",
                 values_to = "event_year") %>%
    filter(!is.na(event_year)) %>% # Remove rows if a year is NA
    mutate(event_type = ifelse(event_type_raw == "initial_year", "Initial", "Recent")) %>%
    distinct() # Ensure unique network-event_year-event_type combinations
  
  cat("Processing each network event for politicization scores...\n")
  
  for (i in 1:nrow(charter_events_processed)) {
    current_network_lc <- charter_events_processed$network_lc[i]
    current_event_year <- charter_events_processed$event_year[i]
    current_event_type <- charter_events_processed$event_type[i]
    
    politicization_score_for_event <- NA_real_
    
    # Try to get the network's own 3-year rolling average
    own_rolling_avg <- financial_data_long_for_lookup %>%
      filter(network == current_network_lc & !!sym(year_column_name_media) == current_event_year) %>%
      pull(mentions_roll_avg_3yr)
    
    if (length(own_rolling_avg) == 1 && !is.na(own_rolling_avg)) {
      politicization_score_for_event <- own_rolling_avg
    } else {
      # If own rolling average is NA, and it's an "Initial" event, calculate proxy
      if (current_event_type == "Initial") {
        cat("Calculating proxy for:", current_network_lc, "in year", current_event_year, "(Initial event)\n")
        
        proxy_data <- financial_data_long_for_lookup %>%
          filter(!!sym(year_column_name_media) == current_event_year & network != current_network_lc) %>%
          filter(!is.na(mentions_roll_avg_3yr)) # Only use non-NA rolling averages for proxy
        
        if (nrow(proxy_data) > 0) {
          politicization_score_for_event <- mean(proxy_data$mentions_roll_avg_3yr, na.rm = TRUE)
          cat("Proxy calculated:", politicization_score_for_event, "\n")
        } else {
          cat("Warning: No data available to calculate proxy for", current_network_lc, "in year", current_event_year, ". Score remains NA.\n")
          politicization_score_for_event <- NA_real_
        }
      } else {
        # For "Recent" events, if own_rolling_avg is NA, it stays NA (no proxy)
        cat("Own rolling average is NA for recent event:", current_network_lc, "in year", current_event_year, ". Score remains NA.\n")
        politicization_score_for_event <- NA_real_
      }
    }
    
    network_event_scores <- bind_rows(
      network_event_scores,
      tibble(
        network_lc = current_network_lc, # Keep lowercase for now
        event_year = current_event_year,
        event_type = current_event_type,
        raw_politicization_score = politicization_score_for_event
      )
    )
  }
  cat("Finished processing network event scores.\n")
  # print(network_event_scores) # Optional: view
  
} else {
  if (is.null(charter_dates_raw) || nrow(charter_dates_raw) == 0) {
    warning(paste(charter_file_name, "could not be loaded or is empty."))
  }
  if (is.null(financial_data_long_for_lookup)) {
    warning("financial_data_long_for_lookup is not available.")
  }
  network_event_scores <- tibble(network_lc=character(), event_year=integer(), event_type=character(), raw_politicization_score=numeric())
}
cat("--- End of Media Salience Data Processing ---\n")


# --- IV. CORE DATA PROCESSING (Placeholder for your existing code) ---
# Assume your existing code from Section IV runs here to create:
# df_appendix, df_for_new_scatterplot, etc.
# For this example, I'll create a dummy df_for_new_scatterplot
# Replace this with your actual data loading and processing for these frames.

# Dummy df_for_new_scatterplot for demonstration purposes
# In your actual script, this would be generated by your existing complex logic
# Ensure it has 'network' (UPPERCASE), 'dem_gov', 'combined_pd_score', etc.
if (!exists("df_for_new_scatterplot")) { # Only create if not already created by your full script
  cat("Creating dummy df_for_new_scatterplot for demonstration.\n")
  # Sample networks - ensure these are present in your charter_year.xlsx (but in uppercase here)
  sample_networks_upper <- c("EBA", "BCBS", "FSB", "ESMA", "EIOPA") 
  df_for_new_scatterplot <- tibble(
    network = sample_networks_upper,
    dem_gov = runif(length(sample_networks_upper), 0.2, 0.8),
    combined_pd_score = runif(length(sample_networks_upper), 0.1, 0.9),
    pool01 = runif(length(sample_networks_upper), 0, 1),
    del01 = runif(length(sample_networks_upper), 0, 1),
    network_vdem_score = runif(length(sample_networks_upper), 0.3, 0.9),
    region = sample(c("Global", "European"), length(sample_networks_upper), replace = TRUE),
    MembershipCategory = sample(c("Regulator", "Both", "Central bank"), length(sample_networks_upper), replace = TRUE)
    # Add other columns present in your actual df_for_new_scatterplot
  )
  # Ensure 'network' is factor if other parts of your script expect it
  df_for_new_scatterplot$network <- as.factor(df_for_new_scatterplot$network)
  df_for_new_scatterplot$region <- as.factor(df_for_new_scatterplot$region)
  df_for_new_scatterplot$MembershipCategory <- as.factor(df_for_new_scatterplot$MembershipCategory)
} else {
  cat("Using existing df_for_new_scatterplot.\n")
}
# Ensure 'network' column in df_for_new_scatterplot is character for the join, then convert back if needed.
# Or ensure the joining key in politicization_data_final is also factor.
# For safety, convert to character for join:
df_for_new_scatterplot <- df_for_new_scatterplot %>% mutate(network = as.character(network))


# --- VI. ADDENDUM: MERGE POLITICIZATION SCORE AND GENERATE PLOT BATTERY ---
cat("\n--- Starting Addendum: Merging Politicization Score & Generating Plot Battery ---\n")

df_plot_battery_final <- NULL # Initialize

if (exists("network_event_scores") && is.data.frame(network_event_scores) && nrow(network_event_scores) > 0) {
  
  cat("Preparing politicization score data for merge...\n")
  
  politicization_data_final <- network_event_scores %>%
    mutate(
      network = toupper(network_lc), # Convert network to UPPERCASE for merging
      politicization_score_numeric = as.numeric(raw_politicization_score),
      politicization_score_sqrt = ifelse(politicization_score_numeric >= 0, sqrt(politicization_score_numeric), NA_real_),
      point_label = paste0(network, "_", event_year)
    ) %>%
    select(network, event_year, event_type, politicization_score_numeric, politicization_score_sqrt, point_label)
  
  cat("Politicization data prepared for merge:\n")
  # print(head(politicization_data_final)) # Optional: view
  
  if (exists("df_for_new_scatterplot") && is.data.frame(df_for_new_scatterplot) && nrow(df_for_new_scatterplot) > 0) {
    cat("\nMerging politicization score into df_for_new_scatterplot...\n")
    
    # Perform the join
    df_plot_battery_final <- df_for_new_scatterplot %>%
      left_join(politicization_data_final, by = "network") %>%
      # Ensure other key numeric columns are indeed numeric
      mutate(
        dem_gov_numeric = as.numeric(dem_gov), # Assuming dem_gov is the original column name
        combined_pd_score_numeric = as.numeric(combined_pd_score),
        pool01_numeric = as.numeric(pool01),
        del01_numeric = as.numeric(del01),
        network_vdem_score_numeric = as.numeric(network_vdem_score)
      ) %>%
      filter(!is.na(event_year)) # Keep only rows that successfully merged with an event year
    
    cat("Merge complete. Dimensions of df_plot_battery_final:", dim(df_plot_battery_final)[1], "rows,", dim(df_plot_battery_final)[2], "cols.\n")
    # print(head(df_plot_battery_final %>% select(network, point_label, politicization_score_sqrt, dem_gov_numeric))) # Optional
    
    # --- Plot 1 (User's Primary Request): SQRT(Politicization Score) vs. Democratic Governance Score ---
    cat("\nGenerating Plot 1: SQRT(Politicization Score) vs. Democratic Governance Score...\n")
    
    plot_df_temp1 <- df_plot_battery_final %>%
      filter(!is.na(politicization_score_sqrt) & !is.na(dem_gov_numeric))
    
    if(nrow(plot_df_temp1) > 0) {
      plot1_sqrt_pol_vs_demgov <- ggplot(plot_df_temp1, 
                                         aes(x = politicization_score_sqrt, y = dem_gov_numeric, label = point_label)) +
        geom_point(aes(color = event_type, shape = region), alpha = 0.8, size = 3, na.rm = TRUE) + # Added color by event_type and shape by region
        ggrepel::geom_text_repel(size = 3, color = "black", max.overlaps = 15, 
                                 segment.color = 'grey70', na.rm = TRUE, 
                                 box.padding = 0.4, point.padding = 0.4,
                                 min.segment.length = 0) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", na.rm = TRUE) +
        labs(title = "Politicization vs. Democratic Governance",
             subtitle = "Each point is a Network-Year (Initial/Recent Charter Year)",
             x = "SQRT(Politicization Score at Event Year)",
             y = "Democratic Governance Score (dem_gov)",
             color = "Event Type",
             shape = "Region") + # Added legend titles
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              legend.position = "bottom")
      
      print(plot1_sqrt_pol_vs_demgov) # Display the plot
      
      # Define academic plot sizes if not already defined globally in your script
      if (!exists("academic_wide_width")) academic_wide_width <- 10
      if (!exists("academic_single_height")) academic_single_height <- 6 # Adjusted for better aspect ratio
      if (!exists("academic_plot_dpi")) academic_plot_dpi <- 300
      
      ggsave("plot_sqrt_politicization_vs_demgov_dual_dates.svg", 
             plot = plot1_sqrt_pol_vs_demgov, 
             width = academic_wide_width, 
             height = academic_single_height * 1.3, # Adjusted height
             dpi = academic_plot_dpi)
      cat("Saved plot_sqrt_politicization_vs_demgov_dual_dates.svg\n")
    } else {
      cat("Insufficient data for Plot 1 after filtering NAs.\n")
    }
    
    # --- Plot 2: SQRT(Politicization Score) vs. Combined Authority (combined_pd_score) ---
    cat("\nGenerating Plot 2: SQRT(Politicization Score) vs. Combined Authority...\n")
    plot_df_temp2 <- df_plot_battery_final %>%
      filter(!is.na(politicization_score_sqrt) & !is.na(combined_pd_score_numeric))
    if(nrow(plot_df_temp2) > 0) {
      plot2_sqrt_pol_vs_combined_auth <- ggplot(plot_df_temp2, aes(x = politicization_score_sqrt, y = combined_pd_score_numeric, label = point_label)) +
        geom_point(aes(color = event_type, shape = region), alpha = 0.7, size = 3, na.rm = TRUE) +
        ggrepel::geom_text_repel(size = 3, color = "black", max.overlaps = 15, segment.color = 'grey70', na.rm = TRUE, box.padding = 0.4, point.padding = 0.4, min.segment.length = 0) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", na.rm = TRUE) +
        labs(title = "Politicization vs. Combined Authority",
             subtitle = "Each point is a Network-Year (Initial/Recent Charter Year)",
             x = "SQRT(Politicization Score at Event Year)", y = "Combined Pooling & Delegation Score (scaled)",
             color = "Event Type", shape = "Region") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                              plot.subtitle = element_text(hjust = 0.5, size = 10), 
                                              legend.position = "bottom")
      print(plot2_sqrt_pol_vs_combined_auth)
      ggsave("plot_sqrt_politicization_vs_combined_authority_dual_dates.svg", plot = plot2_sqrt_pol_vs_combined_auth, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
      cat("Saved plot_sqrt_politicization_vs_combined_authority_dual_dates.svg\n")
    } else { cat("Insufficient data for Plot 2.\n") }
    
    # --- Plot 3: SQRT(Politicization Score) vs. Pooling Score (pool01) ---
    cat("\nGenerating Plot 3: SQRT(Politicization Score) vs. Pooling Score...\n")
    plot_df_temp3 <- df_plot_battery_final %>%
      filter(!is.na(politicization_score_sqrt) & !is.na(pool01_numeric))
    if(nrow(plot_df_temp3) > 0) {
      plot3_sqrt_pol_vs_pooling <- ggplot(plot_df_temp3, aes(x = politicization_score_sqrt, y = pool01_numeric, label = point_label)) +
        geom_point(aes(color = event_type, shape = region), alpha = 0.7, size = 3, na.rm = TRUE) +
        ggrepel::geom_text_repel(size = 3, color = "black", max.overlaps = 15, segment.color = 'grey70', na.rm = TRUE, box.padding = 0.4, point.padding = 0.4, min.segment.length = 0) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", na.rm = TRUE) +
        labs(title = "Politicization vs. Pooling Score",
             subtitle = "Each point is a Network-Year (Initial/Recent Charter Year)",
             x = "SQRT(Politicization Score at Event Year)", y = "Pooling Score (pool01, scaled)",
             color = "Event Type", shape = "Region") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                              plot.subtitle = element_text(hjust = 0.5, size = 10), 
                                              legend.position = "bottom")
      print(plot3_sqrt_pol_vs_pooling)
      ggsave("plot_sqrt_politicization_vs_pooling_dual_dates.svg", plot = plot3_sqrt_pol_vs_pooling, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
      cat("Saved plot_sqrt_politicization_vs_pooling_dual_dates.svg\n")
    } else { cat("Insufficient data for Plot 3.\n") }
    
    # --- Plot 4: SQRT(Politicization Score) vs. Delegation Score (del01) ---
    cat("\nGenerating Plot 4: SQRT(Politicization Score) vs. Delegation Score...\n")
    plot_df_temp4 <- df_plot_battery_final %>%
      filter(!is.na(politicization_score_sqrt) & !is.na(del01_numeric))
    if(nrow(plot_df_temp4) > 0) {
      plot4_sqrt_pol_vs_delegation <- ggplot(plot_df_temp4, aes(x = politicization_score_sqrt, y = del01_numeric, label = point_label)) +
        geom_point(aes(color = event_type, shape = region), alpha = 0.7, size = 3, na.rm = TRUE) +
        ggrepel::geom_text_repel(size = 3, color = "black", max.overlaps = 15, segment.color = 'grey70', na.rm = TRUE, box.padding = 0.4, point.padding = 0.4, min.segment.length = 0) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", na.rm = TRUE) +
        labs(title = "Politicization vs. Delegation Score",
             subtitle = "Each point is a Network-Year (Initial/Recent Charter Year)",
             x = "SQRT(Politicization Score at Event Year)", y = "Delegation Score (del01, scaled)",
             color = "Event Type", shape = "Region") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                              plot.subtitle = element_text(hjust = 0.5, size = 10), 
                                              legend.position = "bottom")
      print(plot4_sqrt_pol_vs_delegation)
      ggsave("plot_sqrt_politicization_vs_delegation_dual_dates.svg", plot = plot4_sqrt_pol_vs_delegation, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
      cat("Saved plot_sqrt_politicization_vs_delegation_dual_dates.svg\n")
    } else { cat("Insufficient data for Plot 4.\n") }
    
    # Add other plots (e.g., vs V-Dem, by Membership Type) similarly, adapting to use df_plot_battery_final
    # and potentially distinguishing points by event_type or faceting if meaningful.
    # For example, Plot 5 vs V-Dem
    cat("\nGenerating Plot 5: SQRT(Politicization Score) vs. Network V-Dem Score...\n")
    plot_df_temp5 <- df_plot_battery_final %>%
      filter(!is.na(politicization_score_sqrt) & !is.na(network_vdem_score_numeric))
    if(nrow(plot_df_temp5) > 0) {
      plot5_sqrt_pol_vs_vdem <- ggplot(plot_df_temp5, aes(x = politicization_score_sqrt, y = network_vdem_score_numeric, label = point_label)) +
        geom_point(aes(color = event_type, shape = region), alpha = 0.7, size = 3, na.rm = TRUE) +
        ggrepel::geom_text_repel(size = 3, color = "black", max.overlaps = 15, segment.color = 'grey70', na.rm = TRUE, box.padding = 0.4, point.padding = 0.4, min.segment.length = 0) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed", na.rm = TRUE) +
        labs(title = "Politicization vs. Network V-Dem Score",
             subtitle = "Each point is a Network-Year (Initial/Recent Charter Year)",
             x = "SQRT(Politicization Score at Event Year)", y = "Network Average V-Dem Polyarchy Score",
             color = "Event Type", shape = "Region") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                              plot.subtitle = element_text(hjust = 0.5, size = 10), 
                                              legend.position = "bottom") +
        scale_y_continuous(limits = c(0,1)) # V-Dem scores are typically 0-1
      print(plot5_sqrt_pol_vs_vdem)
      ggsave("plot_sqrt_politicization_vs_vdem_dual_dates.svg", plot = plot5_sqrt_pol_vs_vdem, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
      cat("Saved plot_sqrt_politicization_vs_vdem_dual_dates.svg\n")
    } else { cat("Insufficient data for Plot 5.\n") }
    
    # Plot 6: Politicization Score (Raw) by Membership Type (MembershipCategory) - Box Plot
    # This plot might need more thought if you want to show Initial/Recent distinctly
    # For now, it will aggregate all points for a given MembershipCategory.
    # Or, you could facet by event_type.
    cat("\nGenerating Plot 6: Politicization Score (Raw) by Membership Type & Event Type...\n")
    plot_df_temp6 <- df_plot_battery_final %>% 
      filter(!is.na(politicization_score_numeric) & !is.na(MembershipCategory) & !is.na(event_type))
    if(nrow(plot_df_temp6) > 0 && length(unique(plot_df_temp6$MembershipCategory[!is.na(plot_df_temp6$MembershipCategory)])) > 0) {
      plot6_pol_by_memtype_event_boxplot <- ggplot(plot_df_temp6, aes(x = MembershipCategory, y = politicization_score_numeric, fill = MembershipCategory)) +
        geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size = 2) +
        facet_wrap(~event_type) + # Facet by event_type
        labs(title = "Politicization Score (Raw) by Membership & Event Type",
             x = "Membership Type", y = "Politicization Score (Media Salience)") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "none", # Fill is redundant with x-axis
              strip.text = element_text(face="bold")) 
      print(plot6_pol_by_memtype_event_boxplot)
      ggsave("plot_politicization_by_membership_event_boxplot.svg", plot = plot6_pol_by_memtype_event_boxplot, width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
      cat("Saved plot_politicization_by_membership_event_boxplot.svg\n")
    } else { cat("Insufficient data or no valid membership categories/event types for Plot 6.\n") }
    
    
  } else {
    warning("df_for_new_scatterplot not found or is empty. Politicization score could not be merged or plotted.")
  }
} else {
  warning("network_event_scores was not created, was empty, or was not a data frame. Please ensure prerequisite steps ran successfully.")
}

cat("\n--- End of Addendum: Politicization Score Merged & Plot Battery Generated & Saved ---\n")

# Assuming dataTGN2.1_long_calculated is already created as per your script's logic.
# This dataframe should have columns: network, region, dimension, dim_value

if (exists("dataTGN2.1_long_calculated") && nrow(dataTGN2.1_long_calculated) > 0) {
  
  # Definition of plot size variables if not already defined:
  # academic_very_wide_width <- 12 # Example, adjust as needed
  # academic_single_height <- 6    # Example, adjust as needed
  # academic_plot_dpi <- 300
  
  plot_dimension_scores_by_region_jitter_style <-
    ggplot(dataTGN2.1_long_calculated, aes(x = region, y = dim_value, color = region)) +
    geom_jitter(width = 0.15, alpha = 0.6, na.rm = TRUE, size = 2) + # Individual network scores for each dimension
    stat_summary(
      geom = "point",
      fun = "mean",
      size = 4,
      shape = 24, # Upward triangle
      fill = "red",
      color = "red", # Ensures border is also red
      na.rm = TRUE
    ) +
    facet_wrap(~ dimension, scales = "fixed") + # MODIFIED: Changed "free_y" to "fixed" to unify y-axes
    coord_cartesian(ylim = c(0, 1)) +            # ADDED: Sets y-axis range to 0-1 for all facets
    labs(
      title = "Governance Dimension Scores by Region",
      x = "Region",
      y = "Network Score for Dimension"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      strip.text = element_text(face = "bold", size = 12), # Facet titles (input, output, throughput)
      legend.position = "none", # Colors map to x-axis categories within each facet
      axis.text.x = element_text(size = 10),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      panel.spacing = unit(1.5, "lines") # Adjust spacing between facets if needed
    )
  
  #To display the plot in your R environment:
  print(plot_dimension_scores_by_region_jitter_style)
  
  # To save the plot:
  ggsave("plot_dimension_scores_by_region_jitter.svg",
         plot = plot_dimension_scores_by_region_jitter_style,
         width = academic_very_wide_width, # Adjust width as needed, e.g., 12 or 14
         height = academic_single_height,    # Adjust height as needed, e.g., 5 or 6
         dpi = academic_plot_dpi)
  cat("Saved plot_dimension_scores_by_region_jitter.svg\n")
  
} else {
  cat("Skipping plot: dataTGN2.1_long_calculated is not available or empty.\n")
}

cat("\n--- End of Modified Script Sections ---\n")


cat("\n--- End of Combined Script ---\n")