# Combined and Refactored R Script - Optimized Version

# --- I. SETUP ---

# Clear the environment (optional: uncomment if needed)
rm(list = ls())

# --- Set working directory based on OS ---
if (Sys.info()['sysname'] == "Windows") {
  # For Windows, working directory is the 'r_output' folder
  setwd("C:\\Users\\WKSTN\\OneDrive - University of Warwick\\Macroprudential Paper October 24\\2025\\r_output")
  cat("Operating System: Windows. Working directory set to Windows path.\n")
} else if (Sys.info()['sysname'] == "Darwin") { # Darwin is for macOS
  # For macOS, working directory is directly the 'R Data' subfolder
  setwd("/Users/ciaran/Library/CloudStorage/OneDrive-UniversityofWarwick/Macroprudential Paper October 24/2025/r_output/R Data")
  cat("Operating System: macOS (Darwin). Working directory set to macOS path.\n")
} else {
  cat("Warning: Operating system not Windows or macOS. Manually set working directory if needed.\n")
  cat("Current R working directory:", getwd(), "\n")
}
# The script assumes 'demgov_int.xlsx', 'demgov_eur.xlsx', 'auth.xlsx', 'financial_tgn_media_nexis.xlsx',
# 'charter_year.xlsx', and the newly created CSV files ('country_name_mapping.csv', 'eu_member_states.csv',
# 'eu_network_entities.csv', 'organization_metadata.csv', 'vdem_membership_data.csv', 'df_granular.csv')
# are accessible from this working directory (either directly or in specified subfolders).

# Load libraries
# Only essential libraries are loaded to keep the environment clean.
library(devtools) # For install_github if vdemdata is not installed via CRAN
library(vdemdata)
library(dplyr) # For data manipulation (e.g., mutate, filter, select, join)
library(tidyr) # For data reshaping (e.g., pivot_longer)
library(tibble) # For tibble data frames
library(ggplot2) # For creating high-quality plots
library(ggrepel) # For non-overlapping text labels in ggplot2
library(readxl) # For reading Excel files
library(forcats) # For working with factors (e.g., reordering levels)
library(plotrix) # Provides functions for plotting, including radial plots (though not explicitly used in current ggplot sections)
library(ggthemes) # Additional themes for ggplot2
library(scales) # Functions for customizing plot scales (e.g., percentage formatting)
library(corrplot) # For visualizing correlation matrices
library(svglite) # For saving plots as SVG files
library(zoo)    # For time-series objects and rolling mean calculations

# Define standard plot saving parameters
academic_single_width <- 7 # inches (Block 3 definition)
academic_single_height <- 5 # inches (Block 3 definition)
academic_wide_width <- 10 # inches (Block 3 definition)
academic_very_wide_width <- 12 # inches (Block 3 definition)
academic_plot_dpi <- 300 # Dots per inch

cat("Setup complete. Libraries loaded and parameters set.\n")

# --- II. HELPER FUNCTION DEFINITIONS ---
# These functions encapsulate the data processing logic, promoting reusability and readability.

# Function 1: Aggregate sub-indicators into main governance components
aggregate_indicators <- function(df) {
  df %>%
    transmute(
      network = network,
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
    )
}

# Function 2: Scale aggregated indicators to a 0-1 range for comparability
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

# Function 3: Convert scaled indicators from wide to long format for ggplot2
pivot_scaled_indicators_long <- function(df_scaled_wide) {
  cols_to_keep <- "network"
  if ("region" %in% names(df_scaled_wide)) {
    cols_to_keep <- c(cols_to_keep, "region")
  }
  
  df_scaled_wide %>%
    select(all_of(cols_to_keep), ends_with("01")) %>%
    pivot_longer(cols = ends_with("01"), 
                 names_to = "indicator", 
                 values_to = "value") %>%
    mutate(indicator = factor(indicator))
}

# Function 4: Assign each indicator to its corresponding governance dimension (input, throughput, output)
add_governance_dimension <- function(df_long) {
  df_long %>%
    mutate(dimension = factor(case_when(
      indicator %in% c("in_part01", "in_rep01", "in_shb01", "in_epi01") ~ "input",
      indicator %in% c("through_tr01", "through_leg01", "through_acc01", "through_reas01") ~ "throughput",
      indicator %in% c("out_effec01", "out_effic01") ~ "output"
    ), levels = c("input", "throughput", "output")))
}

# Function 5: Categorize networks into 'Global' or 'European' regions
add_region_column <- function(df_input) {
  df_input %>%
    mutate(region = factor(case_when(
      network %in% c("BCBS", "IADI", "IAIS", "IOPS", "CPMI", "IOSCO", "FSB", "BIS", "NGFS", "IFRS") ~ "Global",
      network %in% c("EIOPA", "EBA", "ESMA", "ESRB", "SSB", "CEBS", "CEIOPS", "CESR") ~ "European"
    ), levels = c("Global", "European")))
}

# Function 6: Calculate the average dimension score for each network (and optionally region)
calculate_average_dimension_score <- function(df_with_dimensions) {
  grouping_vars <- "network"
  if ("region" %in% names(df_with_dimensions)) {
    grouping_vars <- c(grouping_vars, "region")
  }
  grouping_vars <- c(grouping_vars, "dimension")
  
  df_with_dimensions %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(dim_value = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
}

# Function 7: Calculate the overall democratic governance score for each network
calculate_dem_gov_score <- function(df_long_scaled_with_region_and_dim) {
  if ("region" %in% names(df_long_scaled_with_region_and_dim)) {
    df_long_scaled_with_region_and_dim %>%
      group_by(network, region) %>%
      summarise(dem_gov = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
  } else {
    df_long_scaled_with_region_and_dim %>%
      group_by(network) %>%
      summarise(dem_gov = round(mean(value, na.rm = TRUE), digits = 2), .groups = 'drop')
  }
}

# Function 8: Categorize democratic governance scores into 'low', 'medium', or 'high'
categorize_dem_gov_score <- function(df_with_dem_gov) {
  df_with_dem_gov %>%
    mutate(score = factor(case_when(
      dem_gov < 0.4 ~ "low",
      dem_gov >= 0.4 & dem_gov <= 0.59 ~ "medium",
      dem_gov > 0.59 ~ "high"
    ), levels = c("low", "medium", "high")))
}

# Function 9: Master processing function for governance data pipelines
process_governance_data <- function(raw_df, dataset_name, is_combined_data = FALSE) {
  cat("\nProcessing dataset:", dataset_name, "\n")
  
  df_aggregated <- aggregate_indicators(raw_df)
  cat("Dimensions after aggregate_indicators: ", paste(dim(df_aggregated)), "\n")
  
  df_scaled_wide <- scale_indicators(df_aggregated)
  cat("Dimensions after scale_indicators: ", paste(dim(df_scaled_wide)), "\n")
  
  if (is_combined_data) {
    if(!("region" %in% names(df_scaled_wide))) {
      df_scaled_wide <- add_region_column(df_scaled_wide)
      cat("Dimensions after add_region_column: ", paste(dim(df_scaled_wide)), "\n")
    }
  }
  
  df_long_scaled <- pivot_scaled_indicators_long(df_scaled_wide)
  cat("Dimensions after pivot_scaled_indicators_long: ", paste(dim(df_long_scaled)), "\n")
  
  df_with_dimensions <- add_governance_dimension(df_long_scaled)
  cat("Dimensions after add_governance_dimension: ", paste(dim(df_with_dimensions)), "\n")
  
  df_avg_dim_score <- calculate_average_dimension_score(df_with_dimensions)
  cat("Dimensions for avg_dim_score: ", paste(dim(df_avg_dim_score)), "\n")
  
  df_dem_gov <- calculate_dem_gov_score(df_with_dimensions)
  df_dem_gov_categorized <- categorize_dem_gov_score(df_dem_gov)
  cat("Dimensions for dem_gov_categorized: ", paste(dim(df_dem_gov_categorized)), "\n")
  
  results <- list(
    data_long_scaled_with_dimensions = df_with_dimensions,
    data_avg_dim_scores = df_avg_dim_score,
    data_dem_gov_categorized = df_dem_gov_categorized
  )
  
  if (is_combined_data) {
    results$data_avg_dim_scores_by_region_and_dim <- df_avg_dim_score
    results$data_proportions_by_region_score <- df_dem_gov_categorized %>%
      group_by(region, score) %>%
      summarise(n = n(), .groups = 'drop') %>%
      mutate(prop = n / sum(n, na.rm=TRUE))
    cat("Dimensions for data_proportions_by_region_score: ", paste(dim(results$data_proportions_by_region_score)), "\n")
  }
  
  return(results)
}

# Function 10: Process authority data (delegation and pooling)
process_auth_data <- function(auth_df_raw) {
  auth_df <- auth_df_raw %>%
    mutate(
      del_calc = del_body * ((del_budget + del_org + del_pol) / 3),
      pool_calc = ((pool_memb * pool_vote) + pool_bind) / 2,
      del = round(del_calc, digits = 2),
      pool = round(pool_calc, digits = 2)
    ) %>%
    mutate(
      del01 = round(del / 4, digits = 2),
      pool01 = round(pool / 5, digits = 2)
    ) %>%
    select(network, del, pool, del01, pool01)
  return(auth_df)
}

# Function 11: Identify outliers in a numeric vector
is_outlier <- function(x, na.rm = TRUE) {
  if (!is.numeric(x) || all(is.na(x))) return(rep(FALSE, length(x)))
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  if(any(is.na(qnt))) return(rep(FALSE, length(x)))
  H <- 1.5 * IQR(x, na.rm = na.rm)
  if(is.na(H)) return(rep(FALSE, length(x)))
  (x < (qnt[1] - H)) | (x > (qnt[2] + H))
}

# Function 12: Process V-Dem data and calculate network average V-Dem scores
process_vdem_and_network_scores <- function(membership_df_from_file, vdem_raw_data) {
  country_name_mapping_df <- read.csv("country_name_mapping.csv", stringsAsFactors = FALSE)
  country_name_mapping_internal <- setNames(country_name_mapping_df$V_Dem_Country, country_name_mapping_df$Jurisdiction)
  
  eu_member_states_internal <- read.csv("eu_member_states.csv", stringsAsFactors = FALSE)$CountryName
  eu_network_entities_internal <- read.csv("eu_network_entities.csv", stringsAsFactors = FALSE)$NetworkEntity
  
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
  
  membership_with_vdem_names <- membership_df_from_file %>%
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

# 1. Load primary data from Excel files
raw_mydata_intl <- read_excel('demgov_int.xlsx')
raw_eur_TGN <- read_excel("demgov_eur.xlsx")
raw_auth_data <- read_excel("auth.xlsx") # For Appendix 4

cat("\nPrimary Excel data loaded.\n")
cat("Initial dimensions of raw_mydata_intl: ", paste(dim(raw_mydata_intl)), "\n")
cat("Initial dimensions of raw_eur_TGN: ", paste(dim(raw_eur_TGN)), "\n")
cat("Initial dimensions of raw_auth_data: ", paste(dim(raw_auth_data)), "\n")

# 2. Load organization metadata from CSV
df_organization_metadata <- read.csv("organization_metadata.csv", stringsAsFactors = FALSE)
df_organization_metadata$MembershipCategory <- as.factor(df_organization_metadata$MembershipCategory)
cat("\nOrganization metadata (df_organization_metadata) created from CSV.\n")

# 3. Load V-Dem membership data from CSV
vdem_membership_data_df <- read.csv("vdem_membership_data.csv", stringsAsFactors = FALSE) %>%
  mutate(across(-Jurisdiction, ~ifelse(. == "X", TRUE, FALSE)))
cat("\nV-Dem membership data loaded from CSV.\n")

# 4. Load raw V-Dem data (downloads if not present locally, might take time first run)
raw_vdem_data <- vdemdata::vdem 
cat("\nRaw V-Dem data loaded.\n")

# --- Media Salience Data Loading and Processing ---
cat("\n--- Loading and Processing Media Salience Data ---\n")

media_excel_file_path <- "financial_tgn_media_nexis.xlsx" # Assumed in working directory or accessible path
cat("Media data Excel file path:", media_excel_file_path, "\n")

financial_data_wide <- tryCatch({
  read_excel(media_excel_file_path)
}, error = function(e) {
  cat("Error reading financial media Excel file:", e$message, "\n")
  return(NULL)
})

financial_data_long_for_lookup <- NULL

if (!is.null(financial_data_wide) && nrow(financial_data_wide) > 0) {
  cat("Successfully loaded financial_data_wide. Dimensions:", dim(financial_data_wide)[1], "rows,", dim(financial_data_wide)[2], "cols.\n")
  
  year_column_name_media <- names(financial_data_wide)[1]
  network_raw_columns <- setdiff(names(financial_data_wide), year_column_name_media)
  
  financial_data_mentions_long <- financial_data_wide %>%
    select(all_of(year_column_name_media), all_of(network_raw_columns)) %>%
    pivot_longer(cols = all_of(network_raw_columns),
                 names_to = "network_raw_col_name",
                 values_to = "mentions") %>%
    mutate(network = tolower(network_raw_col_name)) %>%
    select(!!sym(year_column_name_media), network, mentions)
  
  cat("Calculating 3-year rolling averages for media data (long format)...\n")
  financial_data_long_for_lookup <- financial_data_mentions_long %>%
    group_by(network) %>%
    arrange(!!sym(year_column_name_media), .by_group = TRUE) %>%
    mutate(
      mentions_roll_avg_3yr = zoo::rollmean(mentions,
                                            k = 3,
                                            fill = NA,
                                            align = "right",
                                            na.rm = TRUE)
    ) %>%
    ungroup()
  
  financial_data_long_for_lookup$mentions_roll_avg_3yr[is.nan(financial_data_long_for_lookup$mentions_roll_avg_3yr)] <- NA
  cat("Rolling averages calculated and financial_data_long_for_lookup created.\n")
} else {
  warning("Failed to load or financial_data_wide is empty. Subsequent steps will be affected.")
  financial_data_long_for_lookup <- tibble(
    Year = integer(), network = character(), mentions = numeric(), mentions_roll_avg_3yr = numeric()
  )
}

charter_file_name <- "charter_year.xlsx"
cat("Charter date Excel file name:", charter_file_name, "\n")

charter_dates_raw <- tryCatch({
  read_excel(charter_file_name)
}, error = function(e) {
  cat("Error reading charter_year.xlsx:", e$message, "\n")
  cat("Please ensure '", charter_file_name, "' exists in the working directory: ", getwd(), "\n")
  return(NULL)
})

network_event_scores <- tibble()

if (!is.null(charter_dates_raw) && nrow(charter_dates_raw) > 0 && !is.null(financial_data_long_for_lookup)) {
  cat("Successfully loaded charter_dates_raw. Dimensions:", dim(charter_dates_raw)[1], "rows,", dim(charter_dates_raw)[2], "cols.\n")
  
  expected_network_col_charter <- "network"
  expected_initial_year_col_charter <- "charter_year_initial"
  expected_recent_year_col_charter <- "charter_year_recent"
  
  if (!all(c(expected_network_col_charter, expected_initial_year_col_charter, expected_recent_year_col_charter) %in% names(charter_dates_raw))) {
    stop(paste("One or more expected columns ('",expected_network_col_charter,"', '",expected_initial_year_col_charter,"', '",expected_recent_year_col_charter,"') not found in", charter_file_name))
  }
  
  charter_events_processed <- charter_dates_raw %>%
    mutate(network_lc = tolower(!!sym(expected_network_col_charter))) %>%
    select(network_lc,
           initial_year = !!sym(expected_initial_year_col_charter),
           recent_year = !!sym(expected_recent_year_col_charter)) %>%
    pivot_longer(cols = c("initial_year", "recent_year"),
                 names_to = "event_type_raw",
                 values_to = "event_year") %>%
    filter(!is.na(event_year)) %>%
    mutate(event_type = ifelse(event_type_raw == "initial_year", "Initial", "Recent")) %>%
    distinct()
  
  cat("Processing each network event for politicization scores...\n")
  for (i in 1:nrow(charter_events_processed)) {
    current_network_lc <- charter_events_processed$network_lc[i]
    current_event_year <- charter_events_processed$event_year[i]
    current_event_type <- charter_events_processed$event_type[i]
    
    politicization_score_for_event <- NA_real_
    
    own_rolling_avg <- financial_data_long_for_lookup %>%
      filter(network == current_network_lc & !!sym(year_column_name_media) == current_event_year) %>%
      pull(mentions_roll_avg_3yr)
    
    if (length(own_rolling_avg) == 1 && !is.na(own_rolling_avg)) {
      politicization_score_for_event <- own_rolling_avg
    } else {
      if (current_event_type == "Initial") {
        cat("Calculating proxy for:", current_network_lc, "in year", current_event_year, "(Initial event)\n")
        proxy_data <- financial_data_long_for_lookup %>%
          filter(!!sym(year_column_name_media) == current_event_year & network != current_network_lc) %>%
          filter(!is.na(mentions_roll_avg_3yr))
        if (nrow(proxy_data) > 0) {
          politicization_score_for_event <- mean(proxy_data$mentions_roll_avg_3yr, na.rm = TRUE)
          cat("Proxy calculated:", politicization_score_for_event, "\n")
        } else {
          cat("Warning: No data available to calculate proxy for", current_network_lc, "in year", current_event_year, ". Score remains NA.\n")
          politicization_score_for_event <- NA_real_
        }
      } else {
        cat("Own rolling average is NA for recent event:", current_network_lc, "in year", current_event_year, ". Score remains NA.\n")
        politicization_score_for_event <- NA_real_
      }
    }
    
    network_event_scores <- bind_rows(
      network_event_scores,
      tibble(
        network_lc = current_network_lc,
        event_year = current_event_year,
        event_type = current_event_type,
        raw_politicization_score = politicization_score_for_event
      )
    )
  }
  cat("Finished processing network event scores.\n")
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

# --- IV. CORE DATA PROCESSING ---

# 1. Process International Democratic Governance Data
intl_gov_results <- process_governance_data(raw_mydata_intl, "International Regulators")
mydata4 <- intl_gov_results$data_dem_gov_categorized
mydata3_long <- intl_gov_results$data_long_scaled_with_dimensions
mydata3.1_long <- intl_gov_results$data_avg_dim_scores

# 2. Process Combined (All TGN) Democratic Governance Data
raw_all_TGN <- bind_rows(raw_eur_TGN, raw_mydata_intl)
cat("\nDimensions of raw_all_TGN (combined data): ", paste(dim(raw_all_TGN)), "\n")

all_gov_results <- process_governance_data(raw_all_TGN, "All Financial Regulators", is_combined_data = TRUE)
dataTGN3_calculated <- all_gov_results$data_dem_gov_categorized
dataTGN2_long_calculated <- all_gov_results$data_long_scaled_with_dimensions
dataTGN2.1_long_calculated <- all_gov_results$data_avg_dim_scores_by_region_and_dim
dataTGN4_calculated <- all_gov_results$data_proportions_by_region_score
dataTGN2.2_long_calculated <- dataTGN2.1_long_calculated %>%
  group_by(region,dimension) %>%
  summarise(dim_value2= round(mean(dim_value, na.rm = TRUE), digits =2), .groups = 'drop')

# 3. Process V-Dem Data to get Network Averages
network_vdem_scores <- process_vdem_and_network_scores(vdem_membership_data_df, raw_vdem_data)

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

# Create Global and European subsets for df_appendix
df_appendix_global <- filter(df_appendix, region == "Global")
df_appendix_eur <- filter(df_appendix, region == "European")

# Calculate combined_pd_score in df_appendix
df_appendix <- df_appendix %>% mutate(
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)
df_appendix_global <- df_appendix_global %>% mutate(
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)
df_appendix_eur <- df_appendix_eur %>% mutate(
  combined_pd_score = ifelse(is.na(pool01) & is.na(del01), NA_real_, rowSums(select(., pool01, del01), na.rm = TRUE))
)

# Prepare df_for_new_scatterplot by joining df_appendix with membership category
df_organization_metadata_renamed <- df_organization_metadata %>%
  rename(network = Acronym)

df_for_new_scatterplot <- df_appendix %>%
  left_join(df_organization_metadata_renamed, by = "network")

# Add 'central_bank_prop' column from df_granular.csv
df_granular_data <- tryCatch({
  # CORRECTED: Read df_granular.csv directly from the working directory
  # as setwd for macOS is now directly to 'R Data'.
  read.csv("df_granular.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  cat("Error reading df_granular.csv: ", e$message, "\n")
  cat("Please ensure 'df_granular.csv' is in your R working directory: ", getwd(), "\n")
  return(data.frame(network = character(), Proportion_Central_Bankers = numeric(), stringsAsFactors = FALSE))
})

df_granular_props <- df_granular_data %>%
  select(network, central_bank_prop_granular = Proportion_Central_Bankers)

df_for_new_scatterplot <- df_for_new_scatterplot %>%
  # Remove any pre-existing 'central_bank_prop' to ensure clean join
  select(-any_of("central_bank_prop")) %>% 
  left_join(df_granular_props, by = "network") %>%
  # Rename the joined column to 'central_bank_prop'
  rename(central_bank_prop = central_bank_prop_granular)


cat("Updated 'central_bank_prop' in df_for_new_scatterplot using values from 'df_granular.csv'.\n")
cat("\nCreated df_for_new_scatterplot by joining df_appendix with membership category.\n")

# 6. Create specific network datasets from raw_mydata_intl
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

# --- Define a new, consistent, colorblind-safe dark pastel palette for ALL plots ---
# These colors are chosen to be distinct and suitable for colorblind individuals.
# Adjust these HEX codes if you have a very specific aesthetic preference.
cb_dark_pastel_palette <- c(
  "#4477AA", # Medium Blue - used for "Global", "Central Bank" parts
  "#EE6677", # Coral Red - used for "European", "Both"
  "#228833", # Dark Green - for "high" scores, "output" dimension
  "#CCBB44", # Muted Yellow/Gold - for "medium" scores, "throughput" dimension
  "#AA3377", # Dark Pink/Magenta - for "low" scores, "input" dimension
  "#66CCEE", # Light Blue/Cyan - for "Initial" event type
  "#BBBBBB"  # Light Grey - for "Other" part in stacked bars, NA values
)

# Map specific plot elements to this new palette
# Democratic Governance Score colors (low, medium, high)
score_colors_demgov_cb <- c(
  "low" = cb_dark_pastel_palette[5], # Dark Pink/Magenta
  "medium" = cb_dark_pastel_palette[4], # Muted Yellow/Gold
  "high" = cb_dark_pastel_palette[3]  # Dark Green
)

# Appendix Delegation/Pooling Score colors (low, medium, high, NA)
score_colors_appendix_cb <- c(
  "low" = cb_dark_pastel_palette[5],
  "medium" = cb_dark_pastel_palette[4],
  "high" = cb_dark_pastel_palette[3],
  "NA" = cb_dark_pastel_palette[7] # Light Grey
)

# Region colors (Global, European)
region_colors_cb <- c(
  "Global" = cb_dark_pastel_palette[1], # Medium Blue
  "European" = cb_dark_pastel_palette[2] # Coral Red
)

# MembershipCategory colors (Regulator, Central bank, Both)
# Note: User requested to minimize reliance on MembershipCategory for coloring where central_bank_prop is primary.
# However, these may still be used if explicitly mapped in plots, e.g., for scatterplot grouping.
# For consistency, these will now use the new palette.
membership_colors_cb <- c(
  "Regulator" = cb_dark_pastel_palette[1],    # Medium Blue (similar to Global)
  "Central bank" = cb_dark_pastel_palette[3], # Dark Green (similar to high)
  "Both" = cb_dark_pastel_palette[4]          # Muted Yellow/Gold (similar to medium)
)

# Dimension colors (input, throughput, output)
dimension_colors_cb <- c(
  "input" = cb_dark_pastel_palette[5],    # Dark Pink/Magenta
  "throughput" = cb_dark_pastel_palette[4], # Muted Yellow/Gold
  "output" = cb_dark_pastel_palette[3]    # Dark Green
)

# Event Type colors (Initial, Recent)
event_type_colors_cb <- c(
  "Initial" = cb_dark_pastel_palette[6], # Light Blue/Cyan
  "Recent" = cb_dark_pastel_palette[1]   # Medium Blue
)

# Colors for Central Bank Proportion stacked bars (Central Bank, Other)
# 'Central Bank' part of the bar will be primary color, 'Other' will be neutral grey.
cb_stacked_colors_cb <- c(
  "Central Bank" = cb_dark_pastel_palette[1], # Medium Blue
  "Other" = cb_dark_pastel_palette[7]         # Light Grey
)


custom_shapes <- c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14, 18, 19, 20, 21, 22, 23, 24, 25) # For scatterplot shapes

# A. Plots Related to Democratic Governance Scores
if (exists("mydata4") && nrow(mydata4) > 0) {
  plot_global_fr_bar <- ggplot(mydata4, aes(x= reorder(network, -dem_gov), y= dem_gov) )+
    geom_col(width = 0.5, fill = region_colors_cb["Global"]) + # Apply new specific color
    xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Global Financial Regulators (International Sample)")+
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_economist() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave("plot_global_fr_bar.svg", plot = plot_global_fr_bar, width = academic_wide_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_global_fr_bar.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(filter(dataTGN3_calculated, region == "European")) > 0) {
  plot_european_fr_bar <- ggplot(dataTGN3_calculated %>% filter(region == "European"), aes(x= reorder(network, -dem_gov), y= dem_gov) )+
    geom_col(width = 0.5, fill = region_colors_cb["European"]) + # Apply new specific color
    xlab("") + ylab("Democratic Governance Score") +
    labs(title = "European Financial Regulators")+
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_economist() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("plot_european_fr_bar.svg", plot = plot_european_fr_bar, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_european_fr_bar.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_bar <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill=region))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") + labs(title = "Financial Regulators")+
    geom_text(aes(label= dem_gov, vjust= -0.2))+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          legend.position = c(0.8, 0.8), legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y= element_text(margin = margin(r=10))) +
    scale_fill_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_all_fr_bar.svg", plot = plot_all_fr_bar, width = academic_very_wide_width, height = academic_single_height * 1.5, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_bar.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_faceted_region <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill= region))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Financial Regulators by Region")+
    geom_text(aes(label= dem_gov, vjust= -0.2), size=3)+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size=8), strip.text = element_text(face="bold"),
          axis.title.y= element_text(margin = margin(r=10)))+
    scale_fill_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_all_fr_faceted_region.svg", plot = plot_all_fr_faceted_region, width = academic_wide_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_faceted_region.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_all_fr_faceted_score <- ggplot(dataTGN3_calculated, aes(x= reorder(network, -dem_gov), y= dem_gov, fill= score))+
    geom_col(width = 0.5) + xlab("") + ylab("Democratic Governance Score") +
    labs(title = "Financial Regulators by Region and Score")+
    geom_text(aes(label= dem_gov, vjust= -0.2), size=3)+ ylim(0,1)+ theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=8), strip.text = element_text(face="bold"),
          axis.title.y= element_text(margin = margin(r=10)))+
    scale_fill_manual(name= "Democratic \nGovernance Score", values = score_colors_demgov_cb,
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    facet_wrap(~region, scales = "free_x")
  ggsave("plot_all_fr_faceted_score.svg", plot = plot_all_fr_faceted_score, width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
  cat("Saved plot_all_fr_faceted_score.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_freq_all <- ggplot(dataTGN3_calculated, aes(x= fct_relevel(score, "low", "medium", "high"), fill =score))+
    geom_bar(width= 0.5, aes(y=after_stat(count)/sum(after_stat(count)))) +
    labs(title = "Financial Regulators: Governance Score Distribution")+
    xlab("Democratic Governance Score Category") + ylab("Percent") + theme_minimal()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_fill_manual(name = "Dem. \nGovernance", values = score_colors_demgov_cb, # Apply new score colors
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text.x = element_text(size=10), legend.position = "right",
          axis.title.y= element_text(margin = margin(r=10)))
  ggsave("plot_freq_all.svg", plot = plot_freq_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_freq_all.svg\n")
}

if (exists("dataTGN4_calculated") && nrow(dataTGN4_calculated) > 0) {
  region.name.levels <- levels(dataTGN4_calculated$region)
  if(is.null(region.name.levels)) region.name.levels <- unique(dataTGN4_calculated$region)
  
  plot_stack_economist <- ggplot(dataTGN4_calculated, aes(x = factor(region, levels=region.name.levels), y = prop, fill = score))+
    geom_col(position = "fill") + coord_flip() +
    geom_text(aes(label = scales::percent(prop, accuracy=1)), position = position_fill(vjust = 0.5), color = "white", size=3.5, na.rm=TRUE) +
    ggtitle("Financial Regulators", subtitle = "Democratic Governance Score Proportions by Region")+
    xlab("") + ylab("Proportion") + theme_economist()+
    scale_x_discrete(labels = region.name.levels) +
    scale_fill_manual(name = "Score", values = score_colors_demgov_cb, # Apply new score colors
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          plot.subtitle = element_text(size = 10, hjust = 0), legend.title = element_text(size=10))
  ggsave("plot_stack_economist.svg", plot = plot_stack_economist, width = academic_single_width, height = academic_single_height * 0.8, dpi = academic_plot_dpi)
  cat("Saved plot_stack_economist.svg\n")
  
  plot_stack_minimal <- ggplot(dataTGN4_calculated, aes(x = factor(region, levels=region.name.levels), y = prop, fill = score))+
    geom_col(position = "fill") + coord_flip() +
    geom_text(aes(label = scales::percent(prop, accuracy=1)), position = position_fill(vjust = 0.5), color = "white", size=3.5, na.rm=TRUE) +
    ggtitle("Financial Regulators", subtitle = "Democratic Governance Score Proportions by Region")+
    xlab("") + ylab("Proportion") + theme_minimal()+
    scale_x_discrete(labels = region.name.levels) +
    scale_fill_manual(name = "Score", values = score_colors_demgov_cb, # Apply new score colors
                      labels = c("low" = "Low", "medium" = "Medium", "high" = "High"))+
    theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5), axis.line.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          plot.subtitle = element_text(size = 10, hjust = 0.5), legend.title = element_text(size=10))
  ggsave("plot_stack_minimal.svg", plot = plot_stack_minimal, width = academic_single_width, height = academic_single_height * 0.8, dpi = academic_plot_dpi)
  cat("Saved plot_stack_minimal.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_box_all <- ggplot(dataTGN3_calculated, aes(x= region, y= dem_gov, fill=region)) +
    geom_boxplot(na.rm=TRUE) + xlab("") + ylab("Democratic Governance Score") +
    ggtitle("Financial Regulators: Governance Score by Region")+ theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.line.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size=10), legend.position = "none") +
    scale_fill_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_box_all.svg", plot = plot_box_all, width = academic_single_width * 0.8, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_box_all.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_dot_all <- ggplot(dataTGN3_calculated, aes(x= region, y= dem_gov, color = region)) +
    geom_jitter(width = 0.1, alpha = 0.7, na.rm=TRUE) +
    stat_summary(geom="point", fun = "mean", size= 3, shape = 24, fill=cb_dark_pastel_palette[3])+ # Changed fill to a palette color
    xlab("") + ylab("Democratic Governance Score") + ggtitle("Financial Regulators: Governance Scores")+ theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.position = "none") +
    scale_color_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_dot_all.svg", plot = plot_dot_all, width = academic_single_width * 0.8, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_dot_all.svg\n")
}

if (exists("dataTGN3_calculated") && nrow(dataTGN3_calculated) > 0) {
  plot_density_all <- ggplot(dataTGN3_calculated, aes(x= dem_gov, fill=region))+
    geom_density(alpha=.4, na.rm=TRUE) + xlim(0,1)+ xlab("Democratic Governance Score") +
    ggtitle("Density of Governance Scores by Region")+ ylab("Density") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.title = element_blank(), legend.position = c(0.85, 0.85)) +
    scale_fill_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_density_all.svg", plot = plot_density_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_density_all.svg\n")
  
  plot_hist_all <- ggplot(dataTGN3_calculated, aes(x= dem_gov, fill=region))+
    geom_histogram(alpha=.5, position="identity", binwidth = 0.1, na.rm=TRUE) + xlim(0,1)+
    xlab("Democratic Governance Score") + ggtitle("Histogram of Governance Scores by Region")+ ylab("Count") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.title = element_blank(), legend.position = c(0.85, 0.85)) +
    scale_fill_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_hist_all.svg", plot = plot_hist_all, width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
  cat("Saved plot_hist_all.svg\n")
}

# Lollipop plots for indicator scores
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
          axis.text.y = element_text(size=7), legend.position = "bottom") +
    scale_color_manual(values = dimension_colors_cb) # Apply new dimension colors
  ggsave("plot_loli_global.svg", plot = plot_loli_global, width = academic_wide_width, height = loli_global_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_global.svg\n")
}

if (exists("dataTGN2_long_calculated") && nrow(filter(dataTGN2_long_calculated, region == "European")) > 0) {
  # CORRECTED: Changed source data to dataTGN2_long_calculated for indicator plots
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
          axis.text.y = element_text(size=7), legend.position = "bottom") +
    scale_color_manual(values = dimension_colors_cb) # Apply new dimension colors
  ggsave("plot_loli_eur.svg", plot = plot_loli_eur, width = academic_wide_width *0.8, height = loli_eur_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_eur.svg\n")
}

# Lollipop plots for dimension scores
if (exists("mydata3.1_long") && nrow(mydata3.1_long) > 0) {
  num_networks_global_dims_loli <- length(unique(mydata3.1_long$network))
  loli_global_dims_height <- academic_single_height * (1 + num_networks_global_dims_loli %/% 3) * 0.6
  
  plot_loli_global_dims <- ggplot(mydata3.1_long, aes(x= factor(dimension, levels=items.names.1), y= dim_value, color = dimension)) +
    geom_point(size=2, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value), na.rm=TRUE) +
    facet_wrap(~network, ncol=3) + xlab("") + ylab("Dimension Value") +
    scale_x_discrete(labels = items.names.1) + labs(title="Global Financial Regulators: Dimension Scores (Int'l Sample)") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8), legend.position = "none") +
    scale_color_manual(values = dimension_colors_cb) # Apply new dimension colors
  ggsave("plot_loli_global_dims.svg", plot = plot_loli_global_dims, width = academic_wide_width, height = loli_global_dims_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_global_dims.svg\n")
}

if (exists("dataTGN2.1_long_calculated") && nrow(filter(dataTGN2.1_long_calculated, region == "European")) > 0) {
  eur_data_dims <- dataTGN2.1_long_calculated %>% filter(region == "European")
  num_networks_eur_dims_loli <- length(unique(eur_data_dims$network))
  loli_eur_dims_height <- academic_single_height * (1 + num_networks_eur_dims_loli %/% 2) * 0.6
  
  plot_loli_eur_dims <- ggplot(eur_data_dims, aes(x= factor(dimension, levels=items.names.1), y= dim_value, color = dimension)) +
    geom_point(size=2, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value), na.rm=TRUE) +
    facet_wrap(~network, ncol=2) + xlab("") + ylab("Dimension Value") +
    scale_x_discrete(labels = items.names.1) + labs(title="European Financial Regulators: Dimension Scores") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold", size=8), legend.position = "none") +
    scale_color_manual(values = dimension_colors_cb) # Apply new dimension colors
  ggsave("plot_loli_eur_dims.svg", plot = plot_loli_eur_dims, width = academic_wide_width * 0.8, height = loli_eur_dims_height, dpi = academic_plot_dpi)
  cat("Saved plot_loli_eur_dims.svg\n")
}

if(exists("dataTGN2.2_long_calculated") && nrow(dataTGN2.2_long_calculated) > 0) {
  j_plot <- ggplot(dataTGN2.2_long_calculated, aes(x= fct_relevel(dimension, "output", "throughput", "input"), y= dim_value2, color = dimension)) +
    geom_point(size=2.5, na.rm=TRUE) + coord_flip()+
    geom_segment( aes(x= dimension, xend= dimension, y=0, yend= dim_value2), na.rm=TRUE) +
    facet_wrap(~region) + xlab("") + ylab("Average Dimension Score") +
    labs(color = "Dimension", title="Average Dimension Scores by Region") + theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), strip.text = element_text(face="bold"), legend.position = "none") +
    scale_color_manual(values = dimension_colors_cb) # Apply new dimension colors
  ggsave(file="LolliPlot_All_by_region_dimensions.svg", plot=j_plot, width=academic_wide_width, height=academic_single_height * 0.9, dpi = academic_plot_dpi)
  cat("Saved LolliPlot_All_by_by_region_dimensions.svg\n")
}

if (exists("dataTGN2.1_long_calculated") && nrow(dataTGN2.1_long_calculated) > 0) {
  dataTGN2.1_long_temp_calc <- dataTGN2.1_long_calculated %>%
    mutate(region = factor(region, levels = c("Global", "European"))) 
  
  factor_order_calc <- dataTGN2.1_long_temp_calc %>%
    filter(!is.na(network) & !is.na(region)) %>%
    distinct(region, network) %>% arrange(region, network) %>% pull(network)
  
  i_plot_ncols_calc <- 4
  i_plot_nrows_calc <- ceiling(length(factor_order_calc) / i_plot_ncols_calc)
  i_plot_height_calc <- academic_single_height * 0.6 * i_plot_nrows_calc
  
  i_plot <- ggplot(dataTGN2.1_long_temp_calc %>% filter(!is.na(network) & !is.na(region)), 
                   aes(x = fct_relevel(dimension, "output", "throughput", "input"), y = dim_value)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = region), alpha = 0.1, inherit.aes = FALSE) +
    geom_point(aes(color = dimension), size=2, na.rm=TRUE) + coord_flip()+ ylim(0, 1) +
    geom_segment(aes(x = dimension, xend= dimension, y=0, yend= dim_value, color = dimension), na.rm=TRUE) +
    facet_wrap(~factor(network, levels = factor_order_calc), ncol = i_plot_ncols_calc, dir = "h") +
    xlab("") + ylab("Score") + labs(title="Dimension Scores by Network and Region") + theme_linedraw() +
    scale_fill_manual(name = "Region", values = region_colors_cb, labels = c("Global" = "Global", "European" = "European"), na.value="grey") + # Apply new region colors
    scale_color_manual(name = "Dem. Gov. \nDimension", values = dimension_colors_cb, na.value="grey") + # Apply new dimension colors
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines"), strip.text.x = element_text(size = 8, face = "bold"),
          axis.text = element_text(size = 8), axis.text.y = element_text(size=7),
          legend.text = element_text(size = 8), legend.title = element_text(size = 9),
          legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.25), order=1), color = guide_legend(order=2))
  ggsave(file="LolliPlot_All_by_type_and_dimensions.svg", plot=i_plot, width = academic_very_wide_width, height = i_plot_height_calc, dpi = academic_plot_dpi, limitsize = FALSE)
  cat("Saved LolliPlot_All_by_type_and_dimensions.svg\n")
}

temp_all_gov_aggregated <- aggregate_indicators(raw_all_TGN)
temp_all_gov_scaled_wide <- scale_indicators(temp_all_gov_aggregated)

if (all(paste0(c("in_part", "in_rep", "in_shb", "in_epi", "through_tr", "through_leg", "through_acc", "through_reas", "out_effec", "out_effic"), "01") %in% names(temp_all_gov_scaled_wide))) {
  dataTGN_for_corr <- temp_all_gov_scaled_wide %>%
    select(ends_with("01"))
  
  dataTGN_for_corr <- dataTGN_for_corr %>% mutate(across(everything(), as.numeric))
  
  mydata.cor <- cor(dataTGN_for_corr, use="pairwise.complete.obs")
  
  svglite("correlation_plot_indicators.svg", width = 7, height = 7)
  corrplot(mydata.cor, type="upper", order="hclust", tl.col = "black", tl.srt = 45,
           title = "Correlation Matrix of Governance Indicators", mar=c(0,0,1,0))
  dev.off()
  cat("Saved correlation_plot_indicators.svg\n")
} else {
  warning("Not all '01' indicator columns found for correlation plot.")
}

# B. Appendix 4 Plots
if (nrow(df_appendix) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix))) {
  num_networks_all_app4 <- length(unique(df_appendix$network[!is.na(df_appendix$network)]))
  available_shapes <- rep(custom_shapes, length.out = num_networks_all_app4)
  
  plot_scatter_del_pool_all <- ggplot(df_appendix, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm = TRUE) +
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov_cb, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) + # Apply new score colors
    scale_shape_manual(name = "Network", values = available_shapes, drop = FALSE, na.value = 4) +
    geom_text_repel(color= "black", size = 3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + labs(title = "Delegation vs. Pooling by Governance Score") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"), legend.box = "horizontal")
  ggsave(file="ScatterPlot_All_Appendix.svg", plot=plot_scatter_del_pool_all, width=academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_All_Appendix.svg\n")
}

if (nrow(df_appendix_eur) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix_eur))) {
  num_networks_eur_app4 <- length(unique(df_appendix_eur$network[!is.na(df_appendix_eur$network)]))
  available_shapes_eur <- rep(custom_shapes, length.out = num_networks_eur_app4)
  
  plot_scatter_del_pool_eur <- ggplot(df_appendix_eur, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm = TRUE) +
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov_cb, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop=FALSE) + # Apply new score colors
    scale_shape_manual(name = "Network", values = available_shapes_eur, drop = FALSE, na.value = 4) +
    geom_text_repel(color= "black", size = 3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + ggtitle("European Regulators: Delegation vs. Pooling") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=12, face="bold"), legend.box = "horizontal")
  ggsave(file="ScatterPlot_Eur_Appendix.svg", plot=plot_scatter_del_pool_eur, width = academic_single_width, height = academic_single_height * 1.1, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_Eur_Appendix.svg\n")
}

if (nrow(df_appendix_global) > 0 && all(c("pool01", "del01", "score", "network") %in% names(df_appendix_global))) {
  num_networks_global_app4 <- length(unique(df_appendix_global$network[!is.na(df_appendix_global$network)]))
  available_shapes_global <- rep(custom_shapes, length.out = num_networks_global_app4)
  
  plot_scatter_del_pool_global <- ggplot(df_appendix_global, aes(x = pool01 , y = del01, color = score, label = network)) +
    geom_point(aes(shape = network), size = 3, position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange=TRUE, linetype="solid", color="black", na.rm = TRUE) +
    scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov_cb, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) + # Apply new score colors
    scale_shape_manual(name = "Network", values = available_shapes_global, drop = FALSE, na.value = 4) +
    geom_text_repel(color= "black", size=3, max.overlaps = Inf, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), segment.color = 'grey50', min.segment.length = unit(0, "lines"), position = position_jitter(width = 0.002, height = 0.0003, seed = 123), na.rm=TRUE) +
    xlab("Pooling (Scaled)") + ylab("Delegation (Scaled)") + ggtitle("Global Regulators: Delegation vs. Pooling") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=12, face="bold"), legend.box = "horizontal")
  ggsave(file="ScatterPlot_Global_Appendix.svg", plot=plot_scatter_del_pool_global, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_Global_Appendix.svg\n")
}

if (nrow(df_appendix) > 0 && all(c("network", "del01", "score", "region") %in% names(df_appendix))) {
  plot_bar_delegation_appendix <- ggplot(df_appendix, aes(x= fct_reorder(network, -del01), y= del01, fill= score))+
    geom_col(width = 0.5, na.rm=TRUE) + xlab("") + ylab("Delegation (Scaled)") +
    labs(title = "Financial Regulators: Delegation by Governance Score")+
    geom_text(aes(label= del01, vjust= -0.2), size=2.5, na.rm=TRUE)+
    scale_y_continuous(limits = c(0, max(df_appendix$del01, na.rm=TRUE) * 1.15), expand = expansion(mult = c(0, 0.05))) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=8),
          axis.title.y= element_text(margin = margin(r=10)),
          strip.text = element_text(face="bold"))+
    scale_fill_manual(name= "Democratic \nGovernance Score", values=score_colors_appendix_cb,
                      labels = c("high"="High", "medium"="Medium", "low"="Low", "NA"="Not Scored"),
                      na.value = "grey", drop = FALSE) + # Apply new score colors
    facet_wrap(~region, scales = "free_x")
  ggsave(file="BarPlot_Auth_All_Appendix.svg", plot=plot_bar_delegation_appendix, width=academic_wide_width, height=academic_single_height*1.3, dpi = academic_plot_dpi)
  cat("Saved BarPlot_Auth_All_Appendix.svg\n")
}

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
    scale_fill_manual(name= "Democratic \nGovernance Score", values=score_colors_appendix_cb,
                      labels = c("high"="High", "medium"="Medium", "low"="Low", "NA"="Not Scored"),
                      na.value = "grey", drop = FALSE) + # Apply new score colors
    facet_wrap(~region, scales = "free_x")
  ggsave(file="BarPlot_Pool_All_Appendix.svg", plot=plot_bar_pooling_appendix, width=academic_wide_width, height=academic_single_height*1.3, dpi = academic_plot_dpi)
  cat("Saved BarPlot_Pool_All_Appendix.svg\n")
}

if (nrow(df_appendix) > 0 && all(c("region", "del01", "network") %in% names(df_appendix))) {
  plot_dot_delegation_appendix <- ggplot(df_appendix, aes(x= region, y= del01, color = region)) +
    geom_jitter(width=0.1, alpha=0.6, na.rm=TRUE)+
    geom_text_repel(aes(label = network), size=3, max.overlaps = 15, segment.alpha = 0.5, na.rm = TRUE) +
    stat_summary(geom="point", fun = "mean", size= 4, shape = 24, fill=region_colors_cb["Global"])+ # Changed fill to a palette color (e.g., Global's color)
    xlab("") + ylab("Delegation (Scaled)") + ggtitle("Financial Regulators: Delegation Scores by Region")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none") +
    scale_color_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_dot_delegation_appendix.svg", plot = plot_dot_delegation_appendix, width = academic_single_width, height = academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved DotPlot_Auth_All_Appendix.svg\n")
}

if (nrow(df_appendix) > 0 && all(c("region", "pool01", "network") %in% names(df_appendix))) {
  plot_dot_pooling_appendix <- ggplot(df_appendix, aes(x= region, y= pool01, color = region)) +
    geom_jitter(width=0.1, alpha=0.6, na.rm=TRUE)+
    geom_text_repel(aes(label = network), size=3, max.overlaps = 15, segment.alpha = 0.5, na.rm = TRUE) +
    stat_summary(geom="point", fun = "mean", size= 4, shape = 24, fill=region_colors_cb["Global"])+ # Changed fill to a palette color
    xlab("") + ylab("Pooling (Scaled)") + ggtitle("Financial Regulators: Pooling Scores by Region")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none") +
    scale_color_manual(values = region_colors_cb) # Apply new region colors
  ggsave("plot_dot_pooling_appendix.svg", plot = plot_dot_pooling_appendix, width = academic_single_width, height = academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved DotPlot_Pool_All_Appendix.svg\n")
}

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
    scale_fill_manual(name= "Democratic \nGovernance Score", values = score_colors_demgov_cb,
                      labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop=FALSE) + # Apply new score colors
    xlab("Democratic Governance Score") + ylab("Combined Pooling & Delegation Score (Scaled)") +
    ggtitle("Combined P&D Score by Governance Score (All Regulators)") + theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"), legend.box = "horizontal")
  ggsave(file="BoxPlot_All_Combined_PD_Outliers.svg", plot=plot_box_combined_pd_all, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
  cat("Saved BoxPlot_All_Combined_PD_Outliers.svg\n")
}

# C. V-Dem Integration Plots
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
    scale_color_manual(values = score_colors_demgov_cb, drop = FALSE, na.value="grey") + # Apply new score colors
    scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1))
  ggsave(file = "ScatterPlot_VDEM_vs_DemGov_Calculated.svg", plot = plot_scatter_vdem_vs_demgov, width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
  cat("Saved ScatterPlot_VDEM_vs_DemGov_Calculated.svg\n")
}

if(exists("all_gov_results") && "data_avg_dim_scores_by_region_and_dim" %in% names(all_gov_results) && exists("network_vdem_scores")) {
  data_vdem_vs_dimensions <- all_gov_results$data_avg_dim_scores_by_region_and_dim %>%
    left_join(network_vdem_scores, by = "network")
  
  if (nrow(data_vdem_vs_dimensions) > 0 && all(c("network_vdem_score", "dim_value", "dimension", "network", "region") %in% names(data_vdem_vs_dimensions))) {
    plot_scatter_vdem_vs_dimensions_calc <- ggplot(data_vdem_vs_dimensions, aes(x = network_vdem_score, y = dim_value, label = network)) +
      geom_point(aes(color = dimension, shape = region), size = 3, alpha = 0.7, na.rm=TRUE) +
      geom_smooth(aes(color = dimension, group = dimension), method = "lm", se = FALSE, linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(size = 2.5, color = "black", max.overlaps = 7, segment.color = 'grey80', na.rm = TRUE,
                      data = . %>% distinct(network, dimension, .keep_all = TRUE)) +
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
      scale_color_manual(values = dimension_colors_cb, na.value="grey") + # Apply new dimension colors
      scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
      facet_wrap(~dimension, ncol = 3)
    ggsave(file = "ScatterPlot_VDEM_vs_Dimensions_Calculated.svg", plot = plot_scatter_vdem_vs_dimensions_calc, width = academic_very_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_VDEM_vs_Dimensions_Calculated.svg\n")
  } else {
    cat("Skipping ScatterPlot_VDEM_vs_Dimensions_Calculated.svg due to missing data or columns after merge.\n")
  }
} else {
  cat("Skipping ScatterPlot_VDEM_vs_Dimensions_Calculated.svg because prerequisite dataframes are missing.\n")
}

if (nrow(df_appendix) > 0 && "network_vdem_score" %in% names(df_appendix)) {
  if("del01" %in% names(df_appendix)) {
    cor_del_vdem <- cor.test(df_appendix$network_vdem_score, df_appendix$del01, use = "pairwise.complete.obs")
    cat("\nCorrelation between Network V-Dem Score and Delegation (Scaled):\n")
    print(cor_del_vdem)
    
    plot_vdem_del_all <- ggplot(df_appendix, aes(x = network_vdem_score, y = del01, color = score, label = network)) +
      geom_point(aes(shape = network), size = 3, na.rm=TRUE) +
      geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange = TRUE, linetype = "solid", color = "black", na.rm = TRUE) +
      scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov_cb, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) + # Apply new score colors
      scale_shape_manual(name = "Network", values = rep(custom_shapes, length.out = length(unique(df_appendix$network[!is.na(df_appendix$network)]))), drop=FALSE, na.value=4) +
      geom_text_repel(color= "black", size = 3, max.overlaps = Inf, segment.color = 'grey50', na.rm=TRUE) +
      xlab("Network Average V-Dem Polyarchy Score") + ylab("Delegation (Scaled)") +
      labs(title = "Network V-Dem Score vs. Delegation (All Regulators)") + theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"))
    ggsave(file="ScatterPlot_Vdem_Del_All.svg", plot=plot_vdem_del_all, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_Vdem_Del_All.svg\n")
  }
  
  if("pool01" %in% names(df_appendix)) {
    cor_pool_vdem <- cor.test(df_appendix$network_vdem_score, df_appendix$pool01, use = "pairwise.complete.obs")
    cat("\nCorrelation between Network V-Dem Score and Pooling (Scaled):\n")
    print(cor_pool_vdem)
    
    plot_vdem_pool_all <- ggplot(df_appendix, aes(x = network_vdem_score, y = pool01, color = score, label = network)) +
      geom_point(aes(shape = network), size = 3, na.rm=TRUE) +
      geom_smooth(aes(group=1, color=NULL), method = "lm", se = FALSE, fullrange = TRUE, linetype = "solid", color = "black", na.rm = TRUE) +
      scale_color_manual(name= "Democratic \nGovernance Score", values=score_colors_demgov_cb, labels=c("high"="High", "medium"="Medium", "low"="Low"), na.value = "grey50", drop = FALSE) + # Apply new score colors
      scale_shape_manual(name = "Network", values = rep(custom_shapes, length.out = length(unique(df_appendix$network[!is.na(df_appendix$network)]))), drop=FALSE, na.value=4) +
      geom_text_repel(color= "black", size = 3, max.overlaps = Inf, segment.color = 'grey50', na.rm=TRUE) +
      xlab("Network Average V-Dem Polyarchy Score") + ylab("Pooling (Scaled)") +
      labs(title = "Network V-Dem Score vs. Pooling (All Regulators)") + theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, size=14, face="bold"))
    ggsave(file="ScatterPlot_Vdem_Pool_All.svg", plot=plot_vdem_pool_all, width=academic_single_width, height=academic_single_height*1.1, dpi = academic_plot_dpi)
    cat("Saved ScatterPlot_Vdem_Pool_All.svg\n")
  }
}

# D. Scatterplot: Democratic Governance vs. Authority by Membership Type
# Note: MembershipCategory is retained as a color aesthetic here, but uses the new colorblind-safe palette.
if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  df_plot_ready <- df_for_new_scatterplot %>%
    mutate(
      dem_gov = as.numeric(dem_gov),
      combined_pd_score = as.numeric(combined_pd_score)
    ) %>%
    filter(!is.na(dem_gov) & !is.na(combined_pd_score) & !is.na(MembershipCategory))
  
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
      scale_color_manual(values = membership_colors_cb) + # Apply new membership colors
      guides(color = guide_legend(override.aes = list(size=4))) 
    ggsave("plot_demgov_authority_memtype.svg", plot = plot_demgov_authority_memtype, width = academic_wide_width, height = academic_single_height * 1.4, dpi = academic_plot_dpi) 
    cat("Saved plot_demgov_authority_memtype.svg\n")
  } else {
    cat("Skipping new scatterplot: No data available after filtering NAs for critical columns.\n")
  }
} else {
  cat("Skipping new scatterplot: df_for_new_scatterplot is not available or empty.\n")
}

# E. Plots by Membership Category
cat("\n--- Starting Plots by Membership Category ---\n")

if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  df_membership_analysis <- df_for_new_scatterplot %>%
    mutate(
      dem_gov = as.numeric(dem_gov),
      combined_pd_score = as.numeric(combined_pd_score),
      network_vdem_score = as.numeric(network_vdem_score),
      MembershipCategory = as.factor(MembershipCategory)
    )
  
  # Note: membership_colors_cb defined globally will be used here.
  
  plot_data_e1 <- df_membership_analysis %>%
    filter(!is.na(dem_gov) & !is.na(combined_pd_score) & !is.na(MembershipCategory))
  
  if(nrow(plot_data_e1) > 0) {
    plot_demgov_vs_auth_by_memtype_facet <- ggplot(plot_data_e1, aes(x = dem_gov, y = combined_pd_score)) +
      geom_point(aes(color = MembershipCategory), size = 2.5, alpha = 0.8, na.rm = TRUE) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(aes(label = network), size = 2.5, max.overlaps = 10, na.rm = TRUE,
                      segment.color = 'grey70', segment.alpha = 0.5) +
      facet_wrap(~MembershipCategory, scales = "fixed") +
      labs(title = "Democratic Governance vs. Combined Authority",
           subtitle = "Faceted by Membership Type (Trend line for each facet)",
           x = "Democratic Governance Score",
           y = "Combined Pooling & Delegation Score") +
      scale_color_manual(values = membership_colors_cb, name = "Membership Type") + # Apply new membership colors
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            strip.text = element_text(face = "bold"))
    ggsave("plot_demgov_vs_auth_by_memtype_facet.svg", plot = plot_demgov_vs_auth_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_demgov_vs_auth_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_demgov_vs_auth_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
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
      scale_color_manual(values = membership_colors_cb, name = "Membership Type") + # Apply new membership colors
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            strip.text = element_text(face = "bold")) +
      scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1))
    ggsave("plot_vdem_vs_demgov_by_memtype_facet.svg", plot = plot_vdem_vs_demgov_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_vdem_vs_demgov_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_vdem_vs_demgov_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
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
      scale_color_manual(values = membership_colors_cb, name = "Membership Type") + # Apply new membership colors
      theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            strip.text = element_text(face = "bold")) +
      scale_x_continuous(limits = c(0,1))
    ggsave("plot_vdem_vs_auth_by_memtype_facet.svg", plot = plot_vdem_vs_auth_by_memtype_facet, 
           width = academic_very_wide_width, height = academic_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_vdem_vs_auth_by_memtype_facet.svg\n")
  } else {
    cat("Skipping plot_vdem_vs_auth_by_memtype_facet: Not enough data after NA removal.\n")
  }
  
  plot_data_e4 <- df_membership_analysis %>% filter(!is.na(dem_gov) & !is.na(MembershipCategory))
  if(nrow(plot_data_e4) > 0) {
    plot_boxplot_demgov_by_memtype <- ggplot(plot_data_e4, aes(x = MembershipCategory, y = dem_gov, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Democratic Governance Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Democratic Governance Score") +
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave("plot_boxplot_demgov_by_memtype.svg", plot = plot_boxplot_demgov_by_memtype, 
           width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_demgov_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_demgov_by_memtype: Not enough data after NA removal.\n")
  }
  
  plot_data_e5 <- df_membership_analysis %>% filter(!is.na(combined_pd_score) & !is.na(MembershipCategory))
  if(nrow(plot_data_e5) > 0) {
    plot_boxplot_auth_by_memtype <- ggplot(plot_data_e5, aes(x = MembershipCategory, y = combined_pd_score, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Combined Authority Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Combined Pooling & Delegation Score") +
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave("plot_boxplot_auth_by_memtype.svg", plot = plot_boxplot_auth_by_memtype, 
           width = academic_wide_width, height = academic_single_height * 1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_auth_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_auth_by_memtype: Not enough data after NA removal.\n")
  }
  
  plot_data_e6 <- df_membership_analysis %>% filter(!is.na(network_vdem_score) & !is.na(MembershipCategory))
  if(nrow(plot_data_e6) > 0) {
    plot_boxplot_vdem_by_memtype <- ggplot(plot_data_e6, aes(x = MembershipCategory, y = network_vdem_score, fill = MembershipCategory)) +
      geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6, na.rm = TRUE, size=1.5) +
      labs(title = "Distribution of Network V-Dem Scores",
           subtitle = "by Membership Type",
           x = "Membership Type",
           y = "Network Average V-Dem Score") +
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1))
    ggsave("plot_boxplot_vdem_by_memtype.svg", plot = plot_boxplot_vdem_by_memtype, 
           width = academic_wide_width, height = academic_single_height*1.2, dpi = academic_plot_dpi)
    cat("Saved plot_boxplot_vdem_by_memtype.svg\n")
  } else {
    cat("Skipping plot_boxplot_vdem_by_memtype: Not enough data after NA removal.\n")
  }
  
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
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1))
    ggsave("plot_avg_demgov_by_memtype.svg", plot = plot_avg_demgov_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_demgov_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_demgov_by_memtype: Not enough data for averaging.\n")
  }
  
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
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave("plot_avg_auth_by_memtype.svg", plot = plot_avg_auth_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_auth_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_auth_by_memtype: Not enough data for averaging.\n")
  }
  
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
      scale_fill_manual(values = membership_colors_cb) + # Apply new membership colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,1))
    ggsave("plot_avg_vdem_by_memtype.svg", plot = plot_avg_vdem_by_memtype, 
           width = academic_single_width, height = academic_single_height, dpi = academic_plot_dpi)
    cat("Saved plot_avg_vdem_by_memtype.svg\n")
  } else {
    cat("Skipping plot_avg_vdem_by_memtype: Not enough data for averaging.\n")
  }
} else {
  cat("Skipping Plots by Membership Category: df_for_new_scatterplot is not available or empty.\n")
}

# F. Central Bank Proportion Plots
cat("\n--- Starting Central Bank Proportion Plots ---\n")

# Scatterplot: Democratic Governance vs. Central Bank Proportion
if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  plot_data_cb_scatter <- df_for_new_scatterplot %>%
    filter(!is.na(dem_gov) & !is.na(central_bank_prop) & !is.na(network) & !is.na(region))
  
  if (nrow(plot_data_cb_scatter) > 0) {
    plot_demgov_vs_central_bank_prop <- ggplot(plot_data_cb_scatter, 
                                               aes(x = central_bank_prop, y = dem_gov, label = network)) +
      geom_point(aes(color = region, shape = region), size = 3.5, alpha = 0.8, na.rm = TRUE) + # Color and shape by region
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", na.rm = TRUE) +
      geom_text_repel(size = 3, color = "black", max.overlaps = 10, segment.color = 'grey70', na.rm = TRUE,
                      box.padding = unit(0.35, "lines"),
                      point.padding = unit(0.2, "lines")) +
      labs(title = "Democratic Governance vs. Central Bank Proportion",
           subtitle = "Colored and Shaped by Region",
           x = "Central Bank Proportion (from df_granular.csv)",
           y = "Democratic Governance Score",
           color = "Region") + # Legend title for color
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.position = "bottom", legend.box = "horizontal") +
      scale_color_manual(values = region_colors_cb, name = "Region", drop = FALSE) + # Apply new region colors
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) 
    ggsave("plot_demgov_vs_central_bank_prop.svg", plot = plot_demgov_vs_central_bank_prop, 
           width = academic_wide_width, height = academic_single_height * 1.3, dpi = academic_plot_dpi)
    cat("Saved plot_demgov_vs_central_bank_prop.svg\n")
  } else {
    cat("Skipping scatterplot (Democratic Governance vs. Central Bank Proportion): Not enough data after NA filtering for plotting.\n")
  }
} else {
  cat("Skipping Central Bank Proportion plots: df_for_new_scatterplot dataframe not available or is empty.\n")
}

# Histograms: Proportion of Central Bankers in Network, faceted by Region
if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  plot_data_cb_hist <- df_for_new_scatterplot %>%
    filter(!is.na(central_bank_prop) & !is.na(region))
  
  if (nrow(plot_data_cb_hist) > 0) {
    # For histograms, since it's faceted by region, a consistent fill color for the bars is used.
    # The fill won't be mapped to MembershipCategory.
    plot_hist_central_bank_prop_by_region <- ggplot(plot_data_cb_hist, aes(x = central_bank_prop)) + # Removed fill aesthetic here
      geom_bar(position = "stack", width = 0.05, fill = cb_dark_pastel_palette[1], na.rm = TRUE) + # Single fill color from palette
      facet_wrap(~ region, scales = "free_y") +
      labs(title = "Distribution of Central Bank Proportion by Region",
           x = "Central Bank Proportion (from df_granular.csv)",
           y = "Number of Networks") + # Removed fill from labs
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            strip.text = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") + # Removed legend if fill is static
      scale_x_continuous(breaks = unique(sort(plot_data_cb_hist$central_bank_prop)))
    
    print(plot_hist_central_bank_prop_by_region)
    ggsave("plot_hist_central_bank_prop_by_region.svg", plot = plot_hist_central_bank_prop_by_region, 
           width = academic_wide_width, height = academic_single_height * 1.1, dpi = academic_plot_dpi)
    cat("Saved plot_hist_central_bank_prop_by_region.svg\n")
  } else {
    cat("Skipping histograms (Central Bank Proportion by Region): Not enough data after NA filtering for plotting.\n")
  }
} else {
  cat("Skipping Central Bank Proportion histograms: df_for_new_scatterplot dataframe not available or is empty.\n")
}

# 3. Stacked Bar Plot: Proportion of Central Bankers by Network, faceted by Region
# This plot visually represents the 'central_bank_prop' using a two-color stacked bar.
if (exists("df_for_new_scatterplot") && nrow(df_for_new_scatterplot) > 0) {
  plot_data_stacked_bar <- df_for_new_scatterplot %>%
    filter(!is.na(central_bank_prop) & !is.na(network) & !is.na(region)) %>%
    mutate(
      cb_part = central_bank_prop,
      non_cb_part = 1 - central_bank_prop
    ) %>%
    pivot_longer(
      cols = c(cb_part, non_cb_part),
      names_to = "proportion_type",
      values_to = "proportion_value"
    ) %>%
    mutate(
      proportion_type = factor(proportion_type, levels = c("non_cb_part", "cb_part")),
      color_label = case_when(
        proportion_type == "cb_part" ~ "Central Bank",
        proportion_type == "non_cb_part" ~ "Other"
      ),
      network = fct_reorder(network, central_bank_prop, .desc = FALSE)
    ) %>%
    filter(proportion_value > 0) 
  
  if (nrow(plot_data_stacked_bar) > 0) {
    # Apply global colorblind-safe pastel colors for the two parts of the stacked bar.
    plot_central_bank_proportion_bar <- ggplot(plot_data_stacked_bar, 
                                               aes(x = network, y = proportion_value, fill = color_label)) +
      geom_col(width = 0.7, position = "stack", na.rm = TRUE) +
      facet_wrap(~ region, scales = "free_x", ncol = 1) +
      labs(title = "Proportion of Central Bankers in Network",
           subtitle = "Composition by Network and Region",
           x = "",
           y = "Proportion",
           fill = "Membership Type") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = cb_stacked_colors_cb) + # Apply new stacked bar colors
      geom_text(aes(label = scales::percent(central_bank_prop, accuracy = 1), y = 1),
                data = plot_data_stacked_bar %>% 
                  filter(proportion_type == "cb_part") %>% 
                  group_by(network) %>% slice(1),
                vjust = -0.5, size = 3, na.rm = TRUE, color = "black") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            strip.text = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    print(plot_central_bank_proportion_bar)
    ggsave("plot_central_bank_proportion_bar.svg", plot = plot_central_bank_proportion_bar, 
           width = academic_wide_width, height = academic_very_wide_width, dpi = academic_plot_dpi)
    cat("Saved plot_central_bank_proportion_bar.svg\n")
  } else {
    cat("Skipping stacked bar plot (Central Bank Proportion): Not enough data after NA filtering for plotting.\n")
  }
} else {
  cat("Skipping stacked bar plot (Central Bank Proportion): df_for_new_scatterplot dataframe not available or is empty.\n")
}

cat("\n--- End of Central Bank Proportion Plots ---\n")

# --- Export Processed Dataframe to CSV ---
if (exists("df_for_new_scatterplot") && is.data.frame(df_for_new_scatterplot) && nrow(df_for_new_scatterplot) > 0) {
  output_csv_filename <- "df_processed_networks_with_cb_prop.csv"
  write.csv(df_for_new_scatterplot, file = output_csv_filename, row.names = FALSE)
  cat(paste0("\nSuccessfully exported 'df_for_new_scatterplot' to '", output_csv_filename, "'.\n"))
  cat(paste0("You can find this file in your working directory: ", getwd(), "\n"))
} else {
  cat("\nSkipping CSV export: 'df_for_new_scatterplot' dataframe is not available or is empty.\n")
  cat("Please ensure previous data processing steps completed successfully.\n")
}
cat("\n--- CSV Export Process Complete ---\n")

cat("\n--- End of Combined Script ---\n")
