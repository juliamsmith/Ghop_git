library(purrr)
library(dplyr)
library(tidyr)

process_pop_year_flexible <- function(p, year, climate_site) {
  original_site <- pops[p,]$site
  
  # Check if simulation should be run (i.e., if dates are not NA for the climate site)
  if (year == 22) {
    start_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_start_date_22")]
    end_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_end_date_22")]
  } else {
    start_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_start_date_23")]
    end_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_end_date_23")]
  }
  
  if (is.na(start_date) || is.na(end_date)) {
    return(NULL)  # Skip simulation if dates are NA
  }
  
  results <- map_dfr(tbs, function(tb) {
    dthing <- pop_energy_complex(pops[p,]$spp, climate_site, pops[p,]$sex, 
                                 start_date, end_date, 
                                 pops, clim2, tb)
    dthing$tbtype <- tb
    return(dthing)
  })
  
  results %>%
    pivot_longer(cols = c(gains, losses, net_gains), names_to = "meas", values_to = "vals") %>%
    mutate(spp = pops[p,]$spp,
           original_site = original_site,
           climate_site = climate_site,
           sex = pops[p,]$sex,
           year = paste0("20", year))
}

# Function to process all combinations of populations and climate sites
process_all_combinations <- function() {
  all_results <- list()
  
  for (p in 1:nrow(pops)) {
    for (climate_site in sitesinfo$site) {
      # Process 2022 data
      result_22 <- process_pop_year_flexible(p, 22, climate_site)
      if (!is.null(result_22)) all_results <- c(all_results, list(result_22))
      
      # Process 2023 data
      result_23 <- process_pop_year_flexible(p, 23, climate_site)
      if (!is.null(result_23)) all_results <- c(all_results, list(result_23))
    }
  }
  
  return(all_results)
}

# Run the simulation for all combinations
all_results2 <- process_all_combinations()

# Combine all results into a single dataframe
final_results2 <- bind_rows(all_results2)