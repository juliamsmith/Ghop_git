library(tidyverse)

# Load both datasets
old_data <- readRDS("D:/Ghop_git/biophys/pairedcagewsbothyrs.RDS")
new_data <- readRDS("D:/Ghop_git/biophys/pairedcagesclimateuse2bothyrs.rds") %>% 
  filter(Site==coll_site)

# ============================================
# COMPARE SAMPLE SIZES
# ============================================

print("=== OLD DATA ===")
print(paste("Total rows:", nrow(old_data)))
old_data %>% 
  filter(!is.na(Activity)) %>%
  count(year, Site, Species) %>%
  pivot_wider(names_from = Species, values_from = n, values_fill = 0) %>%
  arrange(year, Site) %>%
  print()

print("")
print("=== NEW DATA ===")
print(paste("Total rows:", nrow(new_data)))
new_data %>% 
  filter(!is.na(Activity)) %>%
  count(year, Site, Species) %>%
  pivot_wider(names_from = Species, values_from = n, values_fill = 0) %>%
  arrange(year, Site) %>%
  print()

# ============================================
# CHECK FOR THE PROBLEMATIC COMBINATIONS
# ============================================

# These should NOT exist if filtering is correct:
# - MB at Eldo (MB doesn't naturally occur at lowest elevation)
# - MS at C1 (MS doesn't occur at highest elevation)

print("")
print("=== PROBLEMATIC COMBINATIONS (should be 0 if filtered to home-site) ===")
print("OLD DATA:")
print(paste("  MB at Eldo:", sum(old_data$Species == "MB" & old_data$Site == "Eldo", na.rm = TRUE)))
print(paste("  MS at C1:", sum(old_data$Species == "MS" & old_data$Site == "C1", na.rm = TRUE)))

print("NEW DATA:")
print(paste("  MB at Eldo:", sum(new_data$Species == "MB" & new_data$Site == "Eldo", na.rm = TRUE)))
print(paste("  MS at C1:", sum(new_data$Species == "MS" & new_data$Site == "C1", na.rm = TRUE)))

# ============================================
# CREATE COMPARISON TABLE (what you'd see in the plot)
# ============================================

# For old data, the temperature column is T_1.00, not T_1.00use
old_table <- old_data %>%
  filter(!is.na(Activity)) %>%
  mutate(Activity2 = dplyr::recode(Activity, WALK = "MOVE", CLMB = "MOVE")) %>%
  filter(!Activity %in% c("EGGL", "MATE", "GROO")) %>%
  mutate(airtemp = case_when(
    T_1.00 >= 7.5 & T_1.00 < 12.5 ~ "10",
    T_1.00 >= 12.5 & T_1.00 < 17.5 ~ "15",
    T_1.00 >= 17.5 & T_1.00 < 22.5 ~ "20",
    T_1.00 >= 22.5 & T_1.00 < 27.5 ~ "25",
    T_1.00 >= 27.5 & T_1.00 < 32.5 ~ "30",
    T_1.00 >= 32.5 & T_1.00 < 39 ~ "35",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(airtemp)) %>%
  count(Site, Species, airtemp, Activity2) %>%
  group_by(Site, Species, airtemp) %>%
  mutate(
    total = sum(n),
    prop = round(n / total, 3)
  ) %>%
  ungroup() %>%
  mutate(dataset = "old")

# For new data, use T_1.00use
new_table <- new_data %>%
  filter(!is.na(Activity)) %>%
  mutate(Activity2 = dplyr::recode(Activity, WALK = "MOVE", CLMB = "MOVE")) %>%
  filter(!Activity %in% c("EGGL", "MATE", "GROO")) %>%
  mutate(airtemp = case_when(
    T_1.00use >= 7.5 & T_1.00use < 12.5 ~ "10",
    T_1.00use >= 12.5 & T_1.00use < 17.5 ~ "15",
    T_1.00use >= 17.5 & T_1.00use < 22.5 ~ "20",
    T_1.00use >= 22.5 & T_1.00use < 27.5 ~ "25",
    T_1.00use >= 27.5 & T_1.00use < 32.5 ~ "30",
    T_1.00use >= 32.5 & T_1.00use < 39 ~ "35",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(airtemp)) %>%
  count(Site, Species, airtemp, Activity2) %>%
  group_by(Site, Species, airtemp) %>%
  mutate(
    total = sum(n),
    prop = round(n / total, 3)
  ) %>%
  ungroup() %>%
  mutate(dataset = "new")

# Print tables
print("")
print("=== OLD DATA PLOT TABLE ===")
old_table %>%
  select(Site, Species, airtemp, Activity2, n, total, prop) %>%
  arrange(Site, Species, airtemp, Activity2) %>%
  print(n = 100)

print("")
print("=== NEW DATA PLOT TABLE ===")
new_table %>%
  select(Site, Species, airtemp, Activity2, n, total, prop) %>%
  arrange(Site, Species, airtemp, Activity2) %>%
  print(n = 100)

# ============================================
# SIDE-BY-SIDE COMPARISON
# ============================================

comparison <- full_join(
  old_table %>% select(Site, Species, airtemp, Activity2, n_old = n, prop_old = prop),
  new_table %>% select(Site, Species, airtemp, Activity2, n_new = n, prop_new = prop),
  by = c("Site", "Species", "airtemp", "Activity2")
) %>%
  mutate(
    n_diff = coalesce(n_new, 0L) - coalesce(n_old, 0L),
    prop_diff = round(coalesce(prop_new, 0) - coalesce(prop_old, 0), 3)
  )

print("")
print("=== COMPARISON (showing differences) ===")
comparison %>%
  filter(n_diff != 0 | is.na(n_old) | is.na(n_new)) %>%
  arrange(Site, Species, airtemp, Activity2) %>%
  print(n = 100)

# ============================================
# CHECK: Does new data have coll_site info?
# ============================================

print("")
print("=== CHECKING FOR coll_site COLUMN ===")
print(paste("coll_site in old_data:", "coll_site" %in% names(old_data)))
print(paste("coll_site in new_data:", "coll_site" %in% names(new_data)))

if ("coll_site" %in% names(new_data)) {
  print("")
  print("New data coll_site vs Site breakdown:")
  new_data %>%
    filter(!is.na(coll_site)) %>%
    mutate(home_site = (Site == coll_site)) %>%
    count(year, home_site) %>%
    print()
  
  print("")
  print("If we filter new_data to home-site only:")
  new_data_filtered <- new_data %>% filter(Site == coll_site)
  print(paste("  MB at Eldo:", sum(new_data_filtered$Species == "MB" & new_data_filtered$Site == "Eldo", na.rm = TRUE)))
  print(paste("  MS at C1:", sum(new_data_filtered$Species == "MS" & new_data_filtered$Site == "C1", na.rm = TRUE)))
}




print((arrange(old_data, Site) %>% select(wdt, Site, Date.Created, Content, t, dt, T_1.00))[c(123,133),])
print((arrange(new_data, Site) %>% select(climate_dt, Site, Date.Created, Content, t, dt.x, dt.y, T_1.00, T_1.00use))[c(123,133),])