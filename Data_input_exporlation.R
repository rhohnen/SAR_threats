library(dplyr)

### read in dataset
threats <-read.csv('SAR_overlap_threats_noNA.csv')
head(threats)

### clean dataset
# Filter rows where percent_caribou > 0
overlap_caribou_threats <- threats %>%
  filter(Percent_caribou > 0)

# Keep only the most recent year_published for each common_name
overlap_caribou_threats_single <- overlap_caribou_threats %>%
  group_by(common_name) %>%
  filter(year_published == max(year_published)) %>%
  ungroup()
###delete caribou that aren't relevant and multiples of the same species that weren't eliminated with the process above
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  filter(common_name != "Caribou Atlantic-Gaspésie Population") %>%
  filter(common_name != "caribou atlantic-gaspÃƒÂ©sie population") %>%
  filter(common_name != "caribou dolphin and union population") %>%
  filter(rowID != "2024") %>%
  filter(rowID != "2047")

### find rows with all zeros
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(row_total = rowSums(across(c("res_comm_development", "house_urban", "comm_industrial", "tourisim_rec", "agri_aqua", "crops", "wood_planta",
                                      "livestock_farm", "aqua", "energy", "oil_gas", "mining", "renewable", "transport_service", "roads", "utility_service",
                                      "shipping_lane", "flight_path", "bio_resource_use", "hunting", "gathering", "logging", "fishing", "human_disturbance",
                                      "recreation", "war", "work_activity", "natural_sys_change", "fire_fire_sup", "dams_water", "ecosysmod", "invasive_species",
                                      "invasive_nonative", "problematic_native", "introduced_genetic", "problematic_species", "viral_disease", "disease_unknown",    
                                      "pollution", "sewage_waste_water", "industrial_waste", "agricultural_waste", "garbage_soil_waste", "air_boure_pollute",
                                      "ecess_energ7", "geological_event", "volcano", "earth_quake", "avalanche", "climate_change", "hab_shift", "drought", 
                                      "temp_extreme","storm_flood"))))

###Delete all rows with zeros
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  filter(row_total != "0") 

###Create subset

subset_data <- overlap_caribou_threats_single %>% select(15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
                                                        68,69,70,71,72,73,74)
# Convert all columns to factors
subset_data <- subset_data %>% mutate(across(everything(), as.numeric))

### try to compute similarity between species and woodland caribou row using the Jaccard index

library(vegan)

dist <-vegdist(subset_data, method = "jaccard")
# Convert dist object to a matrix
dist_matrix <- as.matrix(dist)

# Convert matrix to a data frame
dist_df <- as.data.frame(dist_matrix)
# Add species column from overlap_caribou_threats_single
#dist_df <- dist_df %>%
  #mutate(species = overlap_caribou_threats_single$species)

# Extract row 42 from dist_df
row_42 <- dist_df[42, ]

# Add row_42 as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(Row42 = as.numeric(row_42))
summary(overlap_caribou_threats_single)

##change some columns to factors
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(
    sara_status = as.factor(sara_status),
    taxonomic_group = as.factor(taxonomic_group)
  )

model_subset <- overlap_caribou_threats_single %>% select(3,6,12,70,72,73,75)
## check correlations
# Compute correlations
numeric_vars <- model_subset %>%
  select(Percent_caribou, Total_area_km, sara_status, taxonomic_group) %>%
  mutate(
    sara_status = as.numeric(as.factor(sara_status)),  # Convert factors to numeric
    taxonomic_group = as.numeric(as.factor(taxonomic_group))
  )

correlation_matrix <- cor(numeric_vars, use = "complete.obs")  # Use complete.obs to handle NAs

# Print the correlation matrix
print(correlation_matrix)

###Run some glms
library(stats)
m1 <- glm(Row42~Percent_caribou + Total_area_km + sara_status + taxonomic_group, data=model_subset,
          family=quasibinomial(link = "logit"))

summary(m1)
