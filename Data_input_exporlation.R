library(dplyr)

### read in dataset
threats <-read.csv('SAR_overlap_threats_noNA.csv')
head(threats)

### clean dataset
# Filter rows where percent_caribou > 0
overlap_caribou_threats <- threats %>%
  filter(Percent_caribou > 0)

# Replace spaces with underscores in a specific column (taxonomic_group)
overlap_caribou_threats <- overlap_caribou_threats %>%
  mutate(taxonomic_group = gsub(" ", "_", taxonomic_group))

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

#### to make a consistent data set replace any 2 values with a 1
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(across(everything(), ~ ifelse(. == 2, 1, .)))

###Create subset using threat subcategories, and excluding overarching categories (that would be correlated with the sub categories)
subset_data <- overlap_caribou_threats_single %>% select(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
                                                         41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
                                                        68,69,70,71,72,73,74)
# Convert all columns to factors
subset_data <- subset_data %>% mutate(across(everything(), as.numeric))



###################################################################################
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

# make dissimilarity matrix
dissimilarity_matrix <- 1 - dist_matrix

# Extract row 42 from dissimilarity matrix
row_42_diss <- dissimilarity_matrix[42, ]

# Add row_42 as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(Row42 = as.numeric(row_42))

# Add row_42_diss as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(Row42_diss = as.numeric(row_42_diss))



###################################################################################
### Run MCA to visualize differences based on taxonomic group
### try MCA
library("FactoMineR")
library("factoextra")

###Create subset for MCA. This subset has the sub groups but not the group categories
subset_data_MCA<- overlap_caribou_threats_single %>% select(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
                                                         41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
                                                         68,69)
# Remove rows and columns with no variance
subset_data_MCA <- subset_data_MCA[rowSums(is.na(subset_data_MCA)) != ncol(subset_data_MCA), ]
subset_data_MCA <- subset_data_MCA[, colSums(is.na(subset_data_MCA)) != nrow(subset_data_MCA)]

# Remove columns with very few non-zero values
threshold <- 3
subset_data_MCA <- subset_data_MCA %>%
  select(where(~ sum(.) >= threshold))

# Convert all columns to factors
subset_data_MCA <- data.frame(lapply(subset_data_MCA, as.factor))

#run MCA
MCA <-MCA(subset_data_MCA, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(MCA)
# head(eig.val)

# plot % variance explained
fviz_screeplot(MCA, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(MCA, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

var <- get_mca_var(MCA)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

fviz_mca_var(MCA, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

head(round(var$coord, 2), 4)


fviz_mca_var(MCA, col.var="black", shape.var = 15,
             repel = TRUE)

# Change the transparency by cos2 values
fviz_mca_var(MCA, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
# Create a bar plot of variable cos2 using the function fviz_cos2()[in factoextra]:
fviz_cos2(MCA, choice = "var", axes = 1:2)


head(round(var$contrib,2), 4)
# Contributions of rows to dimension 1
fviz_contrib(MCA, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(MCA, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(MCA, choice = "var", axes = 1:2, top = 15)

fviz_mca_var(MCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
### graph idividuals (species in this case)

ind <- get_mca_ind(MCA)
ind

# Coordinates of column points
head(ind$coord)
# Quality of representation
head(ind$cos2)
# Contributions
head(ind$contrib)

### visualize only individuals (species here)
fviz_mca_ind(MCA, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

### group by external variable 
# habillage = external grouping variable here taxonimic group
fviz_mca_ind(MCA, habillage = overlap_caribou_threats_single$taxonomic_group, 
             addEllipses = FALSE)

# habillage = external grouping variable here taxonimic group
fviz_mca_ind(
  MCA, 
  geom = "point", 
  col.ind = overlap_caribou_threats_single$Intersect_caribou_km, # Use Area for coloring
  gradient.cols = c("blue", "yellow", "red"), # Define a color gradient
  addEllipses = FALSE
) +
  labs(color = "Area") # Label for the color legend

fviz_mca_ind(
  MCA, 
  geom = "point", 
  col.ind = overlap_caribou_threats_single$Total_area_km, # Use Area for coloring
  gradient.cols = c("blue", "yellow", "red"), # Define a color gradient
  addEllipses = FALSE
) +
  labs(color = "Area") # Label for the color legend

### The function dimdesc() [in FactoMineR] can be used to identify the most correlated variables with a given dimension:

res.desc <- dimdesc(MCA, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

### run another MCA but just using the group categories, not the subcategories

###Create subset for MCA. This subset has the sub groups but not the group categories
subset_data_MCA2<- overlap_caribou_threats_single %>% select(15,19,24,28,33,38,42,46,53,60,64)
# Remove rows and columns with no variance
subset_data_MCA2 <- subset_data_MCA2[rowSums(is.na(subset_data_MCA2)) != ncol(subset_data_MCA2), ]
subset_data_MCA2 <- subset_data_MCA2[, colSums(is.na(subset_data_MCA2)) != nrow(subset_data_MCA2)]

# Remove columns with very few non-zero values
threshold <- 3
subset_data_MCA2 <- subset_data_MCA2 %>%
  select(where(~ sum(.) >= threshold))

# Convert all columns to factors
subset_data_MCA2 <- data.frame(lapply(subset_data_MCA2, as.factor))

#run MCA
MCA2 <-MCA(subset_data_MCA2, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(MCA2)
# head(eig.val)

# plot % variance explained
fviz_screeplot(MCA2, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(MCA2, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

var2 <- get_mca_var(MCA2)
var2

# Coordinates
head(var2$coord)
# Cos2: quality on the factore map
head(var2$cos2)
# Contributions to the principal components
head(var2$contrib)

fviz_mca_var(MCA2, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

head(round(var2$coord, 2), 4)


fviz_mca_var(MCA2, col.var="black", shape.var = 15,
             repel = TRUE)

# Change the transparency by cos2 values
fviz_mca_var(MCA2, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
# Create a bar plot of variable cos2 using the function fviz_cos2()[in factoextra]:
fviz_cos2(MCA2, choice = "var", axes = 1:2)


head(round(var2$contrib,2), 4)
# Contributions of rows to dimension 1
fviz_contrib(MCA2, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(MCA2, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(MCA2, choice = "var", axes = 1:2, top = 15)

fviz_mca_var(MCA2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
### graph individuals (species in this case)

ind2 <- get_mca_ind(MCA2)
ind2

# Coordinates of column points
head(ind2$coord)
# Quality of representation
head(ind2$cos2)
# Contributions
head(ind2$contrib)

### visualize only individuals (species here)
fviz_mca_ind(MCA2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

### group by external variable 
# habillage = external grouping variable here taxonomic group
overlap_caribou_threats_single$taxonomic_group <- as.factor(overlap_caribou_threats_single$taxonomic_group)
fviz_mca_ind(MCA2, habillage = overlap_caribou_threats_single$taxonomic_group, 
             addEllipses = FALSE)

# external grouping variable here intersect with caribou range
fviz_mca_ind(
  MCA2, 
  geom = "point", 
  col.ind = overlap_caribou_threats_single$Intersect_caribou_km, # Use Area for coloring
  gradient.cols = c("blue", "yellow", "red"), # Define a color gradient
  addEllipses = FALSE
) +
  labs(color = "Area") # Label for the color legend

# external grouping variable here intersect with area
fviz_mca_ind(
  MCA2, 
  geom = "point", 
  col.ind = overlap_caribou_threats_single$Total_area_km, # Use Area for coloring
  gradient.cols = c("blue", "yellow", "red"), # Define a color gradient
  addEllipses = FALSE
) +
  labs(color = "Area") # Label for the color legend

### The function dimdesc() [in FactoMineR] can be used to identify the most correlated variables with a given dimension:

res.desc2 <- dimdesc(MCA2, axes = c(1,2))
# Description of dimension 1
res.desc2[[1]]
# Description of dimension 2
res.desc2[[2]]

## check correlations prior to modelling
## Compute correlations
## change some columns to factors
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(
    sara_status = as.factor(sara_status),
    taxonomic_group = as.factor(taxonomic_group))

model_subset <- overlap_caribou_threats_single %>% select(3,6,12,70,72,73,75,76)

numeric_vars <- model_subset %>%
  select(Percent_caribou, Total_area_km, sara_status, taxonomic_group) %>%
  mutate(
  sara_status = as.numeric(as.factor(sara_status)),  # Convert factors to numeric
  taxonomic_group = as.numeric(as.factor(taxonomic_group))
  )

correlation_matrix <- cor(numeric_vars, use = "complete.obs")  # Use complete.obs to handle NAs

# Print the correlation matrix
print(correlation_matrix)

## Run some glms looking at descriptors of threat overlap with caribou

library(stats)
m2 <- glm(row_42_diss~Percent_caribou + Total_area_km + sara_status + taxonomic_group, data=model_subset,
          family=quasibinomial(link = "logit"))

summary(m2)
