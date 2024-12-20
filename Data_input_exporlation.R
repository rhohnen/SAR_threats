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
  filter(!(rowID %in% c("2024", "1505", "2047")))

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

# Remove row 52 as sums to 0
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  filter(row_number() != 52)

#### to make a consistent data set replace any 2 values with a 1
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(across(everything(), ~ ifelse(. == 2, 1, .)))

###Create subset using threat subcategories, and excluding overarching categories (that would be correlated with the sub categories)
subset_data <- overlap_caribou_threats_single %>% select(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
                                                         41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
                                                        68)
# Convert all columns to factors
subset_data <- subset_data %>% mutate(across(everything(), as.numeric))

###Create subset using all categories and subcategories
subset_data_large <- overlap_caribou_threats_single %>% select(15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
                                                         68)
# Convert all columns to factors
subset_data_large <- subset_data_large %>% mutate(across(everything(), as.numeric))


###################################################################################
### try to compute similarity between species and woodland caribou row using the Jaccard index
library(vegan)
dist <-vegdist(subset_data, method = "jaccard")
# Convert dist object to a matrix
dist_matrix <- as.matrix(dist)

# Convert matrix to a data frame
dist_df <- as.data.frame(dist_matrix)

# Extract row 42 from dist_df
row_17 <- dist_df[17, ]

# make similarity matrix
similarity_matrix <- 1 - dist_matrix

# Convert matrix to a data frame
sim_df <- as.data.frame(similarity_matrix)

# Extract row 42 from similarity matrix
row_17_sim <- sim_df[17, ]

# Add row_42 as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(row_17 = as.numeric(row_17))

# Add row_42_diss as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(row_17_sim = as.numeric(row_17_sim))

### create jaccard index for big subset
dist_large <-vegdist(subset_data_large, method = "jaccard")
# Convert dist object to a matrix
dist_matrix_large <- as.matrix(dist_large)
dist_df_large <- as.data.frame(dist_matrix_large)
row_17_L <- dist_df_large[17, ]

# make similarity matrix
similarity_matrix_large <- 1 - dist_matrix_large
sim_df_large <- as.data.frame(similarity_matrix_large)
row_17_sim_L <- sim_df_large[17, ]

# Add rows as a column to overlap_caribou_threats_single
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(row_17_L = as.numeric(row_17_L))
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(row_17_sim_L = as.numeric(row_17_sim_L))

# find species with jaccard scores of >0.5

# Filter dataset for rows with similarity >0.48 to find ten most similar
jaccard_0.48 <- overlap_caribou_threats_single %>%
  filter(row_17_sim > 0.48)

# Delete threat columns
jaccard_0.48 <- jaccard_0.48 %>%
  select(-c(15,16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
            41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,
            67,68,69))

# find 10ish species most dissimilar
jaccard_0.21 <- overlap_caribou_threats_single %>%
  filter(row_17_sim < 0.21)

# Delete threat columns
jaccard_0.21 <- jaccard_0.21 %>%
  select(-c(15,16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
            41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,
            67,68,69))


####look at the number of column in common between caribou and all other species in the dataset
# First, extract the row corresponding to "caribou"
caribou_row <- overlap_caribou_threats_single %>% 
  filter(common_name == "caribou boreal population") %>%
  select(c(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
           41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
           68))

# Create dataset to compare with
species_all_row <- overlap_caribou_threats_single %>%
  select(c(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
            41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
            68))

# Create a new dataset comparing "caribou" to all other rows (and count 1's)
comparison <- species_all_row %>%
  rowwise() %>%
  mutate(matching_ones = sum(caribou_row == 1 & c_across(everything()) == 1, na.rm = TRUE)) %>%
  ungroup()

# Add common_columns column (!) to overlap_caribou_threats single
matching_ones <- comparison$matching_ones
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  mutate(matching_ones = as.numeric(matching_ones))


###################################################################################
### Run MCA to visualize differences based on taxonomic group
### try MCA
library("FactoMineR")
library("factoextra")

###Create subset for MCA. This subset has the sub groups but not the group categories
subset_data_MCA<- overlap_caribou_threats_single %>% select(c(16,17,18,20,21,22,23,25,26,27,29,30,31,32,34,35,36,37,39,40,
                                                              41,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,61,62,63,65,66,67,
                                                              68))
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
# Cos2: quality on the factor map
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

### graph individuals (species in this case)

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
taxons <- as.factor(overlap_caribou_threats_single$taxonomic_group)
fviz_mca_ind(MCA, habillage = taxons, addEllipses = FALSE)

# Group by overlap with caribou
overlap <- as.numeric(overlap_caribou_threats_single$Percent_caribou)
fviz_mca_ind(
  MCA, 
  geom = "point", 
  col.ind = overlap,  # Use numeric vector for coloring
  gradient.cols = c("blue", "yellow", "red"),  # Continuous gradient
  addEllipses = FALSE
) +
  labs(color = "Overlap") # Adjust legend label

# Group by total range of caribou
range <- as.numeric(overlap_caribou_threats_single$Total_area_km)
fviz_mca_ind(
  MCA, 
  geom = "point", 
  col.ind = range, # Use Area for coloring
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

###########################################################################
## Check correlations prior to modelling
## Compute correlations
## Change some columns to factors
library(corrplot)

model_subset <- overlap_caribou_threats_single %>% select(3,6,12,70,72,73,75,76,77,78)

numeric_vars <- model_subset %>%
  select(Percent_caribou, Total_area_km, sara_status, taxonomic_group, row_17, row_17_sim, matching_ones, row_17_L,row_17_sim_L) %>%
  mutate(
  sara_status = as.numeric(as.factor(sara_status)),  # Convert factors to numeric
  taxonomic_group = as.numeric(as.factor(taxonomic_group))
  )

correlation_matrix <- cor(numeric_vars, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")

# Print the correlation matrix
print(correlation_matrix)

# View the levels of a factor variable
levels(overlap_caribou_threats_single$taxonomic_group)

## Run some glms looking at descriptors of threat overlap with caribou
# Delete row 17 (caribou)
overlap_caribou_threats_single <- overlap_caribou_threats_single %>%
  filter(row_number() != 17)

# Make reference level for the variable taxonomic group
overlap_caribou_threats_single$taxonomic_group <- factor(overlap_caribou_threats_single$taxonomic_group, 
                                        levels = c("Molluscs", "Arthropods", "Birds", "Lichens", "Amphibians",
                                        "Mammals_(terrestrial)", "Mosses", "Reptiles", "Vascular_Plants"))
overlap_caribou_threats_single$taxonomic_group <- relevel(overlap_caribou_threats_single$taxonomic_group, ref = "Mosses")

library(stats)
library(gtools)
library(MuMIn)

n1 <- lm(row_17_sim~ scale(Percent_caribou), data=overlap_caribou_threats_single)
n2 <- lm(row_17_sim~ scale(Total_area_km), data=overlap_caribou_threats_single)
n3 <- lm(row_17_sim~ sara_status, data=overlap_caribou_threats_single)
n4 <- lm(row_17_sim~ taxonomic_group, data=overlap_caribou_threats_single)
n5 <- lm(row_17_sim~ scale(Percent_caribou) + scale(Total_area_km) + sara_status + taxonomic_group, data=overlap_caribou_threats_single)
n6 <- lm(row_17_sim~ 1, data=overlap_caribou_threats_single)

summary(n4)

fmList<-model.sel(n1=n1,n2=n2,n3=n3,n4=n4,n5=n5,n6=n6)
fmList
summary(model.avg(fmList, subset = delta < 2))

### other options

a1 <- lm(logit(row_17_sim)~ scale(Percent_caribou), data=overlap_caribou_threats_single)
a2 <- lm(logit(row_17_sim)~ scale(Total_area_km), data=overlap_caribou_threats_single)
a3 <- lm(logit(row_17_sim)~ sara_status, data=overlap_caribou_threats_single)
a4 <- lm(logit(row_17_sim)~ taxonomic_group, data=overlap_caribou_threats_single)
a5 <- lm(logit(row_17_sim)~ scale(Percent_caribou) + scale(Total_area_km) + sara_status + taxonomic_group, 
         data=overlap_caribou_threats_single)
a6 <- lm(logit(row_17_sim)~ 1, data=overlap_caribou_threats_single)
summary(a4)
fmList2<-model.sel(a1=a1,a2=a2,a3=a3,a4=a4,a5=a5,a6=a6)
fmList2


b1 <- glm(round(cbind(row_17_sim, 1)*100)~ scale(Percent_caribou), data=overlap_caribou_threats_single, family=binomial)
b2 <- glm(round(cbind(row_17_sim, 1)*100)~ scale(Total_area_km), data=overlap_caribou_threats_single,family=binomial)
b3 <- glm(round(cbind(row_17_sim, 1)*100)~ sara_status, data=overlap_caribou_threats_single,family=binomial)
b4 <- glm(round(cbind(row_17_sim, 1)*100)~ taxonomic_group, data=overlap_caribou_threats_single,family=binomial)
b5 <- glm(round(cbind(row_17_sim, 1)*100)~ scale(Percent_caribou) + scale(Total_area_km) + sara_status + taxonomic_group, 
         data=overlap_caribou_threats_single,family=binomial)
b6 <- glm(round(cbind(row_17_sim, 1)*100)~ 1, data=overlap_caribou_threats_single,family=binomial)

fmList3<-model.sel(b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6)
fmList3
summary(b5)

#m1 <- glm(cbind(matching_ones, 33-matching_ones)~ scale(Percent_caribou), data=overlap_caribou_threats_single,family=binomial)
#m2 <- glm(cbind(matching_ones, 33-matching_ones)~ scale(Total_area_km), data=overlap_caribou_threats_single,family=binomial)
#m3 <- glm(cbind(matching_ones, 33-matching_ones)~sara_status, data=overlap_caribou_threats_single,family=binomial)
#m4 <- glm(cbind(matching_ones, 33-matching_ones)~ taxonomic_group, data=overlap_caribou_threats_single,family=binomial)
#m5 <- glm(cbind(matching_ones, 33-matching_ones)~ scale(Percent_caribou) + scale(Total_area_km), data=overlap_caribou_threats_single,family=binomial)
#m6 <- glm(cbind(matching_ones, 33-matching_ones)~ scale(Percent_caribou) + scale(Total_area_km) + sara_status + taxonomic_group, data=overlap_caribou_threats_single,family=binomial)
#m7 <- glm(cbind(matching_ones, 33-matching_ones)~ 1, data=overlap_caribou_threats_single,family=binomial)

#fmList4-model.sel(m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6)
#fmList4

summary(m6)

coef(fmList)
m3 <- glm(row_17_sim_L~scale(Percent_caribou) + scale(Total_area_km) + sara_status + taxonomic_group, data=overlap_caribou_threats_single,
          family=quasibinomial(link = "logit"))


# View the levels of a factor variable
levels(overlap_caribou_threats_single$taxonomic_group)
levels(overlap_caribou_threats_single$sara_status)

# Proportion of deviance explained
null_dev <- summary(m2)$null.deviance
resid_dev <- summary(m2)$deviance
prop_deviance_explained <- (null_dev - resid_dev) / null_dev
cat("Proportion of Deviance Explained: ", prop_deviance_explained, "\n")

# Dispersion parameter
dispersion <- resid_dev / df.residual(m2)
cat("Dispersion Parameter: ", dispersion, "\n")

# Residual Diagnostics
par(mfrow = c(1, 2)) # Plot side by side
plot(fitted(b5), residuals(b5), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals(b5))
qqline(residuals(b5), col = "blue")

## Plot model averaged coefficient estimates of best fitting model

## Make plot of model averaged proficients estimates (all estimates and confidence intervals were computed first in excel)
## Currently b5
# Extract coefficients and standard errors from the model
model_summary <- summary(b5)  # Replace 'glm_model' with your model name
coefficients <- as.data.frame(model_summary$coefficients)

# Create a new data frame for plotting
comp2 <- data.frame(
  Variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  Std_Error = coefficients[, "Std. Error"],
  Lower = coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"],  # 95% CI lower bound
  Upper = coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"]   # 95% CI upper bound
)

library(ggplot2)

# Rename categories
comp2 <- comp2 %>%
  mutate(Variable = recode(Variable, 
                         "scale(Percent_caribou)" = "Percentage range overlap with caribou",
                         "scale(Total_area_km)" = "Total range area",
                         "sara_statusSpecial Concern" = "Special concern",
                         "sara_statusThreatened" = "Threatened",
                         "taxonomic_groupArthropods" = "Arthropods",
                         "taxonomic_groupBirds" = "Birds",
                         "taxonomic_groupLichens" = "Lichens",
                         "taxonomic_groupAmphibians" = "Amphibians",
                         "taxonomic_groupMammals_(terrestrial)" = "Mammals (terrestrial)",
                         "taxonomic_groupReptiles" = "Reptiles",
                         "taxonomic_groupVascular_Plants" = "Vascular plants",
                         "taxonomic_groupMolluscs" = "Molluscs",
                          "(Intercept)"="Intercept"))


# Specify the desired order of categories
desired_order5 <- c("Total range area",
                    "Percentage range overlap with caribou",
                    "Threatened",
                    "Special concern",
                    "Mammals (terrestrial)",
                    "Amphibians",
                    "Reptiles",
                    "Molluscs",
                    "Birds",
                    "Vascular plants",
                    "Lichens",
                    "Arthropods",
                    "Intercept")
 
# Reorder the factor levels in the data frame
comp2$Variable <- factor(comp2$Variable, levels = desired_order5)

p = ggplot(comp2,aes(x=Variable,y=Estimate)) +
  geom_hline(yintercept = 0, col = "darkgrey") +  # Horizontal line at y = 0
  geom_point() +
  geom_errorbar(aes(ymin=Lower,ymax=Upper)) +
  coord_flip() +
  labs(x= "Variable", y= "Coefficent estimate") +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 12),  # Increase overall font size
    axis.title = element_text(size = 14),  # Axis titles
    axis.text = element_text(size = 12))  # Axis text
print(p)


## Plot SAR taxonomic group and caribou range overlap

# Make a graph that shows which taxonomic groups of SAR overlap most with caribou ranges
library(ggplot2)

overlap <- read.csv("SAR_cara_overlap.csv", header=TRUE,)
head(overlap)
# Reshape data for grouped bar plot
library(tidyr)
overlap_long <- overlap %>%
  pivot_longer(cols = c(SAR, SAR_caribou_range, SAR_caribou_range_20),
               names_to = "Metric", 
               values_to = "Value")

# Specify the desired order of categories
desired_order4 <- c(
  "Vascular Plants",
  "Birds",
  "Arthropods",
  "Reptiles",
  "Amphibians",
  "Mammals",
  "Lichens",
  "Molluscs",
  "Mosses"
)
# Reorder the factor levels in the data frame
overlap_long$Group <- factor(overlap_long$Group, levels = desired_order4)

# Create grouped bar plot
ggplot(overlap_long, aes(x = Group, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Taxonomic group",
       y = "Number of species",
       fill = "Metric") +
  scale_fill_manual(values = c("darkseagreen2", "khaki1", "green4"),
                    labels = c("SAR", "SAR that overlap with caribou", "SAR that overlap with caribou >20%")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 16),  # Increase overall font size
    axis.title = element_text(size = 18),  # Axis titles
    axis.text = element_text(size = 16),  # Axis text
  )


#########################################################################################################
### What are the most common threats to SAR in Canada?
### How do these vary from the species within the caribou range, and those listed for caribou?
### First make data set
# Filter rows where percent_caribou > 0

# Replace spaces with underscores in a specific column (taxonomic_group)
threats_all <- threats %>%
  mutate(taxonomic_group = gsub(" ", "_", taxonomic_group))

# Keep only the most recent year_published for each common_name
threats_all <- threats_all %>%
  group_by(common_name) %>%
  filter(year_published == max(year_published)) %>%
  ungroup()

###delete caribou that aren't relevant and multiples of the same species that weren't eliminated with the process above
threats_all <- threats_all %>%
  filter(!(rowID %in% c("1605", "631","1545", "732","615", "612", "639","663", "114",
                        "889", "662","221", "228", "909", "579", "697", "710", "320",
                        "543", "358", "923", "567")))

#### to make a consistent data set replace any 2 values with a 1
threats_all <- threats_all %>%
  mutate(across(everything(), ~ ifelse(. == 2, 1, .)))

library(tidyr)
subset_data_caribou_title <- overlap_caribou_threats_single %>% select(15,19,24,28,33,38,42,46,53,60,64,69)
subset_cat_title <- threats_all %>% select(15,19,24,28,33,38,42,46,53,60,64,69)

summed_threats_caribou <- subset_data_caribou_title %>% 
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Threat", values_to = "Summed_Species_cara") 

summed_threats_title <- subset_cat_title %>% 
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Threat", values_to = "Summed_Species_all")

Threats_both <- summed_threats_title %>%
  mutate(Summed_Species_cara = summed_threats_caribou$Summed_Species_cara)

## Calculate proportions of all species
Threats_both <- Threats_both %>%
  mutate(Proportion_all = (Summed_Species_all/612)*100)

## Calculate proportions of caribou overlapping species
Threats_both <- Threats_both %>%
  mutate(Proportion_cara = (Summed_Species_cara/91)*100)

Threats_both2 <- Threats_both %>% 
  pivot_longer(cols = starts_with("Proportion"), names_to = "type", values_to = "Proportion")

# Rename categories
Threats_both2 <- Threats_both2 %>%
  mutate(Threat = recode(Threat, 
                        "agri_aqua" = "Agriculture and aquaculture",
                        "bio_resource_use" = "Biological resource use",
                        "climate_change" = "Climate change",
                         "energy" = "Energy production and mining",
                         "geological_event" = "Geological events",
                         "human_disturbance" = "Human disturbance",
                         "invasive_species" = "Invasive and problematic species",
                         "natural_sys_change" = "Natural system modifications",
                         "other_impacts" = "Other options",
                         "pollution" = "Pollution",
                         "res_comm_development" = "Residential and commercial development",
                         "transport_service" = "Transport and service corridors"))

# Specify the desired order of categories
desired_order3 <- c(
  "Invasive and problematic species",
  "Natural system modifications",
  "Pollution",
  "Climate change",
  "Human disturbance",
  "Residential and commercial development",
  "Biological resource use",
  "Transport and service corridors",
  "Agriculture and aquaculture",
  "Energy production and mining",
  "Geological events",
  "Other options"
)
# Reorder the factor levels in the data frame
Threats_both2$Threat <- factor(Threats_both2$Threat, levels = desired_order3)

# Define custom colors
custom_colors <- c(
  'Proportion_all' = 'deepskyblue4',  # Dark teal color for 'wolverine'
  'Proportion_cara' = 'cadetblue2'  # Light blue color for 'woodland caribou'
)

# Create grouped bar plot for threats to all SAR
ggplot(Threats_both2, aes(x = Threat, y = Proportion, fill=type, width=0.45)) +
  geom_bar(stat = "identity", position='dodge') +
  labs(x = "Threat",
       y = "Proportion of species") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 0)),
    axis.text.x = element_text(angle = 65, hjust = 1, margin = margin(t = 0, r = 0, b = 20, l = 0)),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 16),  # Increase overall font size
    axis.title = element_text(size = 18),  # Axis titles
    axis.text = element_text(size = 16),  # Axis text
    legend.position = "none"
  )



