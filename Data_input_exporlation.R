library(dplyr)
library(tidyr)

### Read in dataset
threats <-read.csv('SAR_overlap_threats_NE_species_input_final_noNA.csv')
head(threats)

### Clean dataset

# Replace spaces with underscores in the taxonomic_group column
all_species_threats <- threats %>%
  mutate(taxonomic_group = gsub(" ", "_", taxonomic_group))

# Replace spaces with underscores in the common name column
all_species_threats <- all_species_threats %>%
  mutate(common_name = gsub(" ", "_", common_name))

# Keep only the most recent year_published for each common_name
all_species_threats <- all_species_threats %>%
  group_by(common_name) %>%
  filter(year_published == max(year_published)) %>%
  ungroup()

# Delete caribou that aren't relevant 
all_species_threats <- all_species_threats %>%
  filter(!(common_name %in% c("caribou_atlantic-gaspesie_population", "caribou_dolphin_and_union_population", "peary_caribou")))

#### To make a consistent data set replace any 2 values with a 1
all_species_threats_no2 <- all_species_threats %>%
  mutate(across(everything(), ~ ifelse(. == 2, 1, .)))

# Delete multiples of the same species that weren't eliminated with the process above
# Keep the row with the greatest number of threats listed per species
# To do this first count the number of traits (sum of 1s across trait columns)

all_species_threats_no2$X1_res_comm_development <- as.numeric(all_species_threats_no2$X1_res_comm_development)

# Convert non-numeric values in trait columns to NA
all_species_threats_no2 <- all_species_threats_no2 %>%
  mutate(trait_count = rowSums(select(., 16:70)))  

# Keep only the row with the highest number of traits per species
all_species_threats_no2 <- all_species_threats_no2 %>%
  group_by(common_name) %>%
  slice_max(trait_count, n = 1) %>%  # Keeps row with highest trait count
  ungroup()

# Delete all rows if not threats are recorded
all_species_threats_no2 <- all_species_threats_no2 %>%
  filter(trait_count != "0") 

# Note that I considered deleting marine mammals, marine fish, marine molluscs and reptiles
# But these groups were deleted anyway as they didn't overlap in range with woodland caribou.

##################################################################################
## Create row for combined mountain and boreal caribou threats

caribous <- all_species_threats_no2 %>% filter(rowID %in% c(264,2136,2137))

# Extract columns 1-14 from row 264 (keep as a single row)
boreal_row_start <- caribous %>% filter(rowID == 264) %>% select(1:15)

# Extract columns 1-14 from row 264 (keep as a single row) -> will need to be updated with woodland caribou area values
boreal_row_end <- caribous %>% filter(rowID == 264) %>% select(71:76)

# Apply the OR operation across rows for columns 15-40 (ie if there is a 1 in the boreal or the mountain rows then in = 1)
or_values <- apply(caribous[, 16:70], 2, function(x) as.numeric(any(x == 1)))

# Combine the copied columns (1-14) and the new calculated columns (15-40)
caribou_row <- cbind(boreal_row_start, as.data.frame(t(or_values))) 
caribou_row2<- cbind(caribou_row, boreal_row_end) 

# Change woodland caribou specific information
caribou_row2$common_name <- "woodland caribou"
caribou_row2$web_pub_date <- "NA"
caribou_row2$year_published <- "NA"
caribou_row2$date_last_access <- "NA"
caribou_row2$Percent_SAR_caribou <- "100"
caribou_row2$Percent_caribou_SAR <- "100"
caribou_row2$Total_area_km <- "3459299.35"
caribou_row2$Intersect_caribou_km <- "3459299.35"

caribou_row2$year_published <- as.integer(caribou_row2$year_published)

# Bind to existing data set
all_species_threats_no2 <- bind_rows(all_species_threats_no2, caribou_row2)

# Delete boreal and mountain caribou rows
all_species_threats_no2 <- all_species_threats_no2 %>%
  filter(!(common_name %in% c("woodland_caribou_northern_mountain_population", "woodland_caribou_southern_mountain_population", "caribou_boreal_population")))

           
#############################################################
## Now make subsets!
# Make a data set with just species that overlap in range with caribou so filter rows where percent_caribou > 0
overlap_species_threats_no2 <- all_species_threats_no2 %>%
  filter(Percent_SAR_caribou > 0)

###Create subset of overlap data set using threat subcategories, and excluding overarching categories (that would be correlated with the sub categories)
subset_data <- overlap_species_threats_no2 %>% select(17,18,19,21,22,23,24,26,27,28,30,31,32,33,35,36,37,38,40,41,42,44,45,46,
                                                       48,49,50,51,52,53,55,56,57,58,59,60,62,63,64,66,67,68,69,70)
                                                        
# Convert all columns to factors
subset_data <- subset_data %>% mutate(across(everything(), as.numeric))

###Create subset using all over arching categories and subcategories
subset_data_large <- overlap_species_threats_no2 %>% select(16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,33,32,34,35,36,37,38,39,40,42,41,43,44,45,46,47,
                                                               48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70)
# Convert all columns to factors
subset_data_large <- subset_data_large %>% mutate(across(everything(), as.numeric))

###Create subset with just overarching categories
subset_data_over <- overlap_species_threats_no2 %>% select(16,20,25,29,34,39,43,47,54,61,65)

all_species_threats_no2$Total_area_km[all_species_threats_no2$Total_area_km == "#N/A"] <- NA
all_species_threats_no2$Intersect_caribou_km[all_species_threats_no2$Intersect_caribou_km == "#N/A"] <- NA
all_species_threats_no2$Percent_SAR_caribou[all_species_threats_no2$Percent_SAR_caribou == "#N/A"] <- NA
all_species_threats_no2$Percent_caribou_SAR[all_species_threats_no2$Percent_caribou_SAR == "#N/A"] <- NA

non_na_count <- sum(!is.na(all_species_threats_no2$Total_area_km))
print(non_na_count)
table(all_species_threats_no2$Spatial.data.source)
sum(df[1, ] > 20, na.rm = TRUE)

## Find out how many row have species range overlap with caribou values of >20
all_species_threats_no2$Percent_SAR_caribou <- as.integer(all_species_threats_no2$Percent_SAR_caribou)
ex <- all_species_threats_no2 %>%
  filter(.[[73]] > 0) %>%
  nrow
ex
###################################################################################
### Try to compute similarity between species and woodland caribou row using the Jaccard index
library(vegan)

###### First compute for all data columns (overarching and subcategories)
dist <-vegdist(subset_data_large, method = "jaccard")
# Convert dist object to a matrix
dist_matrix <- as.matrix(dist)

# Convert matrix to a data frame
dist_df <- as.data.frame(dist_matrix)

# Extract row 42 from dist_df
row_98 <- dist_df[98, ]

# Make similarity matrix
similarity_matrix <- 1 - dist_matrix

# Convert matrix to a data frame
sim_df <- as.data.frame(similarity_matrix)

# Extract row 42 from similarity matrix
row_98_sim <- sim_df[98, ]

# Add row_42 as a column to overlap_caribou_threats_single
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98 = as.numeric(row_98))

# Add row_42_sim as a column to overlap_caribou_threats_single
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98_sim = as.numeric(row_98_sim))

###### Next Create jaccard index for subset of just sub categories
dist_sub_cat <-vegdist(subset_data, method = "jaccard")
# Convert dist object to a matrix
dist_matrix_cat <- as.matrix(dist_sub_cat)
dist_df_cat <- as.data.frame(dist_matrix_cat)
row_98_cat <- dist_df_cat[98, ]

# Make similarity matrix
similarity_matrix_cat <- 1 - dist_matrix_cat
sim_df_cat <- as.data.frame(similarity_matrix_cat)
row_98_sim_cat <- sim_df_cat[98, ]

# Add rows as a column to overlap_caribou_threats_single
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98_cat = as.numeric(row_98_cat))
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98_sim_cat = as.numeric(row_98_sim_cat))

###### Then next create jaccard index for subset of just overarching categories
dist_over_cat <-vegdist(subset_data_over, method = "jaccard")

# Convert dist object to a matrix
dist_matrix_over <- as.matrix(dist_over_cat)
dist_df_over <- as.data.frame(dist_matrix_over)
row_98_over <- dist_df_over[98, ]

# Make similarity matrix
similarity_matrix_over <- 1 - dist_matrix_over
sim_df_over <- as.data.frame(similarity_matrix_over)
row_98_sim_over <- sim_df_over[98, ]

# Add rows as a column to overlap_caribou_threats_single
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98_over = as.numeric(row_98_over))
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(row_98_sim_over = as.numeric(row_98_sim_over))

################################################################################
###### Find species with jaccard scores of >0.5

# Filter dataset for rows with similarity >0.48 to find ten most similar
jaccard_0.63 <- overlap_species_threats_no2 %>%
  filter(row_98_sim > 0.63)

# Delete threat columns
jaccard_0.63 <- jaccard_0.63 %>%
  select(-c(16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
            41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,
            67,68,69,70))

# find 10ish species most dissimilar
jaccard_0.30 <- overlap_species_threats_no2 %>%
  filter(row_98_sim < 0.28)

# Delete threat columns
jaccard_0.30 <- jaccard_0.30 %>%
  select(-c(15,16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
            41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,
            67,68,69))

#############################################################################################
### Try to compute similarity between species and woodland caribou row using the Jaccard index
### by looking at the number of columns in common between caribou and 
### species that overlap in range with caribou

# First, extract the row corresponding to "caribou". Choose all columns (overarching and subcategories)
woodland_caribou_row <- overlap_species_threats_no2 %>% 
  filter(common_name == "woodland caribou") %>%
  select(c(16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,33,32,34,35,36,37,38,39,40,42,41,43,44,45,46,47,
           48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70))

# Create dataset to compare with
species_all_row <- overlap_species_threats_no2 %>%
  select(c(16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,33,32,34,35,36,37,38,39,40,42,41,43,44,45,46,47,
           48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70))

# Create a new dataset comparing "caribou" to all other rows (and count 1's)
comparison <- species_all_row %>%
  rowwise() %>%
  mutate(matching_ones = sum(woodland_caribou_row == 1 & c_across(everything()) == 1, na.rm = TRUE)) %>%
  ungroup()

# Add common_columns column (!) to overlap_species_threats_no2
matching_ones <- comparison$matching_ones
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(matching_ones = as.numeric(matching_ones))

###################################################################################

# Make some other more interesting variables

# Create a new column based on conditions
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(T_group = case_when(
    taxonomic_group %in% c("Mammals_(terrestrial)", "Reptiles", "Amphibians","Birds" ) ~ "vertebrate",  # If category is A or B, assign 1
    taxonomic_group %in% c("Molluscs", "Arthropods") ~ "invertebrate" ,
    taxonomic_group %in% c("Lichens", "Vascular_Plants", "Mosses") ~ "plant" ,# If category is C or D, assign 2
  ))

overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(T_group = case_when(
    taxonomic_group %in% c("Mammals_(terrestrial)", "Reptiles", "Amphibians","Birds" ) ~ "aquatic",  # If category is A or B, assign 1
    taxonomic_group %in% c("Molluscs", "Arthropods") ~ "land" ,
    taxonomic_group %in% c("Lichens", "Vascular_Plants", "Mosses") ~ "plant" ,# If category is C or D, assign 2
  ))
###################################################################################
### Run MCA to visualize differences based on taxonomic group
### try MCA

# Load libraries
library(FactoMineR)
library(factoextra)

# Load dataset
subset_data_MCA<- overlap_species_threats_no2 %>% select(c(12,17,18,19,21,22,23,24,26,27,28,30,31,32,33,35,36,37,38,40,41,42,44,45,46,
                                                           48,49,50,51,52,53,55,56,57,58,59,60,62,63,64,66,67,68,69,70))
# Select taxonomic group and trait columns
taxonomic_col <- "taxonomic_group"  # Adjust based on actual column name
trait_cols <- setdiff(names(subset_data_MCA), taxonomic_col)  # Assuming all other columns are traits

# Convert taxonomic group to a factor
subset_data_MCA[[taxonomic_col]] <- as.factor(subset_data_MCA[[taxonomic_col]])

# Convert trait columns to factors (since MCA requires categorical data)
subset_data_MCA[trait_cols] <- lapply(subset_data_MCA[trait_cols], as.factor)

# Run MCA (excluding the taxonomic group initially)
mca_result <- MCA(subset_data_MCA[, trait_cols], graph = FALSE)

# Get the contribution scores for variables
trait_contrib <- data.frame(mca_result$var$contrib)

# Select the top 10 most influential traits (modify as needed)
top_traits <- rownames(trait_contrib)[order(-trait_contrib$Dim.1)[1:10]]

# Plot MCA biplot with only the most influential traits labeled
fviz_mca_biplot(mca_result, 
                label = "var",  # Show variable names
                select.var = list(name = top_traits),  # Show only top traits
                habillage = subset_data_MCA[[taxonomic_col]],  # Color by taxonomic group
                addEllipses = TRUE, 
                repel = TRUE)
### might be struggling with lots of zeros and sparse data fro some columns


## Try with jsut overarching categories
# Load dataset
subset_data_MCA2<- overlap_species_threats_no2 %>% select(c(12,16,20,25,29,34,39,43,47,54,61,65))

# Select taxonomic group and trait columns
taxonomic_col3 <- "taxonomic_group"  # Adjust based on actual column name
trait_cols3 <- setdiff(names(subset_data_MCA2), taxonomic_col3)  # Assuming all other columns are traits

# Convert taxonomic group to a factor
subset_data_MCA2[[taxonomic_col3]] <- as.factor(subset_data_MCA2[[taxonomic_col3]])

# Convert trait columns to factors (since MCA requires categorical data)
subset_data_MCA2[trait_cols3] <- lapply(subset_data_MCA2[trait_cols3], as.factor)

# Run MCA (excluding the taxonomic group initially)
mca_result <- MCA(subset_data_MCA2[, trait_cols3], graph = FALSE)

# Get the contribution scores for variables
trait_contrib <- data.frame(mca_result$var$contrib)

# Select the top 10 most influential traits (modify as needed)
top_traits <- rownames(trait_contrib)[order(-trait_contrib$Dim.1)[1:10]]

# Plot MCA biplot with only the most influential traits labeled
fviz_mca_biplot(mca_result, 
                label = "var",  # Show variable names
                select.var = list(name = top_traits),  # Show only top traits
                habillage = subset_data_MCA2[[taxonomic_col3]],  # Color by taxonomic group
                addEllipses = TRUE, 
                repel = TRUE)
fviz_screeplot(mca_result, addlabels = TRUE)


###########################################################################
### Check correlations prior to modelling and change some columns to factors

library(corrplot)

model_subset <- overlap_species_threats_no2 %>% select(72,73,74,6,12,78,79,80,81,82,83,85)

numeric_vars <- model_subset %>%
  select(Total_area_km, Percent_SAR_caribou, Percent_caribou_SAR, sara_status, taxonomic_group, row_98_sim, row_98_cat, row_98_sim_cat, row_98_over,row_98_sim_over, T_group) %>%
  mutate(
  sara_status = as.numeric(as.factor(sara_status)),  # Convert factors to numeric
  taxonomic_group = as.numeric(as.factor(taxonomic_group)),
  T_group = as.numeric(as.factor(T_group)),
  Total_area_km = as.numeric(Total_area_km),
  Percent_SAR_caribou = as.numeric(Percent_SAR_caribou),
  Percent_caribou_SAR = as.numeric(Percent_caribou_SAR)
  )

correlation_matrix <- cor(numeric_vars, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")

# Print the correlation matrix
print(correlation_matrix)

####################################################################################
# Run some glms looking at descriptors of threat overlap with caribou
library(car)
library(stats)
library(gtools)
library(MuMIn)
library(betareg)


overlap_species_threats_no2$sara_status <- as.factor(overlap_species_threats_no2$sara_status)
overlap_species_threats_no2$taxonomic_group <- as.factor(overlap_species_threats_no2$taxonomic_group)
overlap_species_threats_no2$Percent_SAR_caribou <- as.numeric(overlap_species_threats_no2$Percent_SAR_caribou)
overlap_species_threats_no2$Total_area_km <- as.numeric(overlap_species_threats_no2$Total_area_km)


# First delete row 98 (woodland caribou)
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  filter(row_number() != 98)

# Make reference level for the variable taxonomic group
overlap_species_threats_no2$taxonomic_group <- factor(overlap_species_threats_no2$taxonomic_group, 
                                        levels = c("Molluscs", "Arthropods", "Birds", "Lichens", "Amphibians",
                                        "Mammals_(terrestrial)", "Mosses", "Reptiles", "Vascular_Plants"))
overlap_species_threats_no2$taxonomic_group <- relevel(overlap_species_threats_no2$taxonomic_group, ref = "Arthropods")

# Try some basic linear models
n1 <- lm(row_98_sim~ 1, data=overlap_species_threats_no2)
n2 <- lm(row_98_sim~ scale(Percent_SAR_caribou), data=overlap_species_threats_no2)
n3 <- lm(row_98_sim~ scale(log(Total_area_km)), data=overlap_species_threats_no2)
n4 <- lm(row_98_sim~ sara_status, data=overlap_species_threats_no2)
n5 <- lm(row_98_sim~ taxonomic_group, data=overlap_species_threats_no2)
n6 <- lm(row_98_sim~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + sara_status + taxonomic_group, data=overlap_species_threats_no2)

vif(n6)

fmList<-model.sel(n1=n1,n2=n2,n3=n3,n4=n4,n5=n5,n6=n6)
fmList

# Other options
a1 <- lm(logit(row_98_sim)~ scale(Percent_SAR_caribou), data=overlap_species_threats_no2)
a2 <- lm(logit(row_98_sim)~ scale(log(Total_area_km)), data=overlap_species_threats_no2)
a3 <- lm(logit(row_98_sim)~ sara_status, data=overlap_species_threats_no2)
a4 <- lm(logit(row_98_sim)~ taxonomic_group, data=overlap_species_threats_no2)
a5 <- lm(logit(row_98_sim)~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + sara_status + taxonomic_group, 
         data=overlap_species_threats_no2)
a6 <- lm(logit(row_98_sim)~ 1, data=overlap_species_threats_no2)

fmList2<-model.sel(a1=a1,a2=a2,a3=a3,a4=a4,a5=a5,a6=a6)
fmList2
summary(a4)

b1 <- glm(round(cbind(row_98_sim, 1)*100)~ Percent_SAR_caribou, data=overlap_species_threats_no2, family=binomial)
b2 <- glm(round(cbind(row_98_sim, 1)*100)~ scale(Total_area_km), data=overlap_species_threats_no2,family=binomial)
b3 <- glm(round(cbind(row_98_sim, 1)*100)~ sara_status, data=overlap_species_threats_no2,family=binomial)
b4 <- glm(round(cbind(row_98_sim, 1)*100)~ taxonomic_group, data=overlap_species_threats_no2,family=binomial)
b5 <- glm(round(cbind(row_98_sim, 1)*100)~ Percent_SAR_caribou + scale(Total_area_km) + sara_status + taxonomic_group,data=overlap_species_threats_no2, family=binomial)
b6 <- glm(round(cbind(row_98_sim, 1)*100)~ 1, data=overlap_species_threats_no2, family=binomial)

fmList3<-model.sel(b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6)
fmList3
summary(b5)

# Try betareg
# First transform be response variables so they're not close to 0 or 1
overlap_species_threats_no2 <- overlap_species_threats_no2 %>%
  mutate(Trans_row_98_sim = (overlap_species_threats_no2$row_98_sim * (nrow(overlap_species_threats_no2) - 1) + 0.5)/(nrow(overlap_species_threats_no2)))

be1 <- betareg(Trans_row_98_sim ~ scale(Percent_SAR_caribou), data=overlap_species_threats_no2, link = "logit")
be2 <- betareg(Trans_row_98_sim ~ scale(log(Total_area_km)), data=overlap_species_threats_no2, link = "logit")
be3 <- betareg(Trans_row_98_sim ~ taxonomic_group, data=overlap_species_threats_no2, link = "logit")
be4 <- betareg(Trans_row_98_sim ~ 1, data=overlap_species_threats_no2, link = "logit")
be5 <- betareg(Trans_row_98_sim ~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + taxonomic_group,data=overlap_species_threats_no2, link = "logit")
#be5 <- betareg(Trans_row_98_sim ~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + sara_status + taxonomic_group,data=overlap_species_threats_no2, link = "logit")
#be3 <- betareg(Trans_row_98_sim ~ sara_status, data=overlap_species_threats_no2, link = "logit")


fmList4<-model.sel(be1=be1, be2=be2, be3=be3, be4=be4, be5=be5)
fmList4
top_models <- subset(fmList4, delta <= 2)
modelav <-model.avg(top_models)
summary(modelav)

# Look into variance inflation factors
vif(be5)
df_resid <- be5$df.residual
df_resid

# try quasibinomial
model_qb <- glm(row_98_sim~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + sara_status + taxonomic_group, 
                data=overlap_species_threats_no2,
                family = quasibinomial(link = "logit"))
model_qb
summary(model_qb)

# Proportion of deviance explained
null_dev <- summary()$null.deviance
resid_dev <- summary(a2)$deviance
prop_deviance_explained <- (null_dev - resid_dev) / null_dev
cat("Proportion of Deviance Explained: ", prop_deviance_explained, "\n")

# Dispersion parameter
dispersion <- resid_dev / df.residual(a2)
cat("Dispersion Parameter: ", dispersion, "\n")

# Residual Diagnostics
par(mfrow = c(1, 2)) # Plot side by side
plot(fitted(be5), residuals(be7), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals(be5))
qqline(residuals(be5), col = "blue")

hist(overlap_species_threats_no2$row_98_sim, breaks = 20, main = "Distribution of Y")

#################################################################################
## Plot things!

# Plot model averaged coefficient estimates of best fitting model
# Make plot of model averaged proficients estimates (all estimates and confidence intervals were computed first in excel)
# Extract coefficients and standard errors from the model
# update when best model found
model_summary <- summary(modelav)  
# Convert coefficients to a data frame
coefficients <- as.data.frame(model_summary$coefmat.full)

# Check column names to ensure correct reference
print(colnames(coefficients))

# Create a new data frame for plotting
comp3 <- data.frame(
  Variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  Std_Error = coefficients[, "Std. Error"],
  Lower = coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"],  # 95% CI lower bound
  Upper = coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"]   # 95% CI upper bound
)

# Rename categories
comp3 <- comp3 %>%
  mutate(Variable = as.character(Variable)) %>%
  mutate(Variable = dplyr::recode(Variable, 
                         "taxonomic_groupArthropods" = "Arthropods",
                         "taxonomic_groupBirds" = "Birds",
                         "taxonomic_groupLichens" = "Lichens",
                         "taxonomic_groupAmphibians" = "Amphibians",
                         "taxonomic_groupMammals_(terrestrial)" = "Mammals",
                         "taxonomic_groupReptiles" = "Reptiles",
                         "taxonomic_groupVascular_Plants" = "Vascular plants",
                         "taxonomic_groupMolluscs" = "Molluscs",
                         "taxonomic_groupMosses" = "Mosses",
                         "scale(log(Total_area_km))" = "Total range area",
                         "scale(Percent_SAR_caribou)" = "Percent range overlap",
                          "(Intercept)"="Intercept"))


# Specify the desired order of categories
desired_order5 <- c("Molluscs",
                    "Amphibians",
                    "Mammals",
                    "Reptiles",
                    "Birds",
                    "Vascular plants",
                    "Arthropods",
                    "Lichens",
                    "Mosses",
                    "Total range area",
                    "Percent range overlap",
                    "Intercept")
 
# Reorder the factor levels in the data frame
comp3$Variable <- factor(comp3$Variable, levels = desired_order5)
comp4 <-na.omit(comp3)

ggplot(comp4,aes(x=Variable,y=Estimate)) +
  geom_hline(yintercept = 0, col = "darkgrey") +  # Horizontal line at y = 0
  geom_point() +
  geom_errorbar(aes(ymin=Lower,ymax=Upper)) +
  coord_flip() +
  labs(x= "Variable", y= "Model averaged coefficent estimate") +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 12),  # Increase overall font size
    axis.title = element_text(size = 14),  # Axis titles
    axis.text = element_text(size = 12))  # Axis text


#############################################################################################
## Plot SAR taxonomic group and caribou range overlap
# Make a graph that shows which taxonomic groups of SAR overlap most with caribou ranges
# take all rows we have spatial data for
summary_data_overlap_start <- all_species_threats_no2 %>%
  filter(Total_area_km >0)

# Calculate the number of species of the 461 we have spatial data for with overlap values of >0
ex2 <- summary_data_overlap_start %>%
  filter(.[[73]] > 0) %>% nrow
ex2

# Calculate the number of species of the 461 we have spatial data for with overlap values of >20
ex3 <- summary_data_overlap_start %>%
  filter(.[[73]] > 20) %>% nrow
ex3

# summarise the number and percent of SAR that exist and overlap with caribou
summary_data_overlap <- summary_data_overlap_start %>%
  group_by(taxonomic_group) %>%
  summarise(
    total_species = n(),
    overlap_nonzero = sum(Percent_SAR_caribou > 0, na.rm = TRUE),
    overlap_above_20 = sum(Percent_SAR_caribou > 20, na.rm = TRUE)) %>%
  pivot_longer(cols = c(total_species, overlap_nonzero, overlap_above_20), 
               names_to = "Category", values_to = "Count") %>%
  mutate(Percentage = case_when(
    taxonomic_group == "Amphibians" ~ (Count / 24) * 100,
    taxonomic_group == "Arthropods" ~ (Count / 57) * 100,
    taxonomic_group == "Birds" ~ (Count / 84) * 100,
    taxonomic_group == "Lichens" ~ (Count / 18) * 100,
    taxonomic_group == "Mammals_(terrestrial)" ~ (Count / 28) * 100,
    taxonomic_group == "Molluscs" ~ (Count / 12) * 100,
    taxonomic_group == "Mosses" ~ (Count / 16) * 100,
    taxonomic_group == "Reptiles" ~ (Count / 37) * 100,
    taxonomic_group == "Vascular_Plants" ~ (Count / 185) * 100,
    TRUE ~ NA_real_  # Default case (if needed)
  ))

summary_data_overlap <- summary_data_overlap %>%
  mutate(taxonomic_group = as.character(taxonomic_group)) %>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Vascular_Plants" = "Vascular plants",))%>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Mammals_(terrestrial)" = "Terrestrial mammals"))

# Specify the desired order of categories
desired_order4 <- c("Vascular plants", "Birds", "Arthropods", "Reptiles", "Amphibians", "Terrestrial mammals",
                    "Lichens", "Molluscs", "Mosses")

# Reorder the factor levels in the data frame
summary_data_overlap$taxonomic_group <- factor(summary_data_overlap$taxonomic_group, levels = desired_order4)# as no spatial data for

# Create grouped bar plot of number of species
ggplot(summary_data_overlap, aes(x = taxonomic_group, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Taxonomic group",
       y = "Number of species",
       fill = "Metric") +
  scale_fill_manual(values = c("cadetblue2", "cyan3", "dodgerblue"),
                    labels = c("SAR overlapping with caribou >20%", "SAR overlapping with caribou", "SAR")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 15),  # Increase overall font size
    axis.title = element_text(size = 16),  # Axis titles
    axis.text = element_text(size = 15),  # Axis text
  )

# Create grouped bar plot of percentages
# Filter out 100 percentages
summary_data_overlap_percent <- summary_data_overlap %>%
  filter(Category != "total_species")

ggplot(summary_data_overlap_percent, aes(x = taxonomic_group, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", width= 0.6) +
  labs(x = "Taxonomic group",
       y = "Percentage of species/taxonomic group",
       fill = "Metric") +
  scale_fill_manual(values = c("cadetblue2", "cyan3"),
                    labels = c("SAR overlapping with caribou >20%", "SAR overlapping with caribou", "SAR")) +
  theme_minimal() +
  ylim(0, 50) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 15),  # Increase overall font size
    axis.title = element_text(size = 16),  # Axis titles
    axis.text = element_text(size = 15),  # Axis text
  )

#########################################################################################################
### What are the most common threats to SAR in Canada?
# How do these vary from the species within the caribou range, and those listed for caribou?
# Use subset with just overarching categories subset_data_over and overlapping with caribou overlap_species_threats_no2

subset_data_caribou_overlap <- overlap_species_threats_no2 %>% select(16,20,25,29,34,39,43,47,54,61,65)
subset_cat_title <- all_species_threats_no2 %>% select(16,20,25,29,34,39,43,47,54,61,65)

summed_threats_caribou <- subset_data_caribou_overlap %>% 
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Threat", values_to = "Summed_Species_cara") 

summed_threats_over <- subset_cat_title %>% 
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Threat", values_to = "Summed_species_all")

Threats_both <- summed_threats_over %>%
  mutate(Summed_species_cara = summed_threats_caribou$Summed_Species_cara)

## Calculate proportions of all species
Threats_both <- Threats_both %>%
  mutate(Proportion_all = (Summed_species_all/616)*100)

## Calculate proportions of caribou overlapping species
Threats_both <- Threats_both %>%
  mutate(Proportion_cara = (Summed_species_cara/97)*100)

Threats_both2 <- Threats_both %>% 
  pivot_longer(cols = starts_with("Proportion"), names_to = "type", values_to = "Proportion")

# Rename categories
Threats_both2 <- Threats_both2 %>%
  mutate(Threat = dplyr::recode(Threat, 
                        "X2_agri_aqua" = "Agriculture and aquaculture",
                        "X5_bio_resource_use" = "Biological resource use",
                        "X11_climate_change" = "Climate change",
                         "X3_energy" = "Energy production and mining",
                         "X10_geological_event" = "Geological events",
                         "X6_human_disturbance" = "Human disturbance",
                         "X8_invasive_species" = "Invasive and problematic species",
                         "X7_natural_sys_change" = "Natural system modifications",
                         "X9_pollution" = "Pollution",
                         "X1_res_comm_development" = "Residential and commercial development",
                         "X4_transport_service" = "Transport and service corridors"))

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
  "Geological events"
)

# Reorder the factor levels in the data frame
Threats_both2$Threat <- factor(Threats_both2$Threat, levels = desired_order3)

# Define custom colors
custom_colors <- c(
  'Proportion_all' = 'dodgerblue',  # Dark teal color for 'wolverine'
  'Proportion_cara' = 'cadetblue2'  # Light blue color for 'woodland caribou'
)

# Create grouped bar plot for threats to all SAR and SAR within caribou range
ggplot(Threats_both2, aes(x = Threat, y = Proportion, fill=type, width=0.45)) +
  geom_bar(stat = "identity", position='dodge') +
  labs(x = "Threat",
       y = "Proportion of species") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  ylim(0, 100) +
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

library(ggplot2)
# Create  bar plot for threats to all SAR
threats_all_SAR <- Threats_both2 %>%
  filter(type == "Proportion_all")

ggplot(threats_all_SAR, aes(x = Threat, y = Proportion, width=0.45)) +
  geom_bar(stat = "identity", fill = "deepskyblue4" ) +
  labs(x = "Threat",
       y = "Proportion of species") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 0)),
    axis.text.x = element_text(angle = 65, hjust = 1, margin = margin(t = 0, r = 0, b = 20, l = 0)),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 18),  # Increase overall font size
    axis.title = element_text(size = 16),  # Axis titles
    axis.text = element_text(size = 18),  # Axis text
    legend.position = "none"
  )

###graph the number of species with values of the jaccard index
# Summarize the number of values in each range
summary <- overlap_species_threats_no2 %>%
  mutate(bin = cut(row_98_sim, breaks = seq(0, 1, by = 0.1), right = FALSE)) %>%
  group_by(bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!is.na(bin)) %>%
  bind_rows(
  tibble(
    bin = c("[0.7,0.8)","[0.8,0.9)", "[0.9,1)"),
    count = c(0, 0, 0)
  )
)

ggplot(summary, aes(x = bin, y = count, width=0.45)) +
  geom_bar(stat = "identity", fill = "deepskyblue4" ) +
  labs(x = "Jaccard index",
       y = "Number of species") +
  theme_minimal() +
  ylim(0, 40) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 0)),
    axis.text.x = element_text(angle = 65, hjust = 1, margin = margin(t = 0, r = 0, b = 20, l = 0)),
    panel.background = element_rect(fill = 'white', colour = 'darkgrey'),
    panel.grid.major = element_line(colour = 'white'),
    text = element_text(size = 18),  # Increase overall font size
    axis.title = element_text(size = 16),  # Axis titles
    axis.text = element_text(size = 18),  # Axis text
    legend.position = "none"
  )

###### Make a heatmap figure for overarching categories
# Load your dataset

heat <- overlap_species_threats_no2  # Replace with actual dataset
###Create subset with just overarching categories
heat_subset_data_over <- heat %>% select(12,16,20,25,29,34,39,43,47,54,61,65)
# Assuming the dataset has 'Taxonomic_Group' and binary trait columns
taxonomic_col <- "taxonomic_group"  # Adjust as needed
trait_cols <- setdiff(names(heat_subset_data_over), taxonomic_col)

# Convert to long format and calculate proportions
df_long <- heat_subset_data_over %>%
  pivot_longer(cols = all_of(trait_cols), names_to = "Trait", values_to = "Presence") %>%
  group_by(taxonomic_group, Trait) %>%
  summarise(Proportion = mean(Presence), .groups = "drop") 

df_long <- df_long %>%
  mutate(Trait = dplyr::recode(Trait, 
                                "X2_agri_aqua" = "Agriculture and aquaculture",
                                "X5_bio_resource_use" = "Biological resource use",
                                "X11_climate_change" = "Climate change",
                                "X3_energy" = "Energy production and mining",
                                "X10_geological_event" = "Geological events",
                                "X6_human_disturbance" = "Human disturbance",
                                "X8_invasive_species" = "Invasive and problematic species",
                                "X7_natural_sys_change" = "Natural system modifications",
                                "X9_pollution" = "Pollution",
                                "X1_res_comm_development" = "Residential and commercial development",
                                "X4_transport_service" = "Transport and service corridors"))

df_long <- df_long %>%
  mutate(taxonomic_group = as.character(taxonomic_group)) %>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Vascular_Plants" = "Vascular plants",))%>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Mammals_(terrestrial)" = "Terrestrial mammals"))%>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Molluscs" = "Terrestrial molluscs"))

library(forcats)
df_long <- df_long %>%
  mutate(taxonomic_group = fct_relevel(taxonomic_group, "Terrestrial molluscs", "Amphibians", "Terrestrial mammals", "Reptiles","Birds","Vascular plants", "Lichens","Mosses","Arthropods"))

ggplot(df_long, aes(x = Trait, y = taxonomic_group, fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", name = "Proportion") +
  theme_minimal() +
  labs(x = "Trait", y = "Taxonomic Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###### Make a heatmap figure for sub categories
# Load your dataset

heat2 <- overlap_species_threats_no2  # Replace with actual dataset
###Create subset with just overarching categories
heat_subset_data_over2 <- heat2 %>% select(12,17,18,19,21,22,23,24,26,27,28,30,31,32,33,35,36,37,38,40,41,42,44,45,46,
                                         48,49,50,51,52,53,55,56,57,58,59,60,62,63,64,66,67,68,69,70)
# Assuming the dataset has 'Taxonomic_Group' and binary trait columns
taxonomic_col2 <- "taxonomic_group"  # Adjust as needed
trait_cols2 <- setdiff(names(heat_subset_data_over2), taxonomic_col2)

# Convert to long format and calculate proportions
df_long2 <- heat_subset_data_over2 %>%
  pivot_longer(cols = all_of(trait_cols2), names_to = "Trait", values_to = "Presence") %>%
  group_by(taxonomic_group, Trait) %>%
  summarise(Proportion = mean(Presence), .groups = "drop") 

df_long2 <- df_long2 %>%
  mutate(Trait = dplyr::recode(Trait, 
                               "X10_1_volcano" = "10.1 Volcanic events",
                               "X10_2_earth_quake" = "10.2 Earthquake/tsunami events",
                               "X10_3_avalanche" = "10.3 Avalances/landside events",
                               "X11_1_hab_shift" = "11.1 Climate change caused habitat shifts",
                               "X11_2_drought" = "11.2 Climate change caused droughts",
                               "X11_3_temp_extreme" = "11.3 Climate change caused temperature extremes",
                               "X11_4_storm_flood" = "11.4 Climate change caused storms and flooding",
                               "X11_5_other_impacts" = "11.5 Other impacts of climate change",
                               "X1_1_house_urban" = "1.1 Housing and urban development",
                               "X1_2_comm_industrial" = "1.2 Commercial and industrial development",
                               "X1_3_tourisim_rec" = "1.2 Tourisim and recreational development",
                               "X2_1_crops" = "2.1 Annual and perennial crops",
                               "X2_2_wood_planta" = "2.2 Wood and pulp plantations",
                               "X2_3_livestock_farm" = "2.3 Livestock farming",
                               "X2_4_aqua" = "2.4 Marine and freshwater aquaculture",
                               "X3_1_oil_gas" = "3.1 Oil and gas drilling",
                               "X3_2_mining" = "3.2 Mining and quarrying",
                               "X3_3_renewable" = "3.3 Renewable energy development",
                               "X4_1_roads" = "4.1 Roads",
                               "X4_2_utility_service" = "4.2 Utility and service corridors",
                               "X4_3_shipping_lane" = "4.3 Shipping lanes",
                               "X4_4_flight_path" = "4.4 Flight paths",
                               "X5_1_hunting" = "5.1 Hunting and collection of terrestial animals",
                               "X5_2_gathering" = "5.2 Hunting and collection of terrestrial plants",
                               "X5_3_logging" = "5.3 Logging and wood harvesting",
                               "X5_4_fishing" = "5.4 Fishing and harvesting of aquatic resources",
                               "X6_1_recreation" = "6.1 Recreational activities",
                               "X6_2_war" = "6.2 War and civil unrest",
                               "X6_3_work_activity" = "6.3 Work activities",
                               "X7_1_fire_fire_sup" = "7.1 Fire and fire suppression",
                                "X7_2_dams_water" = "7.2 Dam and water management/use",
                                "X7_3_ecosysmod" = "7.3 Other ecosystem modifications",
                                "X8_1_invasive_nonative" = "8.1 Invasive species",
                                "X8_2_problematic_native" = "8.2 Problematic species",
                                "X8_3_introduced_genetic" = "8.3 Introduced genetic material",
                                "X8_4_problematic_species" = "8.4 Problematic species of unknown origin",
                                "X8_5_viral_disease" = "8.5 Viral induced diseases",
                               "X8_6_disease_unknown" = "8.6 Diseases of unknown cause",
                               "X9_1_sewage_waste_water" = "9.1 Domestic and urban waste water",
                               "X9_2_industrial_waste" = "9.2 Industrial and military effluents",
                               "X9_3_agricultural_waste" = "9.3 Agricultural and forestry effluents",
                               "X9_4_garbage_soil_waste" = "9.4 Garbage and solid waste",
                               "X9_5_air_boure_pollute" = "9.5 Air-bourne pollutants",
                               "X9_6_excess_energy" = "9.6 Excess energy"))


df_long2 <- df_long2 %>%
  mutate(taxonomic_group = as.character(taxonomic_group)) %>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Vascular_Plants" = "Vascular plants",))%>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Mammals_(terrestrial)" = "Terrestrial mammals"))%>% 
  mutate(taxonomic_group = dplyr::recode(taxonomic_group, "Molluscs" = "Terrestrial molluscs"))

df_long2 <- df_long2 %>%
  mutate(taxonomic_group = fct_relevel(taxonomic_group, "Terrestrial molluscs", "Amphibians", "Terrestrial mammals", "Reptiles","Birds","Vascular plants", "Lichens","Mosses","Arthropods"))

# Specify the desired order of categories
desired_order6 <- c(
  "1.1 Housing and urban development",
  "1.2 Commercial and industrial development",
  "1.2 Tourisim and recreational development",
  "2.1 Annual and perennial crops",
  "2.2 Wood and pulp plantations",
  "2.3 Livestock farming",
  "2.4 Marine and freshwater aquaculture",
  "3.1 Oil and gas drilling",
  "3.2 Mining and quarrying",
  "3.3 Renewable energy development",
  "4.1 Roads",
  "4.2 Utility and service corridors",
  "4.3 Shipping lanes",
  "4.4 Flight paths",
  "5.1 Hunting and collection of terrestial animals",
  "5.2 Hunting and collection of terrestrial plants",
  "5.3 Logging and wood harvesting",
  "5.4 Fishing and harvesting of aquatic resources",
  "6.1 Recreational activities",
  "6.2 War and civil unrest",
  "6.3 Work activities",
  "7.1 Fire and fire suppression",
  "7.2 Dam and water management/use",
  "7.3 Other ecosystem modifications",
  "8.1 Invasive species",
  "8.2 Problematic species",
  "8.3 Introduced genetic material",
  "8.4 Problematic species of unknown origin",
  "8.5 Viral induced diseases",
  "8.6 Diseases of unknown cause",
  "9.1 Domestic and urban waste water",
  "9.2 Industrial and military effluents",
  "9.3 Agricultural and forestry effluents",
  "9.4 Garbage and solid waste",
  "9.5 Air-bourne pollutants",
  "9.6 Excess energy",
  "10.1 Volcanic events",
  "10.2 Earthquake/tsunami events",
  "10.3 Avalances/landside events",
  "11.1 Climate change caused habitat shifts",
  "11.2 Climate change caused droughts",
  "11.3 Climate change caused temperature extremes",
  "11.4 Climate change caused storms and flooding",
  "11.5 Other impacts of climate change")

# Reorder the factor levels in the data frame
df_long2$Trait <- factor(df_long2$Trait, levels = desired_order6)

ggplot(df_long2, aes(x = Trait, y = taxonomic_group, fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", name = "Proportion") +
  theme_minimal() +
  labs(x = "Trait", y = "Taxonomic Group") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

