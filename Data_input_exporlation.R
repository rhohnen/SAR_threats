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
jaccard_0.48 <- overlap_species_threats_no2 %>%
  filter(row_98_sim > 0.48)

# Delete threat columns
jaccard_0.48 <- jaccard_0.48 %>%
  select(-c(16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
            41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,
            67,68,69,70))

# find 10ish species most dissimilar
jaccard_0.30 <- overlap_species_threats_no2 %>%
  filter(row_98_sim < 0.30)

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
library("FactoMineR")
library("factoextra")

###Create subset for MCA. This subset has the sub groups but not the group categories
subset_data_MCA<- overlap_species_threats_no2 %>% select(c(17,18,19,21,22,23,24,26,27,28,30,31,32,33,35,36,37,38,40,41,42,44,45,46,
                                                           48,49,50,51,52,53,55,56,57,58,59,60,62,63,64,66,67,68,69,70))
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
taxons <- as.factor(overlap_species_threats_no2$taxonomic_group)
fviz_mca_ind(MCA, habillage = taxons, addEllipses = FALSE)

# Group by overlap with caribou
overlap <- as.numeric(overlap_species_threats_no2$Percent_SAR_caribou)
fviz_mca_ind(
  MCA, 
  geom = "point", 
  col.ind = overlap,  # Use numeric vector for coloring
  gradient.cols = c("blue", "yellow", "red"),  # Continuous gradient
  addEllipses = FALSE
) +
  labs(color = "Overlap") # Adjust legend label

# Group by total range of caribou
range <- as.numeric(overlap_species_threats_no2$Total_area_km)
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

### Run another MCA but just using the group categories, not the subcategories

### Create subset for MCA. This subset has the sub groups but not the group categories
subset_data_MCA2<- overlap_species_threats_no2 %>% select(16,20,25,29,34,39,43,47,54,61,65)
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
overlap_species_threats_no2$taxonomic_group <- as.factor(overlap_species_threats_no2$taxonomic_group)
fviz_mca_ind(MCA2, habillage = overlap_species_threats_no2$taxonomic_group, 
             addEllipses = FALSE)

# external grouping variable here intersect with caribou range
fviz_mca_ind(
  MCA2, 
  geom = "point", 
  col.ind = overlap_species_threats_no2$Intersect_caribou_km, # Use Area for coloring
  addEllipses = FALSE
) +
  labs(color = "Area") # Label for the color legend

# external grouping variable here intersect with area
fviz_mca_ind(
  MCA2, 
  geom = "point", 
  col.ind = overlap_species_threats_no2$Total_area_km, # Use Area for coloring
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
be3 <- betareg(Trans_row_98_sim ~ sara_status, data=overlap_species_threats_no2, link = "logit")
be4 <- betareg(Trans_row_98_sim ~ taxonomic_group, data=overlap_species_threats_no2, link = "logit")
be5 <- betareg(Trans_row_98_sim ~ scale(Percent_SAR_caribou) + scale(log(Total_area_km)) + sara_status + taxonomic_group,data=overlap_species_threats_no2, link = "logit")
be6 <- betareg(Trans_row_98_sim ~ 1, data=overlap_species_threats_no2, link = "logit")

fmList4<-model.sel(be1=be1, be2=be2, be3=be3, be4=be4, be5=be5, be6=be6)
fmList4
summary(be4)
vif(be5)

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
plot(fitted(be4), residuals(be4), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals(be4))
qqline(residuals(be4), col = "blue")

hist(overlap_species_threats_no2$row_98_sim, breaks = 20, main = "Distribution of Y")

#################################################################################
## Plot things!

# Plot model averaged coefficient estimates of best fitting model
# Make plot of model averaged proficients estimates (all estimates and confidence intervals were computed first in excel)
# Extract coefficients and standard errors from the model
# update when best mdoel found
model_summary <- summary(b5)  
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
# How do these vary from the species within the caribou range, and those listed for caribou?
# First make data set
# Filter rows where percent_caribou > 0

# Replace spaces with underscores in a specific column (taxonomic_group)
threats_all <- threats %>%
  mutate(taxonomic_group = gsub(" ", "_", taxonomic_group))

# Keep only the most recent year_published for each common_name
threats_all <- threats_all %>%
  group_by(species) %>%
  filter(year_published == max(year_published)) %>%
  ungroup()

# Delete multiples of the same species that weren't eliminated with the process above
# Keep the row with the greatest number of threats listed per species
# To do this first count the number of traits (sum of 1s across trait columns)

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
  mutate(Proportion_all = (Summed_Species_all/586)*100)

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

library(ggplot2)
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
    text = element_text(size = 20),  # Increase overall font size
    axis.title = element_text(size = 22),  # Axis titles
    axis.text = element_text(size = 20),  # Axis text
    legend.position = "none"
  )

###graph the number of species with values of the jaccard index
# Summarize the number of values in each range
summary <- overlap_caribou_threats_single %>%
  mutate(bin = cut(row_17_sim, breaks = seq(0, 1, by = 0.1), right = FALSE)) %>%
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
    text = element_text(size = 20),  # Increase overall font size
    axis.title = element_text(size = 22),  # Axis titles
    axis.text = element_text(size = 20),  # Axis text
    legend.position = "none"
  )

