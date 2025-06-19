library(foreign)
library(tibble)
library(dplyr)
library(psych)
library(googleVis)
library(cluster)
library(factoextra)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(reshape2)
library(haven)
library(stringr)
library(broom)



# Block 1: Import SPSS files and extract selected modules with metadata
rm(list = ls())

# Define main project directory
project_path <- "C:/Users/Sergey Filipov/Desktop/Проект SHARE/0. Решение"

# Define path to .sav module files (SPSS)
modules_path <- file.path(project_path, "Modules")

# Define and create results/plots folder
results_path <- file.path(project_path, "Plots")
dir.create(results_path, showWarnings = FALSE, recursive = TRUE)

# Block 1: Import SPSS files and extract selected modules with metadata
# List all .sav files in the Modules folder
aux <- list.files(modules_path, pattern = "\\.sav$")

# Load each .sav file into a list, preserving labels
dd <- lapply(file.path(modules_path, aux), read.spss, to.data.frame = FALSE, use.value.labels = TRUE)

# Clean file names to use as list element names
aux2 <- gsub("sharew7_rel9-0-0_", "", aux)
aux2 <- gsub(".sav", "", aux2)
names(dd) <- aux2

# Define modules of interest (ac = activities, ph = physical health, gv_imputations = demographics)
nn <- c("ac", "ph", "gv_imputations")
dds <- list()   # raw module data
ddsc <- list()  # variable metadata

# Loop over selected modules to extract data and variable info
for (i in seq_along(nn)) {
  dds[[i]] <- as_tibble(dd[[nn[i]]])
  ddsc[[i]] <- data.frame(
    var = names(dds[[i]]),
    varlab = unname(attr(dd[[nn[i]]], "variable.labels")),
    varclass = sapply(dds[[i]], class),
    unqs = sapply(dds[[i]], function(x) length(unique(x))),
    nas = colSums(is.na(dds[[i]]))
  )
}

# Assign names to output lists
names(dds) <- nn
names(ddsc) <- nn



# Block 2: Clean and transform variables from AC module
# Save the country column before cleaning
country_column = dds$ac$country

# Remove unnecessary variables from the AC module
excl = c("hhid7", "mergeidp7", "coupleid7", "language",
         as.character(ddsc$ac$var[grep("ac036_|ac037_|ac038_|ac740", ddsc$ac$var)]))
ac = dds$ac[, !names(dds$ac) %in% excl]

# Convert all variables (except the first) to character
ac[,2:ncol(ac)] = sapply(ac[,2:ncol(ac)], as.character)

# Replace "Refusal" and "Don't know" with NA
AC = as.data.frame(sapply(ac, function(x) {
  ifelse(x == "Refusal" | x == "Don't know", NA, as.character(x))
}), stringsAsFactors = FALSE)

# Convert specific variable to numeric
AC$ac012_ = as.numeric(AC$ac012_)

# Remove rows with any NA values
AC = na.omit(AC)

# Prepare a numeric version of the data
ACn = AC
unqs = sapply(ACn, function(x) length(unique(x)))

# Transform binary variables
bin_vars = names(unqs[unqs == 2])
ACn[bin_vars] = sapply(ACn[bin_vars], function(x) ifelse(x == "Selected", 5, 1))

# Transform 4-level ordinal variables
q4_vars = names(unqs[unqs == 4])
ACn[q4_vars] = sapply(q4_vars, function(var) {
  x = ACn[[var]]
  ifelse(x == "Never", 1,
         ifelse(x == "Rarely", 2,
                ifelse(x == "Sometimes", 3, 4)))
})

# Transform 5-level ordinal variables
q5_vars = names(unqs[unqs == 5])
ACn[q5_vars] = sapply(q5_vars, function(var) {
  x = ACn[[var]]
  ifelse(x == "Disagree strongly", 1,
         ifelse(x == "Disagree a little", 2,
                ifelse(x == "Neither agree not disagree", 3,
                       ifelse(x == "Agree a little", 4, 5))))
})



# Block 3: Extract demographic data, filter by age and employment, and merge with activity variables
# Extract required variables for demographic information
incl = c("mergeid", "gender", "single", "mstat", "yedu", "cjs", "thinc2", "age", "country")
D = dds$gv_imputations[, incl]
D[, c("mergeid", "gender", "mstat", "cjs", "thinc2", "country")] = sapply(D[, c("mergeid", "gender", "mstat", "cjs", "thinc2", "country")], as.character)
D$age = as.numeric(D$age)

# Restrict to specific age group
DD = D %>% filter(age >= 50 & age <= 64)

# Filter: employed and self-employed
DD = DD %>% filter(cjs %in% c("Employed or self-employed", "Self-employed", "Employed"))

# Merge ACn and DD by mergeid
dd = merge(ACn, DD, by = "mergeid", all.x = TRUE)  # Ensure all rows from ACn are preserved

# Check if 'country' column exists in merged data
if (!"country" %in% colnames(dd)) {
  # If not, add it from D
  dd$country = D$country[match(dd$mergeid, D$mergeid)]
}

# Explicitly define variables used in factor analysis (12 from activities module)
fa_vars <- c("ac014_", "ac015_", "ac016_", "ac017_", "ac018_", "ac019_",
             "ac020_", "ac021_", "ac022_", "ac023_", "ac024_", "ac025_")

# Additional columns to retain, including ac012_
keep_vars <- c("mergeid", fa_vars, "ac012_", "age", "gender", "country", 
               "thinc2", "yedu", "cjs", "income_group", 
               "has_chronic", "social_active")

# Retain only relevant columns
dd <- dd[, intersect(names(dd), keep_vars)]



# Block 4: Perform factor analysis and visualize factor structure and scores
# Perform factor analysis
fa_result <- fa(dd[, fa_vars], nfactors = 2, rotate = "oblimin")

# Read original SPSS file for variable labels
ac_spss <- read_sav(file.path(modules_path, "sharew7_rel9-0-0_ac.sav"))

# Create label dictionary: variable name -> label
ac_var_labels <- setNames(
  sapply(ac_spss, function(x) attr(x, "label")),
  names(ac_spss)
)

# Extract labels for selected variables
var_labels <- sapply(fa_vars, function(x) ac_var_labels[[x]])
names(var_labels) <- fa_vars

# Truncate labels to a maximum of 50 characters
short_labels <- str_trunc(var_labels, 50)

# Visualize factor structure
op <- par(no.readonly = TRUE)

# Save factor diagram
png(file.path(results_path, "01_factor_structure.png"), width = 800, height = 600)

par(mar = c(4, 12, 4, 4))
fa.diagram(
  fa_result,
  simple = FALSE,
  labels = short_labels,
  cex = 0.85
)
par(op)
dev.off()

# Compute factor scores
MR = factor.scores(dd[, fa_vars], fa_result)$scores

# Add factors to main dataframe
dd$MR1 <- MR[, 1]
dd$MR2 <- MR[, 2]

# Plot MR1 – Positive Attitude
png(file.path(results_path, "02_MR1_positive_attitude.png"), width = 800, height = 600)
plot(density(dd$MR1), main = "Positive Attitude (MR1)", col = "blue", lwd = 2)
dev.off()

# Plot MR2 – Depressive Symptoms
png("Plots/03_MR2_depressive_symptoms.png", width = 800, height = 600)
plot(density(dd$MR2), main = "Depressive Symptoms (MR2)", col = "red", lwd = 2)
dev.off()



# Block 5: Correlation matrix between activity items and factor scores
# Compute correlation matrix between all ac0* variables and MR1/MR2
cor_matrix <- cor(dd[, grep("^ac0", names(dd))], dd[, c("MR1", "MR2")], use = "pairwise.complete.obs")
cor_df <- round(cor_matrix, 2)
cor_melt <- melt(cor_df)

# Convert variable names to character
cor_melt$Var1 <- as.character(cor_melt$Var1)

# Extract original labels from SPSS metadata
cor_melt$Var1_label <- ifelse(
  is.na(ac_var_labels[cor_melt$Var1]),
  cor_melt$Var1,
  ac_var_labels[cor_melt$Var1]
)

# Optional: manually defined translations or user-friendly descriptions (can be edited as needed)
translated_labels <- c(
  "ac012_"   = "Satisfied with life",
  "ac014_"   = "Age prevents doing desired things",
  "ac015_"   = "Feeling out of control",
  "ac016_"   = "Feeling isolated",
  "ac017_"   = "Able to do what one wants",
  "ac018_"   = "Family responsibilities interfere",
  "ac019_"   = "Running out of money",
  "ac020_"   = "Looking forward to each day",
  "ac021_"   = "Life has meaning",
  "ac022_"   = "Looking back with happiness",
  "ac023_"   = "Feeling energetic",
  "ac024_"   = "Having opportunities",
  "ac025_"   = "Expecting a good future",
  "ac035d1"  = "Volunteering activities",
  "ac035d10" = "Playing cards, chess, etc.",
  "ac035d4"  = "Attending educational course",
  "ac035d5"  = "Participating in social/sports club",
  "ac035d7"  = "Political/community engagement",
  "ac035d8"  = "Reading books/newspapers/magazines",
  "ac035d9"  = "Solving puzzles/crosswords",
  "ac035dno" = "None of the listed activities"
)

# Add translation and construct combined label: code | original → translation
cor_melt$Var1_label_translated <- translated_labels[cor_melt$Var1]
cor_melt$combined_label <- paste0(
  cor_melt$Var1, " | ",
  cor_melt$Var1_label, "  →  ",
  cor_melt$Var1_label_translated
)

# Define factor labels for display
factor_labels <- c(
  "MR1" = "Positive Attitude (MR1)",
  "MR2" = "Depressive Symptoms (MR2)"
)
cor_melt$Var2_label <- factor_labels[as.character(cor_melt$Var2)]

# Plot correlation heatmap with labels
ggplot(cor_melt, aes(x = Var2_label, y = combined_label, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(value), "NA", sprintf("%.2f", value))), size = 3.2) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Correlation") +
  theme_minimal() +
  labs(
    title = "Correlation between activity items and factor scores",
    x = "Factor",
    y = "Item (code | label → translation)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_line(color = "gray90")
  )

# Save the plot to the Plots folder
ggsave(filename = file.path(results_path, "04_item_factor_correlations.png"),
       width = 9, height = 7, dpi = 300)



# Block 6: Country Clustering Based on MR1 and MR2 Scores
# Calculate mean scores by country
country_scores <- dd %>%
  group_by(country) %>%
  summarise(
    MR1_mean = mean(MR1, na.rm = TRUE),
    MR2_mean = mean(MR2, na.rm = TRUE),
    LifeSat = mean(ac012_, na.rm = TRUE),
    HHinc = median(as.numeric(thinc2), na.rm = TRUE)
  )

# Prepare data for clustering
cluster_data <- country_scores %>%
  select(country, MR1_mean, MR2_mean) %>%
  na.omit()

# Elbow method plot for optimal number of clusters
elbow_plot <- fviz_nbclust(cluster_data[, -1], kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save Elbow plot
ggsave(filename = file.path(results_path, "05_elbow_method_kmeans.png"),
       plot = elbow_plot, width = 8, height = 5, dpi = 300)

# Perform K-means clustering with k = 4
set.seed(123)
kmeans_result <- kmeans(cluster_data[, -1], centers = 4, nstart = 25)
cluster_data$cluster <- as.factor(kmeans_result$cluster)

# Define cluster colors
cluster_colors <- c("#f44336", "#2196f3", "#4caf50", "#9c27b0")

# Scatter plot with country labels
scatter_plot <- ggplot(cluster_data, aes(x = MR1_mean, y = MR2_mean, color = cluster)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = country), size = 3.5) +
  labs(
    title = "Clusters by MR1 and MR2",
    x = "MR1: Positive Attitude",
    y = "MR2: Depressive Symptoms",
    color = "Cluster"
  ) +
  theme_light() +
  scale_color_manual(values = cluster_colors)

# Save scatter plot
ggsave(filename = file.path(results_path, "07_clusters_scatterplot.png"),
       plot = scatter_plot, width = 10, height = 6, dpi = 300)

# Generate and plot GeoChart
geo_chart <- gvisGeoChart(cluster_data, locationvar = "country", colorvar = "cluster",
                          options = list(
                            region = "150",
                            displayMode = "regions",
                            resolution = "countries",
                            datalessRegionColor = '#ffffff',
                            defaultColor = '#ffffff',
                            colorAxis = paste0("{colors:[", paste(shQuote(cluster_colors, type = "cmd"), collapse = ","), "]}")
                          ))

# Display map
plot(geo_chart)



# Block 7: Profiling Respondents by Demographics and Health
# Create age groups
dd <- dd %>%
  mutate(age_group = case_when(
    age < 55 ~ "50–54",
    age >= 55 & age < 60 ~ "55–59",
    age >= 60 ~ "60–64"
  ))

# MR2 by gender
gender_profile <- dd %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(MR2_mean = mean(MR2, na.rm = TRUE))

p_gender <- ggplot(gender_profile, aes(x = gender, y = MR2_mean, fill = gender)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(MR2_mean, 2)), vjust = -0.5) +
  labs(title = "Mean MR2 by Gender", x = "Gender", y = "MR2 (Depressive Symptoms)") +
  theme_gray() +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))

ggsave(file.path(results_path, "08_gender_MR2_profile.png"), p_gender, width = 7, height = 5, dpi = 300)

# MR2 by age group
age_profile <- dd %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(MR2_mean = mean(MR2, na.rm = TRUE))

p_age <- ggplot(age_profile, aes(x = age_group, y = MR2_mean, fill = age_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(MR2_mean, 2)), vjust = -0.5) +
  labs(title = "MR2 by Age Group", x = "Age Group", y = "MR2") +
  theme_gray() +
  scale_fill_brewer(palette = "Set2")

ggsave(file.path(results_path, "09_age_group_MR2_profile.png"), p_age, width = 7, height = 5, dpi = 300)

# Social activity and MR2
dd$social_active <- ifelse(dd$ac025_ %in% c(3, 4), 1, 0)

ggsave(file.path(results_path, "10_social_activity_MR2_boxplot.png"),
       ggplot(dd, aes(x = factor(social_active, labels = c("Inactive", "Active")), y = MR2)) +
         geom_boxplot(fill = "#1f77b4") +
         labs(title = "MR2 and Social Activity", x = "Social Activity", y = "MR2") +
         theme_gray(),
       width = 7, height = 5, dpi = 300)

# Income and MR2
dd$income_group <- cut(as.numeric(dd$thinc2),
                       breaks = quantile(as.numeric(dd$thinc2), probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Low", "Middle", "High"))

ggsave(file.path(results_path, "11_income_group_MR2_boxplot.png"),
       ggplot(dd, aes(x = income_group, y = MR2, fill = income_group)) +
         geom_boxplot() +
         labs(title = "MR2 by Income Group", x = "Income Group", y = "MR2") +
         theme_gray() +
         scale_fill_brewer(palette = "Pastel2"),
       width = 7, height = 5, dpi = 300)

# Education and MR2
dd$edu_group <- cut(as.numeric(dd$yedu),
                    breaks = quantile(as.numeric(dd$yedu), probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                    include.lowest = TRUE, labels = c("Low", "Medium", "High"))

ggsave(file.path(results_path, "12_education_MR2_boxplot.png"),
       ggplot(dd, aes(x = edu_group, y = MR2, fill = edu_group)) +
         geom_boxplot() +
         labs(title = "MR2 by Education Level", x = "Education", y = "MR2") +
         theme_gray() +
         scale_fill_brewer(palette = "Set3"),
       width = 7, height = 5, dpi = 300)

# Income + Chronic Conditions interaction
dd$risk_group <- paste(
  ifelse(is.na(dd$income_group), "No Income", as.character(dd$income_group)),
  ifelse(dd$has_chronic == 1, "with condition", "no condition")
)

ggsave(file.path(results_path, "13_income_chronic_interaction_MR2_boxplot.png"),
       ggplot(dd, aes(x = risk_group, y = MR2, fill = risk_group)) +
         geom_boxplot() +
         labs(title = "MR2 by Income and Chronic Condition", x = "Group", y = "MR2") +
         theme_gray() +
         scale_fill_brewer(palette = "Set2"),
       width = 8, height = 5, dpi = 300)

# Social Activity + Chronic Conditions interaction
dd$soc_chronic_group <- paste(
  ifelse(dd$social_active == 1, "Active", "Inactive"),
  ifelse(dd$has_chronic == 1, "with condition", "no condition")
)

ggsave(file.path(results_path, "14_social_chronic_interaction_MR2_boxplot.png"),
       ggplot(dd, aes(x = soc_chronic_group, y = MR2, fill = soc_chronic_group)) +
         geom_boxplot() +
         labs(title = "MR2 by Social Activity and Chronic Condition", x = "Group", y = "MR2") +
         theme_gray() +
         scale_fill_brewer(palette = "Pastel1"),
       width = 8, height = 5, dpi = 300)



# Block 8: Multiple Linear Regression - combined effects of all predictors
# Fit linear model: predicting MR2 from all main predictors
model <- lm(MR2 ~ social_active + income_group + edu_group + age + gender, data = dd)

# Print regression summary
summary(model)

# Tidy model results and remove rows with NA estimates
model_tidy <- broom::tidy(model, conf.int = TRUE)
model_tidy <- model_tidy[!is.na(model_tidy$estimate), ]

# Plot regression coefficients with confidence intervals
p <- ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", estimate)), vjust = -1.5, size = 4) +
  labs(
    title = "Regression Coefficients for MR2",
    x = "Effect on MR2", y = "Predictor"
  ) +
  theme_minimal()

# Save plot to results folder with white background
ggsave(filename = file.path(results_path, "15_linear_regression_coefficients.png"),
       plot = p, width = 7, height = 5, dpi = 300, bg = "white")

# Save entire R environment
save.image(file = file.path(project_path, "SHARE_analysis_full_workspace.RData"))
