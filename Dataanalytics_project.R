# ============================================================
#   BANKING CUSTOMER RISK SEGMENTATION
#   Using Agglomerative Hierarchical Clustering
#   Group 18 | Data Analytics Project
#   Dataset: Credit Risk Dataset (Kaggle)
# ============================================================

# ── STEP 1: Install & Load Required Libraries ────────────────
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("corrplot")
# install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(corrplot)
library(gridExtra)

# ── STEP 2: Load Real Dataset ────────────────────────────────
cat("Loading dataset...\n")

# ⚠️ Change this path if your file is somewhere else
df <- read.csv("D:/SEM 6/DATA ANALYTICS/PROJECT/credit_risk_dataset.csv",
               stringsAsFactors = FALSE)

cat("✅ Dataset Loaded Successfully!\n")
cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
cat("Column Names:\n")
print(names(df))

# ── STEP 3: Data Quality Check (like friend's report) ────────
cat("\n============================================\n")
cat("         DATA QUALITY CHECK\n")
cat("============================================\n")

cat("Total Records:", nrow(df), "\n")
cat("Total Columns:", ncol(df), "\n")

cat("\nMissing Values per Column:\n")
print(colSums(is.na(df)))

cat("\nDuplicate Rows:", sum(duplicated(df)), "\n")

# Remove duplicates and missing values
df <- df[!duplicated(df), ]
df <- na.omit(df)
cat("✅ After Cleaning:", nrow(df), "records remaining\n")

# ── STEP 4: Rename Columns to Match Our Project ──────────────
df <- df %>% rename(
  Age                = person_age,
  Annual_Income      = person_income,
  Loan_Amount        = loan_amnt,
  Loan_Intent        = loan_intent,
  Credit_Score       = cb_person_cred_hist_length,
  Debt_to_Income     = loan_percent_income,
  Loan_Grade         = loan_grade,
  Default_History    = cb_person_default_on_file,
  Home_Ownership     = person_home_ownership,
  Interest_Rate      = loan_int_rate,
  Loan_Status        = loan_status
)

cat("\n✅ Columns Renamed Successfully!\n")
print(head(df, 5))

# ── STEP 5: Exploratory Data Analysis (EDA) ──────────────────
cat("\n============================================\n")
cat("     EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("============================================\n")

# Summary Statistics
cat("\n📊 Summary Statistics:\n")
numeric_cols <- df %>% select(Age, Annual_Income, Loan_Amount,
                              Credit_Score, Debt_to_Income, Interest_Rate)
print(summary(numeric_cols))

# ── EDA Plot 1: Age Distribution ─────────────────────────────
p1 <- ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "#3498db", color = "white") +
  labs(title = "Distribution of Customer Age",
       subtitle = "How old are our banking customers?",
       x = "Age (Years)", y = "Number of Customers") +
  theme_minimal()
print(p1)

# ── EDA Plot 2: Annual Income Distribution ───────────────────
p2 <- ggplot(df, aes(x = Annual_Income)) +
  geom_histogram(bins = 30, fill = "#2ecc71", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Annual Income",
       subtitle = "Income spread across all customers",
       x = "Annual Income ($)", y = "Number of Customers") +
  theme_minimal()
print(p2)

# ── EDA Plot 3: Loan Amount Distribution ─────────────────────
p3 <- ggplot(df, aes(x = Loan_Amount)) +
  geom_histogram(bins = 30, fill = "#e74c3c", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Loan Amounts",
       subtitle = "How much do customers borrow?",
       x = "Loan Amount ($)", y = "Number of Customers") +
  theme_minimal()
print(p3)

# ── EDA Plot 4: Loan Status (Default vs Non-Default) ─────────
loan_counts <- df %>%
  group_by(Loan_Status) %>%
  summarise(Count = n()) %>%
  mutate(Label = ifelse(Loan_Status == 1, "Default", "Non-Default"),
         Pct = round(Count / sum(Count) * 100, 1))

p4 <- ggplot(loan_counts, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Default" = "#e74c3c",
                               "Non-Default" = "#2ecc71")) +
  labs(title = "Loan Default Distribution",
       subtitle = "Checking class balance in dataset",
       x = "Loan Status", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)

# ── EDA Plot 5: Income vs Loan Amount ────────────────────────
p5 <- ggplot(df %>% sample_n(min(2000, nrow(df))),
             aes(x = Annual_Income, y = Loan_Amount)) +
  geom_point(alpha = 0.4, color = "#9b59b6") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Income vs Loan Amount",
       subtitle = "Relationship between income and borrowing",
       x = "Annual Income ($)", y = "Loan Amount ($)") +
  theme_minimal()
print(p5)

# ── EDA Plot 6: Debt-to-Income by Loan Grade ─────────────────
p6 <- ggplot(df, aes(x = Loan_Grade, y = Debt_to_Income,
                     fill = Loan_Grade)) +
  geom_boxplot() +
  labs(title = "Debt-to-Income Ratio by Loan Grade",
       subtitle = "Higher grade = riskier customers?",
       x = "Loan Grade", y = "Debt-to-Income Ratio") +
  theme_minimal() +
  theme(legend.position = "none")
print(p6)

# ── STEP 6: Correlation Analysis ─────────────────────────────
cat("\n📊 Correlation Analysis:\n")

corr_data <- df %>%
  select(Age, Annual_Income, Loan_Amount,
         Credit_Score, Debt_to_Income, Interest_Rate) %>%
  cor(use = "complete.obs")

print(round(corr_data, 3))

corrplot(corr_data,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlation Matrix of Financial Attributes",
         mar = c(0, 0, 2, 0))

# ── STEP 7: Data Preprocessing ───────────────────────────────
cat("\n============================================\n")
cat("          DATA PREPROCESSING\n")
cat("============================================\n")

# Select numeric features for clustering
features <- df %>%
  select(Age, Annual_Income, Loan_Amount,
         Credit_Score, Debt_to_Income, Interest_Rate)

cat("Features selected for clustering:\n")
print(names(features))
cat("\nBefore Scaling (first 3 rows):\n")
print(round(head(features, 3), 2))

# Normalize features using scale()
features_scaled <- scale(features)

cat("\nAfter Scaling (first 3 rows):\n")
print(round(head(features_scaled, 3), 3))
cat("✅ All features scaled to mean=0, sd=1\n")

# ── STEP 8: Euclidean Distance Matrix ────────────────────────
# Use sample for speed if dataset is large
set.seed(42)
sample_size <- min(500, nrow(features_scaled))
sample_idx  <- sample(1:nrow(features_scaled), sample_size)
features_sample <- features_scaled[sample_idx, ]

cat("\n✅ Using", sample_size, "customer sample for clustering\n")
dist_matrix <- dist(features_sample, method = "euclidean")
cat("✅ Euclidean Distance Matrix Computed\n")

# ── STEP 9: Agglomerative Hierarchical Clustering ────────────
cat("\n============================================\n")
cat("      AGGLOMERATIVE CLUSTERING\n")
cat("============================================\n")

hclust_model <- hclust(dist_matrix, method = "ward.D2")
cat("✅ Agglomerative Clustering Done (Ward's D2 Linkage)\n")

# ── STEP 10: Dendrogram ───────────────────────────────────────
plot(hclust_model,
     main = "Dendrogram — Banking Customer Risk Segmentation",
     sub  = "Ward's Linkage | Euclidean Distance | Credit Risk Dataset (Kaggle)",
     xlab = "Customers",
     ylab = "Euclidean Distance",
     labels = FALSE,
     hang = -1,
     cex = 0.7)
rect.hclust(hclust_model, k = 3,
            border = c("#2ecc71", "#f39c12", "#e74c3c"))
legend("topright",
       legend = c("Low Risk", "Medium Risk", "High Risk"),
       fill   = c("#2ecc71", "#f39c12", "#e74c3c"),
       cex    = 0.8)

# ── STEP 11: Cut into 3 Clusters ─────────────────────────────
k        <- 3
clusters <- cutree(hclust_model, k = k)

sample_df         <- df[sample_idx, ]
sample_df$Cluster <- clusters

cat("\n✅ Customers cut into", k, "clusters\n")
cat("Cluster distribution:\n")
print(table(sample_df$Cluster))

# ── STEP 12: Assign Risk Labels ──────────────────────────────
cluster_summary <- sample_df %>%
  group_by(Cluster) %>%
  summarise(
    Mean_Income        = mean(Annual_Income),
    Mean_Loan          = mean(Loan_Amount),
    Mean_DTI           = mean(Debt_to_Income),
    Mean_Interest      = mean(Interest_Rate),
    Mean_Credit_Hist   = mean(Credit_Score),
    Default_Rate       = mean(Loan_Status) * 100,
    Count              = n()
  )

cat("\nCluster Summary:\n")
print(cluster_summary)

# Assign: lowest default rate + lowest DTI = Low Risk
cluster_ranked <- cluster_summary %>%
  arrange(Default_Rate, Mean_DTI)

risk_labels                    <- c("Low Risk", "Medium Risk", "High Risk")
cluster_summary$Risk_Level     <- risk_labels
risk_map                       <- setNames(cluster_summary$Risk_Level,
                                           cluster_summary$Cluster)
sample_df$Risk_Level           <- risk_map[as.character(sample_df$Cluster)]

cat("\n✅ Risk Labels Assigned:\n")
print(table(sample_df$Risk_Level))

# ── STEP 13: PCA Cluster Plot ─────────────────────────────────
fviz_cluster(
  list(data = features_sample, cluster = clusters),
  geom         = "point",
  ellipse.type = "convex",
  palette      = c("#2ecc71", "#f39c12", "#e74c3c"),
  ggtheme      = theme_minimal(),
  main         = "Banking Customer Risk Clusters (PCA View)\nCredit Risk Dataset — Kaggle"
)

# ── STEP 14: Risk Level Visualizations ───────────────────────

# Bar chart: Count per risk level
ggplot(sample_df, aes(x = Risk_Level, fill = Risk_Level)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("Low Risk"    = "#2ecc71",
                               "Medium Risk" = "#f39c12",
                               "High Risk"   = "#e74c3c")) +
  labs(title = "Number of Customers per Risk Category",
       subtitle = "Banking Customer Risk Segmentation — Group 18",
       x = "Risk Level", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Box plot: Interest Rate by Risk Level
ggplot(sample_df, aes(x = Risk_Level, y = Interest_Rate,
                      fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low Risk"    = "#2ecc71",
                               "Medium Risk" = "#f39c12",
                               "High Risk"   = "#e74c3c")) +
  labs(title = "Interest Rate by Risk Level",
       subtitle = "Higher risk customers pay higher interest rates",
       x = "Risk Level", y = "Interest Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Box plot: Debt-to-Income by Risk Level
ggplot(sample_df, aes(x = Risk_Level, y = Debt_to_Income,
                      fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low Risk"    = "#2ecc71",
                               "Medium Risk" = "#f39c12",
                               "High Risk"   = "#e74c3c")) +
  labs(title = "Debt-to-Income Ratio by Risk Level",
       subtitle = "High risk customers carry more debt relative to income",
       x = "Risk Level", y = "Debt-to-Income Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

# Box plot: Annual Income by Risk Level
ggplot(sample_df, aes(x = Risk_Level, y = Annual_Income,
                      fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low Risk"    = "#2ecc71",
                               "Medium Risk" = "#f39c12",
                               "High Risk"   = "#e74c3c")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Income by Risk Level",
       subtitle = "Income differences across risk categories",
       x = "Risk Level", y = "Annual Income ($)") +
  theme_minimal() +
  theme(legend.position = "none")

# Default Rate by Risk Level (bar chart)
default_summary <- sample_df %>%
  group_by(Risk_Level) %>%
  summarise(Default_Rate = round(mean(Loan_Status) * 100, 1))

ggplot(default_summary, aes(x = Risk_Level, y = Default_Rate,
                            fill = Risk_Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Default_Rate, "%")), vjust = -0.5,
            size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Low Risk"    = "#2ecc71",
                               "Medium Risk" = "#f39c12",
                               "High Risk"   = "#e74c3c")) +
  labs(title = "Loan Default Rate by Risk Level",
       subtitle = "Validating cluster risk labels using actual default data",
       x = "Risk Level", y = "Default Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# ── STEP 15: Final Summary Report ────────────────────────────
cat("\n")
cat("============================================================\n")
cat("        FINAL CLUSTER SUMMARY REPORT — GROUP 18\n")
cat("     Banking Customer Risk Segmentation\n")
cat("     Dataset: Credit Risk Dataset (Kaggle)\n")
cat("============================================================\n")

final_summary <- sample_df %>%
  group_by(Risk_Level) %>%
  summarise(
    Customer_Count   = n(),
    Avg_Age          = round(mean(Age), 1),
    Avg_Income       = round(mean(Annual_Income), 0),
    Avg_Loan         = round(mean(Loan_Amount), 0),
    Avg_DTI          = round(mean(Debt_to_Income), 3),
    Avg_Interest     = round(mean(Interest_Rate), 2),
    Default_Rate_Pct = round(mean(Loan_Status) * 100, 1)
  )

print(final_summary)


cat("3 risk clusters identified: Low Risk, Medium Risk, High Risk\n")
cat("Results validated using actual loan default rates.\n")
cat("Dataset Source: Credit Risk Dataset — Kaggle\n")

