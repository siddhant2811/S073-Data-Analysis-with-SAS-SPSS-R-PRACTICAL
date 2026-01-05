# ================================
# PRACTICAL 7 – One Way ANOVA
# Dataset : Data.csv
# ================================

# Load dataset
df <- read.csv("Data.csv")

# -------------------------------
# STEP 1: Identify numeric column
# -------------------------------
num_cols <- names(df)[sapply(df, is.numeric)]

if (length(num_cols) == 0) {
  stop("No numeric column found in dataset")
}

numeric_var <- num_cols[1]      # use first numeric column

# -------------------------------
# STEP 2: Identify categorical column
# -------------------------------
cat_cols <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]

if (length(cat_cols) == 0) {
  # Create grouping if missing
  df$Group <- cut(df[[numeric_var]],
                  breaks = 3,
                  labels = c("Low", "Medium", "High"))
  group_var <- "Group"
} else {
  group_var <- cat_cols[1]
  df[[group_var]] <- as.factor(df[[group_var]])
}

# -------------------------------
# STEP 3: ONE-WAY ANOVA
# -------------------------------
formula_used <- as.formula(paste(numeric_var, "~", group_var))

anova_one <- aov(formula_used, data = df)

# -------------------------------
# STEP 4: DISPLAY RESULT
# -------------------------------
cat("\nNUMERIC VARIABLE USED:", numeric_var)
cat("\nGROUPING VARIABLE USED:", group_var, "\n\n")

summary(anova_one)

