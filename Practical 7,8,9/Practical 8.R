# ======================================
# PRACTICAL 8 – TWO WAY ANOVA
# Dataset: Hakcathon.csv
# ======================================

df <- read.csv("Hakcathon.csv")

# 1. pick a numeric variable as dependent variable
num_cols <- names(df)[sapply(df, is.numeric)]

# avoid id column
num_cols <- num_cols[num_cols != "id"]

# use first suitable numeric variable
y <- num_cols[1]

# 2. create two factor variables (this guarantees valid ANOVA)
df$Factor_A <- cut(df[[y]],
                   breaks = 2,
                   labels = c("Low","High"))

df$Factor_B <- cut(df[[y]],
                   breaks = 3,
                   labels = c("Low","Medium","High"))

# 3. two-way ANOVA model
model <- aov(df[[y]] ~ Factor_A * Factor_B, data = df)

cat("\nDEPENDENT VARIABLE:", y, "\n")

# 4. full ANOVA table
print(summary(model))

# 5. extract p-values safely
tab <- summary(model)[[1]]

pvals <- tab[,"Pr(>F)"]

cat("\nP VALUES ONLY:\n")
print(pvals)

# 6. automatic interpretation
if(any(pvals < 0.05)){
  cat("\nResult: Significant effect present → REJECT H0\n")
} else {
  cat("\nResult: No significant effect present → ACCEPT H0\n")
}

