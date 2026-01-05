# ==========================================
# PRACTICAL 9 – CHI SQUARE TEST
# Dataset : flipkard.csv
# ==========================================

df <- read.csv("flipkard.csv", na.strings = c("", "NA", " "))

# ------------------------------------------
# 1. remove identifier columns automatically
# ------------------------------------------
bad_cols <- c("id","ID","product_id","order_id","user_id","serial","sr_no")
df <- df[, !(names(df) %in% bad_cols)]

# ------------------------------------------
# 2. detect categorical columns
# ------------------------------------------
cat_cols <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]

# if not enough categorical columns, create them
if(length(cat_cols) < 2){
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  
  # create two categorical groups from numeric data
  df$Group1 <- cut(df[[num_cols[1]]], breaks = 3,
                   labels = c("Low","Medium","High"))
  
  df$Group2 <- cut(df[[num_cols[1]]], breaks = 2,
                   labels = c("Small","Large"))
  
  var1 <- "Group1"
  var2 <- "Group2"
  
} else {
  
  # choose two categorical with small number of levels
  small <- sapply(df[cat_cols], function(x) length(unique(x)))
  small <- sort(small)
  
  var1 <- names(small)[1]
  var2 <- names(small)[2]
  
  df[[var1]] <- as.factor(df[[var1]])
  df[[var2]] <- as.factor(df[[var2]])
}

# ------------------------------------------
# 3. create contingency table safely
# ------------------------------------------
tab <- table(df[[var1]], df[[var2]])

# remove empty rows/columns
tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]

# ------------------------------------------
# 4. perform chi-square test
# ------------------------------------------
chi_result <- chisq.test(tab)

cat("\nVARIABLE 1:", var1)
cat("\nVARIABLE 2:", var2, "\n")

cat("\n--- CONTINGENCY TABLE ---\n")
print(tab)

cat("\n--- CHI-SQUARE RESULT ---\n")
print(chi_result)

# p-value only
cat("\nP-VALUE =", chi_result$p.value, "\n")

# Interpretation
if(chi_result$p.value < 0.05){
  cat("\nConclusion: Significant association → REJECT H0\n")
} else {
  cat("\nConclusion: No significant association → ACCEPT H0\n")
}
