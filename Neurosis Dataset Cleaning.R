library(tidyverse)
#F431301 TARITH ALAWATTA
data <- read_csv("Data_Coursework_DATA.csv")

# ==========================================
# SUMMARY STATISTICS (MEASURES OF CENT TEND)
# ==========================================

data %>%
  summarise(across(c(Height, Weight), 
                   list(Mean = mean, Median = median, Min = min, Max = max))) %>%
  print()

# =========================
# Z-SCORE OUTLIER DETECTION
# =========================

data <- data %>%
  mutate(Height_Z = (Height - mean(Height)) / sd(Height))

z_outliers <- data %>% filter(abs(Height_Z) > 3)

if (nrow(z_outliers) == 0) {
  cat("No outliers detected. All Z-scores are within +/- 3.\n")
  cat("Max Z-score observed:", max(abs(data$Height_Z)), "\n")
} else {
  print(z_outliers)
}

# ================
# HEIGHT BY GENDER
# ================

data %>%
  group_by(Gender) %>%
  summarise(Mean_H = mean(Height), SD_H = sd(Height)) %>%
  print()

# =================================
# AGE DISTRIBUTION PLOT (HISTOGRAM)
# =================================

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Figure 1: Distribution of Age",
       x = "Age (Years)", y = "Frequency")

# ==========================
# PROBABILITIES (BINOM TEST)
# ==========================

p_male <- mean(data$Gender == "Male")
binom_p <- binom.test(sum(data$Gender == "Male"), nrow(data), p = 0.5)$p.value
cat("P(Male):", p_male, "\nBinomial p-value:", binom_p, "\n")

p_a30 <- mean(data$Age > 30)
p_joint <- mean(data$Gender == "Male" & data$Age > 30)
cat("P(Male) * P(Age > 30):", p_male * p_a30, "\n")
cat("Actual P(Male & Age > 30):", p_joint, "\n")

# =============================
# DOSE-RESPONSE (PEARSON COEFF)
# =============================

cat("Pearson r:", cor(data$Dosage, data$C_Performance), "\n")


ggplot(data, aes(x = Dosage, y = C_Performance)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Figure 2: Dose-Response (Cognitive)",
       x = "Dosage (ml)", y = "Cognitive Performance Score")

# =========================
# SENSITIVITY & SPECIFICITY
# =========================

tab <- table(data$Disease, data$Test_Result)
print(tab)
sens <- tab["Yes", "Positive"] / sum(tab["Yes", ])
spec <- tab["No", "Negative"] / sum(tab["No", ])
cat("Sensitivity:", round(sens, 3), "\nSpecificity:", round(spec, 3), "\n")

# ==========================================================
# SUBSET ANALYSIS (SCATTERP,REGRESSION MOD, REGRESSION PLOT)
# ==========================================================

an_data <- data %>% filter(Selected == 1)

# SCATTERPLOT 
ggplot(an_data, aes(x = Dosage, y = P_Performance)) +
  geom_point(size = 3, color = "darkred") +
  theme_minimal() +
  labs(title = "Figure 3: Physical Performance vs Dosage (Subset)",
       x = "Dosage (ml)", y = "Physical Performance (min)")

# REGRESSION MODEL
model <- lm(P_Performance ~ Dosage, data = an_data)
cat("INTERCEPT:", coef(model)[1], "\n")
cat("SLOPE:", coef(model)[2], "\n")
cat("R-SQUARED:", summary(model)$r.squared, "\n")


an_data %>%
  summarise(n = n(), Sum_X = sum(Dosage), Sum_Y = sum(P_Performance), 
            Sum_XY = sum(Dosage * P_Performance), Sum_X2 = sum(Dosage^2)) %>%
  print()

# REGRESSION PLOT 
ggplot(an_data, aes(x = Dosage, y = P_Performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Figure 4: Regression Line - Dosage vs Physical Performance",
       subtitle = paste("R-squared =", round(summary(model)$r.squared, 3)),
       x = "Dosage (ml)", y = "Physical Performance (min)")