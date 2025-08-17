# ---------- Packages ----------
library(readr)

# Load data 
Animal_data <- read_csv("Animal_data.csv")

# Outcome and predictors 
# Predict milk_yield_l from the same 
preds <- c(
  "feed_intake_kg","age_months","body_weight_kg","pasture_quality_index",
  "lactation_days","herd_size","temp_c","humidity_pct","protein_pct_feed",
  "energy_density_mjkg","vet_visits_per_year","parasite_index",
  "age_group","weight_category","pasture_rating","temperature_category"
)

# Categorical predictors
cat_vars <- c("age_group","weight_category","pasture_rating","temperature_category")

# Drop incomplete rows (outcome + predictors) 
Animal_data <- Animal_data[complete.cases(Animal_data[, c("milk_yield_l", preds)]), ]


# Train/test split (70/30) 
set.seed(42)
idx   <- sample(seq_len(nrow(Animal_data)), size = floor(0.7 * nrow(Animal_data)))
train <- Animal_data[idx, ]
test  <- Animal_data[-idx, ]

# Ensure categorical consistency between train and test 
# Trim whitespace and align factor levels; map unseen test levels to a baseline
for (v in intersect(cat_vars, names(train))) {
  train[[v]] <- trimws(as.character(train[[v]]))
  test[[v]]  <- trimws(as.character(test[[v]]))
  tr_lv <- sort(unique(train[[v]]))
  test[[v]][!(test[[v]] %in% tr_lv)] <- tr_lv[1]   # map unseen -> baseline
  train[[v]] <- factor(train[[v]], levels = tr_lv)
  test[[v]]  <- factor(test[[v]],  levels = tr_lv)
}

# Fit linear regression (NO standardization) 
form_reg <- as.formula(paste("milk_yield_l ~", paste(preds, collapse = " + ")))
lm_mod   <- lm(form_reg, data = train)
summary(lm_mod)

# Predict on test 
y_true <- test$milk_yield_l
y_hat  <- predict(lm_mod, newdata = test)

#  Metrics 
rmse <- sqrt(mean((y_true - y_hat)^2))
mae  <- mean(abs(y_true - y_hat))
r2   <- 1 - sum((y_true - y_hat)^2) / sum((y_true - mean(y_true))^2)

cat("RMSE:", round(rmse, 3), "\n")
cat("MAE :", round(mae, 3), "\n")
cat("R²  :", round(r2, 3), "\n")


# Quick diagnostic plots 
# Predicted vs Actual
plot(y_true, y_hat,
     xlab = "Actual milk_yield_l",
     ylab = "Predicted milk_yield_l",
     main = "Predicted vs Actual")
abline(0, 1, lwd = 2)

# Residuals vs Fitted
plot(fitted(lm_mod), resid(lm_mod),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2)



