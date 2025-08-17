library(car)
library(dplyr)
library(readxl)
#import data
Kenya_agri_disease_spatial <- read_excel("Kenya_agri_disease_spatial.xls")

crop_recommendation <- read_excel("crop_recommendation.xlsx")

# LOGISTIC REGRESSION (Predict high_yield)
#create an outcome variable with 1 representing event of interest
Animal_data$outcome <- ifelse(Animal_data$high_yield == "High", 1, 0)

log_mod <- glm(outcome~body_weight_kg, data = Animal_data, family = binomial(link = "logit"))
# Model summary
summary(log_mod)

# Predict probability of "High" on the same dataset
Animal_data$prob_high <- predict(log_mod, newdata = Animal_data, type = "response")

# Predict class (cutoff = 0.5)
Animal_data$pred_class <- ifelse(Animal_data$prob_high >= 0.5, "High", "Low")

# View first few predictions
head(Animal_data[, c("high_yield", "prob_high", "pred_class")])

#Logistic regression with many predictors
log_mod <- glm(outcome~feed_intake_kg+age_months+body_weight_kg+pasture_quality_index+
                 lactation_days+herd_size+temp_c+humidity_pct+protein_pct_feed+
                 energy_density_mjkg+vet_visits_per_year+parasite_index, data = Animal_data, family = binomial(link = "logit"))
# Model summary
summary(log_mod)

# Predict probability of "High" on the same dataset
Animal_data$prob_high <- predict(log_mod, newdata = Animal_data, type = "response")

# Predict class (cutoff = 0.5)
Animal_data$pred_class <- ifelse(Animal_data$prob_high >= 0.5, "High", "Low")

# View first few predictions
head(Animal_data[, c("high_yield", "prob_high", "pred_class")])