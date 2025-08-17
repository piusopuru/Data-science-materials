
install.packages("car")   # Run once if needed
library(car)
library(dplyr)

Animal_data <- read.csv("Animal_data.csv", stringsAsFactors = FALSE)

#convert categorical variables into factor variables
Animal_data$high_yield <- factor(Animal_data$high_yield) 
Animal_data$age_group <- as.factor(Animal_data$age_group)
Animal_data$pasture_rating <- as.factor(Animal_data$weight_category)
Animal_data$pasture_rating <- as.factor(Animal_data$pasture_rating)
Animal_data$temperature_category <- as.factor(Animal_data$temperature_category)

# Keep only complete rows
#Animal_data <- Animal_data[complete.cases(Animal_data[, c("high_yield", preds, "milk_yield_l")]), ]


#Simple Linear regression (Predict milk yield)
lin_mod <- lm(milk_yield_l~body_weight_kg, data = Animal_data)
#model summary
summary(lin_mod)

#Multiple Linear regression (Predict milk yield)
lin_mod <- lm(milk_yield_l~body_weight_kg+temp_c, data = Animal_data)
summary(lin_mod)

#Multiple Linear regression (Predict milk yield)
lin_mod <- lm(milk_yield_l~feed_intake_kg+age_months+body_weight_kg+pasture_quality_index+
              lactation_days+herd_size+temp_c+humidity_pct+protein_pct_feed+
              energy_density_mjkg+vet_visits_per_year+parasite_index, data = Animal_data)
summary(lin_mod)


# Predict milk yield on the same dataset
Animal_data$milk_yield_pred <- predict(lin_mod, newdata = Animal_data)



