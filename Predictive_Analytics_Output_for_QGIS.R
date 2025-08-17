####################################################################################
# Kenya_agri_disease: Tree, SVM, Logistic, Linear + QGIS outputs                   #
# Uses columns:                                                                    #
#  year, month, lon, lat, NDVI, Rainfall_mm, Temp_C, Humidity_pct, Elevation_m,    #
#  Pesticide_kg, SoilType, LandCover, PopDensity_km2, LivestockDensity_km2,        #
#  DistToWater_km, HealthAccess_km, CropYieldClass, MalariaRisk, BrucellosisRisk,  #
#  MalariaCases, BrucellosisCases, Outbreak_Malaria, Outbreak_Brucellosis          #
####################################################################################

# install.packages(c("tidyverse","caret","rpart","rpart.plot","e1071","pROC","sf"))
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(pROC)
library(sf)
library(readxl)

set.seed(123)


# --------------------------------------------------------------------
#  If you already have the data.frame Kenya_agri_disease in memory,
#    skip this read step. Otherwise, read from Excel/CSV as appropriate.
# --------------------------------------------------------------------
# Example (uncomment and adjust path if needed):

Kenya_agri_disease <- read_excel("Kenya_agri_disease_spatial.xlsx")

# Coerce types safely -------------------------
# Factors (categorical)
factor_cols <- c("SoilType", "LandCover", "CropYieldClass",
                 "Outbreak_Malaria", "Outbreak_Brucellosis")
for (nm in factor_cols) {
  if (nm %in% names(Kenya_agri_disease)) {
    Kenya_agri_disease[[nm]] <- as.factor(Kenya_agri_disease[[nm]])
  }
}

# Numeric predictors (ensure numeric)
num_cols <- c("NDVI","Rainfall_mm","Temp_C","Humidity_pct","Elevation_m",
              "Pesticide_kg","PopDensity_km2","LivestockDensity_km2",
              "DistToWater_km","HealthAccess_km","MalariaRisk","BrucellosisRisk",
              "MalariaCases","BrucellosisCases","year","month","lon","lat")
for (nm in num_cols) {
  if (nm %in% names(Kenya_agri_disease)) {
    Kenya_agri_disease[[nm]] <- suppressWarnings(as.numeric(Kenya_agri_disease[[nm]]))
  }
}

# Common predictor set for models (edit if needed)
pred_vars <- c("NDVI","Rainfall_mm","Temp_C","Humidity_pct","Elevation_m",
               "Pesticide_kg","SoilType","LandCover","PopDensity_km2",
               "LivestockDensity_km2","DistToWater_km","HealthAccess_km")

# Keep rows complete for these predictors
base_df <- Kenya_agri_disease %>%
  filter(if_all(all_of(pred_vars), ~ !is.na(.x)))


# CLASSIFICATION: CropYieldClass (robust; SVM with one-hot) 
if ("CropYieldClass" %in% names(base_df)) {
  clf_df <- base_df %>% filter(!is.na(CropYieldClass))
  if (nrow(clf_df) > 1 && nlevels(clf_df$CropYieldClass) >= 2) {
    
    # Predictors used
    pred_vars <- c("NDVI","Rainfall_mm","Temp_C","Humidity_pct","Elevation_m",
                   "Pesticide_kg","SoilType","LandCover","PopDensity_km2",
                   "LivestockDensity_km2","DistToWater_km","HealthAccess_km")
    factor_predictors <- c("SoilType","LandCover")
    
    # Stratified split
    idx_clf   <- caret::createDataPartition(clf_df$CropYieldClass, p = 0.7, list = FALSE)
    train_clf <- droplevels(clf_df[idx_clf, ])
    test_clf  <- droplevels(clf_df[-idx_clf, ])
    
    # Align target levels in TEST to TRAIN
    test_clf$CropYieldClass <- factor(test_clf$CropYieldClass,
                                      levels = levels(train_clf$CropYieldClass))
    
    # Decision Tree path (unchanged, but with complete-case mask) 
    keep_train_tree <- complete.cases(train_clf[, c(pred_vars, "CropYieldClass"), drop = FALSE])
    keep_test_tree  <- complete.cases(test_clf[,  c(pred_vars, "CropYieldClass"), drop = FALSE])
    if (any(!keep_train_tree)) message("Tree: dropped ", sum(!keep_train_tree), " TRAIN rows (NA).")
    if (any(!keep_test_tree))  message("Tree: dropped ", sum(!keep_test_tree),  " TEST rows (NA).")
    
    train_tree <- droplevels(train_clf[keep_train_tree, , drop = FALSE])
    test_tree  <- droplevels(test_clf[keep_test_tree,  , drop = FALSE])
    
    tree_formula <- as.formula(
      paste0("CropYieldClass ~ ", paste(pred_vars, collapse = " + "))
    )
    
    tree_fit <- rpart::rpart(
      formula = tree_formula,
      data    = train_tree,
      method  = "class",
      control = rpart::rpart.control(cp = 0.01, minsplit = 20)
    )
    tree_pred_test <- predict(tree_fit, newdata = test_tree, type = "class")
    stopifnot(length(tree_pred_test) == nrow(test_tree))
    cat("\n--- Decision Tree (CropYieldClass, TEST) ---\n")
    print(caret::confusionMatrix(tree_pred_test, test_tree$CropYieldClass))
    
    # SVM path with one-hot encoding (stable) ----------------
    # 1) Ensure factors in TRAIN are factors (template levels)
    for (nm in factor_predictors) {
      if (nm %in% names(train_clf)) train_clf[[nm]] <- as.factor(train_clf[[nm]])
    }
    # Coerce TEST factor levels to TRAIN levels (unseen -> NA)
    for (nm in factor_predictors) {
      if (nm %in% names(test_clf) && is.factor(train_clf[[nm]])) {
        test_clf[[nm]] <- factor(test_clf[[nm]], levels = levels(train_clf[[nm]]))
      }
    }
    
    # Build TRAIN design matrix (template) and drop NAs consistently
    svm_train_df <- train_clf[, c("CropYieldClass", pred_vars), drop = FALSE]
    cc_train <- complete.cases(svm_train_df)
    if (any(!cc_train)) message("SVM: dropped ", sum(!cc_train), " TRAIN rows (NA).")
    svm_train_df <- droplevels(svm_train_df[cc_train, , drop = FALSE])
    
    # One-hot encode TRAIN (no intercept)
    svm_formula_x <- as.formula(paste0("~ ", paste(pred_vars, collapse = " + "), " - 1"))
    X_train <- model.matrix(svm_formula_x, data = svm_train_df)
    y_train <- droplevels(svm_train_df$CropYieldClass)
    
    # 4) Build TEST design matrix using TRAIN template
    svm_test_df <- test_clf[, c("CropYieldClass", pred_vars), drop = FALSE]
    cc_test <- complete.cases(svm_test_df)
    if (any(!cc_test)) message("SVM: dropped ", sum(!cc_test), " TEST rows (NA).")
    svm_test_df <- droplevels(svm_test_df[cc_test, , drop = FALSE])
    
    # Apply same factor levels to TEST again (safety)
    for (nm in factor_predictors) {
      if (nm %in% names(svm_test_df) && is.factor(train_clf[[nm]])) {
        svm_test_df[[nm]] <- factor(svm_test_df[[nm]], levels = levels(train_clf[[nm]]))
      }
    }
    
    X_test <- model.matrix(svm_formula_x, data = svm_test_df)
    
    # 5) Force TEST columns to match TRAIN (add missing as zeros; reorder)
    missing_in_test <- setdiff(colnames(X_train), colnames(X_test))
    if (length(missing_in_test) > 0) {
      X_test <- cbind(
        X_test,
        matrix(0, nrow = nrow(X_test), ncol = length(missing_in_test),
               dimnames = list(NULL, missing_in_test))
      )
    }
    # Also drop any extra columns that appear only in TEST
    X_test <- X_test[, colnames(X_train), drop = FALSE]
    y_test <- svm_test_df$CropYieldClass
    
    # Fit SVM on numeric matrix; predict on aligned TEST matrix
    svm_fit <- e1071::svm(
      x = X_train, y = y_train,
      kernel = "radial",
      scale = TRUE,
      probability = TRUE
    )
    svm_pred_test <- predict(svm_fit, newdata = X_test)
    stopifnot(length(svm_pred_test) == length(y_test))  # now guaranteed
    
    cat("\n--- SVM radial (CropYieldClass, TEST) ---\n")
    print(caret::confusionMatrix(svm_pred_test, y_test))
    
    # Optional ROC if binary
    if (nlevels(y_train) == 2) {
      pos_lab <- levels(y_train)[2]
      svm_prob_mat <- attr(predict(svm_fit, newdata = X_test, probability = TRUE), "probabilities")
      probs_pos <- svm_prob_mat[, pos_lab]
      roc_svm <- pROC::roc(response = y_test, predictor = probs_pos,
                           levels = levels(y_train), direction = "<")
      cat(sprintf("SVM AUC (CropYieldClass, TEST): %.3f\n", as.numeric(pROC::auc(roc_svm))))
    }
    
    # FULL-DATA predictions for QGIS (using the same template) 
    clf_full <- clf_df
    # align factors on full data to TRAIN levels
    for (nm in factor_predictors) {
      if (nm %in% names(clf_full) && is.factor(train_clf[[nm]])) {
        clf_full[[nm]] <- factor(clf_full[[nm]], levels = levels(train_clf[[nm]]))
      }
    }
    # TREE full predictions (mask by complete cases)
    keep_full_tree <- complete.cases(clf_full[, pred_vars, drop = FALSE])
    clf_full$tree_cropyield_pred <- NA
    if (any(keep_full_tree)) {
      clf_full$tree_cropyield_pred[keep_full_tree] <- predict(tree_fit,
                                                              newdata = clf_full[keep_full_tree, ], type = "class")
    }
    
    # SVM full predictions: build matrix with TRAIN template
    full_df <- clf_full[, pred_vars, drop = FALSE]
    cc_full <- complete.cases(full_df)
    if (any(!cc_full)) message("SVM full: ", sum(!cc_full), " rows lack predictors; predictions set to NA.")
    X_full <- model.matrix(svm_formula_x, data = clf_full)
    # Add missing columns and reorder
    miss_full <- setdiff(colnames(X_train), colnames(X_full))
    if (length(miss_full) > 0) {
      X_full <- cbind(X_full,
                      matrix(0, nrow = nrow(X_full), ncol = length(miss_full),
                             dimnames = list(NULL, miss_full))
      )
    }
    X_full <- X_full[, colnames(X_train), drop = FALSE]
    
    clf_full$svm_cropyield_pred     <- NA
    clf_full$svm_cropyield_prob_pos <- NA_real_
    if (any(cc_full)) {
      p_lbl <- predict(svm_fit, newdata = X_full[cc_full, , drop = FALSE])
      clf_full$svm_cropyield_pred[cc_full] <- p_lbl
      if (nlevels(y_train) == 2) {
        p_prob <- attr(predict(svm_fit, newdata = X_full[cc_full, , drop = FALSE], probability = TRUE),
                       "probabilities")[, pos_lab]
        clf_full$svm_cropyield_prob_pos[cc_full] <- p_prob
      }
    }
    
  } else {
    clf_full <- base_df
    clf_full$tree_cropyield_pred <- NA
    clf_full$svm_cropyield_pred  <- NA
    clf_full$svm_cropyield_prob_pos <- NA_real_
    warning("CropYieldClass not suitable for classification (insufficient rows or levels).")
  }
} else {
  clf_full <- base_df
  clf_full$tree_cropyield_pred <- NA
  clf_full$svm_cropyield_pred  <- NA
  clf_full$svm_cropyield_prob_pos <- NA_real_
  warning("CropYieldClass column not found; skipping CropYieldClass classification.")
}



# LOGISTIC: Outbreaks (binary) 
# Helper to fit/evaluate logistic on a binary target
fit_logistic <- function(df, target, predictors) {
  df2 <- df %>% filter(!is.na(.data[[target]]) & if_all(all_of(predictors), ~ !is.na(.x)))
  if (nrow(df2) < 2 || nlevels(df2[[target]]) != 2) return(NULL)
  df2[[target]] <- factor(df2[[target]])
  idx <- createDataPartition(df2[[target]], p = 0.7, list = FALSE)
  tr <- df2[idx, ]; te <- df2[-idx, ]
  fml <- as.formula(paste0(target, " ~ ", paste(predictors, collapse = " + ")))
  mod <- glm(fml, data = tr, family = binomial(link = "logit"))
  probs <- predict(mod, newdata = te, type = "response")
  pos_lab <- levels(tr[[target]])[2]
  pred  <- factor(ifelse(probs >= 0.5, pos_lab, levels(tr[[target]])[1]),
                  levels = levels(tr[[target]]))
  cm <- confusionMatrix(pred, te[[target]])
  roc_obj <- roc(response = te[[target]], predictor = probs,
                 levels = levels(tr[[target]]), direction = "<")
  list(model = mod, cm = cm, auc = as.numeric(auc(roc_obj)))
}

# Logistic for CropYieldClass IF binary
logit_cropyield <- NULL
if ("CropYieldClass" %in% names(base_df) && nlevels(base_df$CropYieldClass) == 2) {
  cat("\n--- Logistic (CropYieldClass, TEST) ---\n")
  logit_cropyield <- fit_logistic(base_df, "CropYieldClass", pred_vars)
  if (!is.null(logit_cropyield)) {
    print(logit_cropyield$cm$table)
    cat(sprintf("Accuracy: %.3f | AUC: %.3f\n",
                logit_cropyield$cm$overall["Accuracy"], logit_cropyield$auc))
  }
}

# Logistic for Outbreak_Malaria
logit_malaria <- NULL
if ("Outbreak_Malaria" %in% names(base_df)) {
  cat("\n--- Logistic (Outbreak_Malaria, TEST) ---\n")
  logit_malaria <- fit_logistic(base_df, "Outbreak_Malaria", pred_vars)
  if (!is.null(logit_malaria)) {
    print(logit_malaria$cm$table)
    cat(sprintf("Accuracy: %.3f | AUC: %.3f\n",
                logit_malaria$cm$overall["Accuracy"], logit_malaria$auc))
  }
}

# Logistic for Outbreak_Brucellosis
logit_bruc <- NULL
if ("Outbreak_Brucellosis" %in% names(base_df)) {
  cat("\n--- Logistic (Outbreak_Brucellosis, TEST) ---\n")
  logit_bruc <- fit_logistic(base_df, "Outbreak_Brucellosis", pred_vars)
  if (!is.null(logit_bruc)) {
    print(logit_bruc$cm$table)
    cat(sprintf("Accuracy: %.3f | AUC: %.3f\n",
                logit_bruc$cm$overall["Accuracy"], logit_bruc$auc))
  }
}


# Add FULL-DATA logistic predictions to clf_full (QGIS)
# (Only if models were successfully trained)
if (!is.null(logit_malaria)) {
  clf_full$logit_outbreak_malaria_prob <- predict(logit_malaria$model, newdata = clf_full, type = "response")
  pos_lab <- levels(model.frame(logit_malaria$model)[[1]])[2]
  clf_full$logit_outbreak_malaria_pred <- factor(
    ifelse(clf_full$logit_outbreak_malaria_prob >= 0.5, pos_lab, levels(model.frame(logit_malaria$model)[[1]])[1]),
    levels = levels(model.frame(logit_malaria$model)[[1]])
  )
} else {
  clf_full$logit_outbreak_malaria_prob <- NA_real_
  clf_full$logit_outbreak_malaria_pred <- NA
}

if (!is.null(logit_bruc)) {
  clf_full$logit_outbreak_bruc_prob <- predict(logit_bruc$model, newdata = clf_full, type = "response")
  pos_lab <- levels(model.frame(logit_bruc$model)[[1]])[2]
  clf_full$logit_outbreak_bruc_pred <- factor(
    ifelse(clf_full$logit_outbreak_bruc_prob >= 0.5, pos_lab, levels(model.frame(logit_bruc$model)[[1]])[1]),
    levels = levels(model.frame(logit_bruc$model)[[1]])
  )
} else {
  clf_full$logit_outbreak_bruc_prob <- NA_real_
  clf_full$logit_outbreak_bruc_pred <- NA
}

# If binary CropYieldClass logistic was trained, add full predictions
if (!is.null(logit_cropyield)) {
  clf_full$logit_cropyield_prob <- predict(logit_cropyield$model, newdata = clf_full, type = "response")
  pos_lab <- levels(model.frame(logit_cropyield$model)[[1]])[2]
  clf_full$logit_cropyield_pred <- factor(
    ifelse(clf_full$logit_cropyield_prob >= 0.5, pos_lab, levels(model.frame(logit_cropyield$model)[[1]])[1]),
    levels = levels(model.frame(logit_cropyield$model)[[1]])
  )
} else {
  clf_full$logit_cropyield_prob <- NA_real_
  clf_full$logit_cropyield_pred <- NA
}


# MULTIPLE LINEAR REGRESSION 
# For MalariaCases and BrucellosisCases (numeric). (Poisson may be better; using linear as requested.)
regress_and_score <- function(df, target, predictors) {
  df2 <- df %>% filter(!is.na(.data[[target]]) & if_all(all_of(predictors), ~ !is.na(.x)))
  if (nrow(df2) < 3) return(NULL)
  idx <- createDataPartition(df2[[target]], p = 0.7, list = FALSE)
  tr <- df2[idx, ]; te <- df2[-idx, ]
  fml <- as.formula(paste0(target, " ~ ", paste(predictors, collapse = " + ")))
  fit <- lm(fml, data = tr)
  yhat <- predict(fit, newdata = te)
  y    <- te[[target]]
  rmse <- sqrt(mean((y - yhat)^2))
  mae  <- mean(abs(y - yhat))
  r2   <- 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
  list(model = fit, rmse = rmse, mae = mae, r2 = r2)
}

lin_mal <- if ("MalariaCases" %in% names(base_df)) regress_and_score(base_df, "MalariaCases", pred_vars) else NULL
if (!is.null(lin_mal)) cat(sprintf("\nLinear (MalariaCases, TEST): RMSE=%.3f MAE=%.3f R2=%.3f\n", lin_mal$rmse, lin_mal$mae, lin_mal$r2))
lin_bru <- if ("BrucellosisCases" %in% names(base_df)) regress_and_score(base_df, "BrucellosisCases", pred_vars) else NULL
if (!is.null(lin_bru)) cat(sprintf("Linear (BrucellosisCases, TEST): RMSE=%.3f MAE=%.3f R2=%.3f\n", lin_bru$rmse, lin_bru$mae, lin_bru$r2))

# Add FULL-DATA regression predictions to clf_full (QGIS)
# Add FULL-DATA regression predictions to clf_full (for QGIS)
if (!is.null(lin_mal)) {
  cols <- intersect(pred_vars, names(clf_full))           # only use cols that exist
  ok   <- stats::complete.cases(clf_full[, cols, drop=FALSE])
  
  clf_full$malaria_cases_pred <- NA_real_
  clf_full$malaria_cases_pred[ok] <-
    predict(lin_mal$model, newdata = clf_full[ok, cols, drop=FALSE])
} else {
  clf_full$malaria_cases_pred <- NA_real_
}

# EXPORTS FOR QGIS 
# 5A) CSV with all predictions
out_csv <- clf_full %>%
  select(any_of(c(
    # geometry
    "lon","lat",
    # original cols
    names(Kenya_agri_disease),
    # predictions
    "tree_cropyield_pred","svm_cropyield_pred","svm_cropyield_prob_pos",
    "logit_cropyield_pred","logit_cropyield_prob",
    "logit_outbreak_malaria_pred","logit_outbreak_malaria_prob",
    "logit_outbreak_bruc_pred","logit_outbreak_bruc_prob",
    "malaria_cases_pred","brucellosis_cases_pred"
  ))) %>%
  # ensure predicted columns exist even if NA (if some models were skipped)
  mutate(
    logit_cropyield_prob = if (!"logit_cropyield_prob" %in% names(.)) NA_real_ else logit_cropyield_prob,
    logit_outbreak_malaria_prob = if (!"logit_outbreak_malaria_prob" %in% names(.)) NA_real_ else logit_outbreak_malaria_prob,
    logit_outbreak_bruc_prob    = if (!"logit_outbreak_bruc_prob" %in% names(.)) NA_real_ else logit_outbreak_bruc_prob
  )

write.csv(out_csv, "kenya_agri_predictions.csv", row.names = FALSE)
cat('\nSaved: "kenya_agri_predictions.csv"\n')

# 5B) GeoPackage (points; WGS84)
if (all(c("lon","lat") %in% names(clf_full))) {
  gpkg <- clf_full %>%
    filter(!is.na(lon), !is.na(lat)) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  st_write(gpkg, "kenya_agri_predictions.gpkg", delete_layer = TRUE, quiet = TRUE)
  cat('Saved: "kenya_agri_predictions.gpkg" (WGS84). Load in QGIS.\n')
} else {
  cat("lon/lat not found — skipped GPKG export.\n")
}

cat("\nQGIS styling tips:\n- Use tree_cropyield_pred / svm_cropyield_pred for class symbology\n- Use *_prob columns for graduated colors (risk/probability)\n- Use malaria_cases_pred / brucellosis_cases_pred for numeric mapping\n")

