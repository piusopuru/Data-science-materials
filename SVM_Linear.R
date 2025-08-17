#######################################################################
# SVM for high_yield                                                  #
# - Clean data                                                        #
# - Train/Test split (70/30)                                          #
# - 10-fold CV on TRAIN to tune cost (linear kernel)                  #
# - Evaluate on TRAIN and TEST: Confusion matrix, Accuracy, ROC/AUC.  #
# Robust to "new levels" via one-hot encoding from TRAIN template.    #
#######################################################################

# Packages 
# install.packages(c("e1071","pROC","readr"))  # run once if needed
library(e1071)
library(pROC)
library(readr)

# Load 
Data <- read_csv("Animal_data.csv")

# Ensure target is factor with "Low","High" (so High is the positive class)
Data$high_yield <- factor(Data$high_yield, levels = c("Low","High"))

# Drop rows with missing target
Data <- Data[!is.na(Data$high_yield), ]

# Train/Test split (70/30) 
set.seed(123)
n   <- nrow(Data)
idx <- sample(seq_len(n), size = floor(0.7 * n))
TrainDF <- Data[idx, ]
TestDF  <- Data[-idx, ]

# Build X/y and make encodings stable 
# Drop target for X frames
X_train_df <- subset(TrainDF, select = -high_yield)
X_test_df  <- subset(TestDF,  select = -high_yield)

# Convert character columns to factors in TRAIN (template)
char_cols <- names(X_train_df)[sapply(X_train_df, is.character)]
for (nm in char_cols) X_train_df[[nm]] <- factor(X_train_df[[nm]])

# For TEST: coerce matching character columns to factor, then align factor levels to TRAIN
for (nm in intersect(char_cols, names(X_test_df))) {
  if (is.character(X_test_df[[nm]])) X_test_df[[nm]] <- factor(X_test_df[[nm]])
}
for (nm in names(X_train_df)[sapply(X_train_df, is.factor)]) {
  if (nm %in% names(X_test_df) && is.factor(X_test_df[[nm]])) {
    X_test_df[[nm]] <- factor(X_test_df[[nm]], levels = levels(X_train_df[[nm]]))
  }
}

# Drop rows with any NA in predictors and keep y aligned
cc_train <- complete.cases(X_train_df)
X_train_df <- X_train_df[cc_train, , drop = FALSE]
y_train    <- TrainDF$high_yield[cc_train]

cc_test <- complete.cases(X_test_df)
X_test_df <- X_test_df[cc_test, , drop = FALSE]
y_test    <- TestDF$high_yield[cc_test]

# One-hot encode from TRAIN template (no intercept)
X_train <- model.matrix(~ . - 1, data = X_train_df)
X_test  <- model.matrix(~ . - 1, data = X_test_df)

# Ensure TEST has same columns as TRAIN (add any missing dummies as zeros)
missing_in_test <- setdiff(colnames(X_train), colnames(X_test))
if (length(missing_in_test) > 0) {
  X_test <- cbind(
    X_test,
    matrix(0, nrow = nrow(X_test), ncol = length(missing_in_test),
           dimnames = list(NULL, missing_in_test))
  )
}
# Order columns identically
X_test <- X_test[, colnames(X_train), drop = FALSE]

# 10-fold Cross-Validation on TRAIN to tune cost 
set.seed(123)
K <- 10
fold_id <- sample(rep(1:K, length.out = nrow(X_train)))
cost_grid <- 10^seq(-2, 2, by = 1)  # 0.01, 0.1, 1, 10, 100

cv_acc <- numeric(length(cost_grid))

for (ci in seq_along(cost_grid)) {
  Cval <- cost_grid[ci]
  acc_fold <- numeric(K)
  for (k in 1:K) {
    tr_idx <- which(fold_id != k)
    va_idx <- which(fold_id ==  k)
    
    x_tr <- X_train[tr_idx, , drop = FALSE]
    y_tr <- y_train[tr_idx]
    x_va <- X_train[va_idx, , drop = FALSE]
    y_va <- y_train[va_idx]
    
    fit_k <- svm(x = x_tr, y = y_tr,
                 kernel = "linear",
                 cost = Cval,
                 scale = TRUE)  # standardize using TRAIN stats
    
    pred_va <- predict(fit_k, newdata = x_va)
    cm_va   <- table(Actual = y_va, Pred = pred_va)
    acc_fold[k] <- sum(diag(cm_va)) / sum(cm_va)
  }
  cv_acc[ci] <- mean(acc_fold)
}

best_cost <- cost_grid[which.max(cv_acc)]

cat("\n==== 10-fold CV on TRAIN ====\n")
print(data.frame(cost = cost_grid, CV_Accuracy = round(cv_acc, 4)))
cat("Best cost: ", best_cost, " | Best CV Accuracy: ",
    round(max(cv_acc), 4), "\n", sep = "")

# Refit final SVM on full TRAIN with best cost 
svm_fit <- svm(x = X_train, y = y_train,
               kernel = "linear",
               cost = best_cost,
               scale = TRUE,
               probability = TRUE)

# TRAIN performance 
pred_train <- predict(svm_fit, newdata = X_train, probability = TRUE)
stopifnot(length(pred_train) == length(y_train))

cm_train  <- table(Actual = y_train, Predicted = pred_train)
acc_train <- sum(diag(cm_train)) / sum(cm_train)

probs_train <- attr(pred_train, "probabilities")[, "High"]  # High = positive
roc_train   <- roc(response = y_train, predictor = probs_train,
                   levels = c("Low","High"), direction = "<")
auc_train   <- auc(roc_train)

cat("\n==== TRAIN performance ====\n")
print(cm_train)
cat(sprintf("Train Accuracy: %.3f\n", acc_train))
cat(sprintf("Train AUC: %.3f\n", as.numeric(auc_train)))

# TEST performance
pred_test <- predict(svm_fit, newdata = X_test, probability = TRUE)
stopifnot(length(pred_test) == length(y_test))

cm_test  <- table(Actual = y_test, Predicted = pred_test)
acc_test <- sum(diag(cm_test)) / sum(cm_test)

probs_test <- attr(pred_test, "probabilities")[, "High"]
roc_test   <- roc(response = y_test, predictor = probs_test,
                  levels = c("Low","High"), direction = "<")
auc_test <- auc(roc_test)

cat("\n==== TEST performance ====\n")
print(cm_test)
cat(sprintf("Test Accuracy: %.3f\n", acc_test))
cat(sprintf("Test AUC: %.3f\n", as.numeric(auc_test)))

# Plot ROC curves 
par(mfrow = c(1,2))
plot(roc_train, main = sprintf("TRAIN ROC (AUC = %.3f)", as.numeric(auc_train)))
abline(a = 0, b = 1, lty = 2)
plot(roc_test,  main = sprintf("TEST ROC (AUC = %.3f)",  as.numeric(auc_test)))
abline(a = 0, b = 1, lty = 2)
par(mfrow = c(1,1))

# (Optional) Extract linear hyperplane parameters 
# w = sum_i alpha_i y_i x_i ;  b = -rho  (e1071 convention)
SV    <- as.matrix(svm_fit$SV)
alpha <- as.numeric(svm_fit$coefs)
w     <- colSums(sweep(SV, 1, alpha, `*`))
b     <- -svm_fit$rho
cat("\nHyperplane (linear): f(x) = w.x + b\nw (length ", length(w), ")\n", sep = "")
print(w)
cat("b = ", b, "\n", sep = "")

