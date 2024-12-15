# Loading the dataset
df <- read.table("C:\\Users\\abbas\\OneDrive\\Desktop\\MSDA\\Fall2024\\Linear Regression\\Projects\\Case_Study\\Case_Study.txt", header = FALSE, col.names =c("id", "Y", "X1","X2", "X3", "X4", "X5", "X6"))
attach(df) 

#checking for null values
cat("No. of Null values in each of the columns:")
colSums(is.na(df))

#Structure of the dataframe
df = df[,2:8] #removing the id column
cat("The Structure of the dataframe:")
str(df)

#Summary of the dataframe
print("The Summary of the dataframe:")
summary(df)


#Visualize the data
library(ggplot2)
library(GGally)
ggpairs(df, lower = list(continuous = "points"), aes(color = as.factor(X4)), 
        upper = list(continuous = wrap("cor", size = 5)),
        diag = list(continuous = "densityDiag"),
        columnLabels = c("Website Delivered", "Backlog of Orders", "Team Number", "Team Experience", "Process Change", "Year", "Quarter")) + ggtitle("Pairwise plot of Website Developer data") + theme_light()


#Pre-processing
#Encoding
categorical_col <- c("X2", "X4", "X5", "X6")
categorical_col
df[categorical_col] <- lapply(df[categorical_col], as.factor)

#scaling
df[, c("X1", "X3")] <- scale(df[, c("X1", "X3")])

# Structure of the matrix after Encoding and Scaling
str(df)

#One-Hot Encoding using model matrix
df_onehot <- model.matrix(~ X2+ X4 +X5 + X6 - 1, data = df)  # -1 removes the intercept
print(df_onehot)
df <- cbind(df[, c("Y", "X1", "X3")] , df_onehot)
cat("\n Main Effect Terms:")
str(df)


# Separate numeric and categorical variables
numeric_vars <- c("X1", "X3")
categorical_vars <- c("X21", "X22", "X23", "X24", "X41", "X52002", "X62", "X63", "X64")

# Create interactions: Numeric × Numeric
for (i in 1:length(numeric_vars)) {
  for (j in (i + 1):length(numeric_vars)) {
    interaction_name <- paste0(numeric_vars[i], "_", numeric_vars[j])
    df[[interaction_name]] <- df[[numeric_vars[i]]] * df[[numeric_vars[j]]]
  }
}

# Create interactions: Numeric × Categorical
for (num_var in numeric_vars) {
  for (cat_var in categorical_vars) {
    interaction_name <- paste0(num_var, "_", cat_var)
    df[[interaction_name]] <- df[[num_var]] * df[[cat_var]]
  }
}

# Create interactions: Categorical × Categorical
for (i in 1:(length(categorical_vars) - 1)) {
  for (j in (i + 1):length(categorical_vars)) {
    interaction_name <- paste0(categorical_vars[i], "_", categorical_vars[j])
    df[[interaction_name]] <- df[[categorical_vars[i]]] * df[[categorical_vars[j]]]
  }
}
cat("structure of the dataframe with main effect, interaction and quadratic terms:\n")
str(df)

cat("No. of columns in dataframe:", ncol(df))
cat("No. of rows in dataframe:", nrow(df))


# Training
full_model <- lm(Y ~ ., data = df )
print("The summary of Full model:")
summary(full_model)

#Backward Selection
b_model <- step(full_model, direction = "backward")
print("The summary of Backward selection model:")
summary(b_model)

#Forward Selection
null_model <- lm(Y~1, data = df) # Null model
f_model <- step(null_model, direction = 'forward', scope = list(upper = full_model, lower = null_model))
print("The summary of Forward selection model:")
summary(f_model)

#Adjusted R square
cat("The Adjusted R square for backward selection model:\n", summary(b_model)$adj.r.squared)
cat("The Adjusted R square for forward selection model:\n", summary(f_model)$adj.r.squared)
cat("BIC for Backward selection model:\n", BIC(b_model))
cat("BIC for forward selection model:\n", BIC(f_model))

print("Analysis of Variance test between Forward selection and full model:\n")
anova(f_model, full_model)
# Through this approach we'll use forward selection model as the best subset model


# Diagnostic Checks to evaluate Outliers
# Outliers in X direction
X <- model.matrix(f_model)
H <- X %*% solve(t(X) %*% X) %*% t(X)
leverage <- diag(H)
leverage
n <- length(Y)
p <- ncol(X)
threshold <- (2 * p) / n
threshold
# Test for outlying X observation
x_outliers <- which(leverage > threshold) #index of influential points
cat("Observations with high leverage:") 
leverage[x_outliers]

#Outliers in Y direction
studentized_residuals <- rstudent(f_model)
print("Studentized Residuals:")
print(studentized_residuals)
y_outliers <- which(abs(studentized_residuals) > 3)
cat("Observation outside of -3 and 3: ", y_outliers, "\n")
studentized_residuals[y_outliers]

# Identifying Influential Points
# DFFITS for influential cases
dffits_values <- dffits(f_model)
dffits_values
cat("DFFITS values for each observation:\n", dffits_values, "\n")
# Threshold for DFFITS: Absolute value > 2 * sqrt(p/n) could indicate an influential case
threshold_dffits <- 2 * sqrt(p / n)
threshold_dffits
high_influence_dffits <- which(abs(dffits_values) > threshold_dffits)
cat("Index values of observations with high influence (DFFITS > threshold):", high_influence_dffits, "\n")
dffits_values[high_influence_dffits]


# Cook's Distance
# Cook's distance to measure overall influence of each point
cooks_distance <- cooks.distance(f_model)
cat("Cook's distance for each observation:\n")
cooks_distance
# Threshold for Cook's distance: generally values > 4/n are considered influential
threshold_cooks <- 4 / n
high_influence_cooks <- which(cooks_distance > threshold_cooks)
cat("Index Value of observations with high influence (Cook's Distance > threshold):", high_influence_cooks, "\n")
cooks_distance[high_influence_cooks]


# DFBETAS
# DFBETAS measure the influence of each observation on each coefficient
dfbetas_values <- dfbetas(f_model)
dfbetas_values
cat("DFBETAS values for each coefficient:\n", dfbetas_values, "\n")
# DFBETAS threshold: generally values greater than 2/sqrt(n) are considered influential
threshold_dfbetas <- 2 / sqrt(n)
high_influence_dfbetas <- which(abs(dfbetas_values[, "X41"]) > threshold_dfbetas)
cat("Index Values of observations with high influence (DFBETAS > threshold):", high_influence_dfbetas, "\n")
dfbetas_values[high_influence_dfbetas, "X41"]

# Visualizing DFFITS and Cook's Distance
library(olsrr)

# DFBETAS plot
ols_plot_dfbetas(f_model)

# Cook's Distance plot
ols_plot_cooksd_bar(f_model)


#DFFITS Plot
ols_plot_dffits(f_model)

# Influential Data Points

Points_to_remove <- c(dffits_values[high_influence_dffits], cooks_distance[high_influence_cooks], dfbetas_values[high_influence_dfbetas, "X41"] )
unique_points <- unique(names(Points_to_remove))
cat("The influential data points are:\n" , unique_points)

# Diagnostic Plots
# Residual Vs Fitted
plot(f_model, which = 1)

# Scale-Location
plot(f_model, which = 3)

#Residuals vs Leverage
ols_plot_resid_lev(f_model)

#QQ plot
ols_plot_resid_qq(f_model)

# Model Selection After removing influential points
unique_points <- as.numeric(unique_points)
df_wo <- df[-unique_points, ]
full_model_wo <- lm(Y ~ . , data = df_wo)
#Forward Selection
null_model <- lm(Y~1, data = df_wo) # Null model
f_model_wo <- step(null_model, direction = 'forward', scope = list(upper = full_model_wo, lower = null_model))
print("The summary of forward selection model without outliers:\n")
summary(f_model_wo)


cat("The Adjusted R square for forward selection model:\n", summary(f_model)$adj.r.squared)
cat("The Adjusted R square for forward selection model without outliers:\n", summary(f_model_wo)$adj.r.squared)
cat("BIC for forward selection model:\n", BIC(f_model))
cat("BIC for forward selection model without outliers:\n", BIC(f_model_wo))


# Multicollinearity
library(car)
cat("Variance Inflation Factor values for the forward selection without outliers are:\n")
vif_values <- vif(f_model_wo)
vif_values
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))

#Removing X1_X41
#update the f_model_wo
updated_f_model_wo <- update(f_model_wo, . ~ . - X1_X41)
summary(updated_f_model_wo)
vif_values <- vif(updated_f_model_wo)
vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))


# removing X1_X52002
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X1_X52002)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))

# removing X24_X41
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X24_X41)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))
vif(updated_f_model_wo)

# removing X41_X62
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X41_X62)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))
vif(updated_f_model_wo)


# removing X21_X41
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X21_X41)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))
vif(updated_f_model_wo)

# removing X24_X52002
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X24_X52002)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))
vif(updated_f_model_wo)

# removing X23_X52002
updated_f_model_wo <- update(updated_f_model_wo, . ~ . -X23_X52002)
vif_values <- vif(updated_f_model_wo)
cat("The observation with maximimum VIF value is" , names(which.max(vif_values)), "with a VIF value of", max(vif_values))
vif(updated_f_model_wo)


summary(updated_f_model_wo)
anova(updated_f_model_wo)

# Performed Anova test for above model and removed irrelevant features
updated_f_model_wo_t <- update(updated_f_model_wo, . ~ . -(X22_X41 + X26 + X24_X62 + X23_X41 + X24_X64 + X21_X64 + X21_X62 ))
vif(updated_f_model_wo_t)
summary(updated_f_model_wo_t)
anova(updated_f_model_wo_t, updated_f_model_wo)

updated_f_model_wo_t1 <- update(updated_f_model_wo_t, .~. -(X21_X52002 + X212 + X1_X22))
summary(updated_f_model_wo_t1)
anova(updated_f_model_wo_t1, updated_f_model_wo_t)

updated_f_model_wo_t2 <- update(updated_f_model_wo_t1, .~. -(X3_X24 +X210 + X27))
summary(updated_f_model_wo_t2)
anova(updated_f_model_wo_t2, updated_f_model_wo_t1) # Since above removed variables are significant


#Final 
# Summary of the model after removing correlated columns
print("Summary of the model after removing correlated columns:") 
summary(updated_f_model_wo_t1)
print("Analysis of varaiance of model:")
anova(updated_f_model_wo_t1)


#K-cross Validation
install.packages("caret")
library(caret)
train_control <- trainControl(method = "cv", number = 5)

# Fit the model using cross-validation
cv_model <- train(Y ~ X41 + X1_X3 + X1_X63 + X23_X63 + X3_X24 + X210 + X27 + X29, data = df_wo, method = "lm", trControl = train_control)

# Print cross-validation results
print(cv_model)

