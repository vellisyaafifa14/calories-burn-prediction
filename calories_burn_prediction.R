# INSTALL JIKA BELUM ADA
install.packages("randomForest")
install.packages("caTools")
install.packages("Metrics")
install.packages("ggplot2")
install.packages("dplyr")

# IMPORT LIBRARY
library(randomForest)
library(caTools)
library(ggplot2)
library(Metrics)
library(dplyr)

# LOAD DATASET
data <- read.csv("dataset/calories.csv")


summary(data)

# PEMBERSIHAN DAN PEMERIKSAAN DATA
colSums(is.na(data))
data <- data %>% distinct()
summary(data)

# CONVERT GENDER MENJADI FAKTOR
data$Gender <- as.factor(data$Gender)

# SPLIT DATA
set.seed(123)   # agar hasil konsisten
split <- sample.split(data$Calories, SplitRatio = 0.8)

train_data <- subset(data, split == TRUE)
test_data  <- subset(data, split == FALSE)

nrow(train_data)
nrow(test_data)

# MEMBANGUN MODEL RANDOM FOREST REGRESSION
rf_model <- randomForest(
  Calories ~ Age + Height + Weight + Duration + Heart_Rate + Body_Temp + Gender,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

print(rf_model)

# PREDIKSI DAN EROR MATRIKS
predictions <- predict(rf_model, newdata = test_data)

mse  <- mse(test_data$Calories, predictions)
rmse <- rmse(test_data$Calories, predictions)
mae  <- mae(test_data$Calories, predictions)

mse
rmse
mae

# HITUNG R-SQUARED
SSE <- sum((test_data$Calories - predictions)^2)
SST <- sum((test_data$Calories - mean(test_data$Calories))^2)
R2  <- 1 - SSE/SST

R2

# PLOT ACTUAL VS PREDICTED
ggplot() +
  geom_point(aes(x = test_data$Calories, y = predictions)) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  labs(x="Actual Calories", y="Predicted Calories",
       title="Actual vs Predicted Calories (Random Forest Model)")


# RESIDUAL PLOT (MENGECEK EROR MODEL)
residuals <- test_data$Calories - predictions

ggplot() +
  geom_point(aes(x = predictions, y = residuals)) +
  geom_hline(yintercept = 0, color="red") +
  labs(x="Predicted", y="Residuals",
       title="Residual Plot")

# FEATURE IMPORTANCE
importance(rf_model)
varImpPlot(rf_model)

# PREDIKSI DATA BARU
new_data <- data.frame(
  Gender = factor("male", levels = levels(data$Gender)),
  Age = 25,
  Height = 175,
  Weight = 70,
  Duration = 30,
  Heart_Rate = 120,
  Body_Temp = 37.5
)

new_prediction <- predict(rf_model, newdata = new_data)
new_prediction

