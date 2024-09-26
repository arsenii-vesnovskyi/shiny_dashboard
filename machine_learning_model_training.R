######## Machine Learning Model for final R Project ########

library(tidymodels)
library(readr)
library(workflows)
library(tune)
library(dials)
library(parsnip)
library(discrim)
library(xgboost)

# We set the seed for reproducibility
set.seed(123)

# Load the dataset
# You must change the path to the location of the dataset on your machine
setwd("C:/Users/janic/OneDrive/Desktop/_ESADE_Master/_2._Semester_ESADE/Data_Analytics_with_R/final_project")
data <- read.csv("./BankChurners.csv") 

# We encode target variable as 0 for Existing Customer and 1 for Attrited Customer
# Our "positive class" is the minority class, which is Attrited Customer
# We are interested in predicting customers who are likely to churn (Attrited Customer)
# We want to find the model with the best ROC_AUC score for that goal
data$Attrition_Flag <- if_else(data$Attrition_Flag == "Existing Customer", 0, 1)

# We convert Attrition_Flag to a categorical variable and a factor given that it is our target variable
# and we want to do binary classification
data$Attrition_Flag <- as.factor(data$Attrition_Flag)

#We define the metrics we want to use later on
metrics_performance <- metric_set(roc_auc, accuracy, precision, recall, f_meas)

data <- data %>% 
  select(-CLIENTNUM) %>% # Remove CLIENTNUM column because it is not useful for modeling given that it is a unique identifier
  mutate(across(where(is.character), as.factor)) # We convert categorical variables to factors otherwise some ML models may not work

# Split data into training and test data
# We split in a stratified manner to ensure that the proportion of Attrited Customers is the same in both sets
# This is important due to the class imbalance in the target variable
data_split <- initial_split(data, prop = 0.75, strata = Attrition_Flag)
train_data <- training(data_split)
test_data <- testing(data_split)

# We save the train and test data sets as csv-files
#write.csv(train_data, file = "./train_data.csv")
#write.csv(test_data, file = "./test_data.csv")

############ Model Training, Evaluation and Selection ############

# Recipe for data preprocessing
recipe <- recipe(Attrition_Flag ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # We use one-hot encoding for categorical variables
  step_zv(all_predictors()) %>% # We remove zero-variance predictors that do not provide any information for the ML model
  step_normalize(all_numeric(), -all_outcomes()) # We normalize numeric predictors

# We also utilize grid control artefact (including get_coefs function) analogically 
# to the procedure in class in order to control behavior of the tune_grid() function.

get_coefs <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

# Unlike in class, we had to set save_pred to FALSE, as otherwise RStudio crashes
# when trying to open the results object.
df_grid_control <- control_grid(save_pred = FALSE,
                                allow_par = TRUE,
                                verbose = TRUE,
                                extract = get_coefs,
                                parallel_over = 'everything', 
                                save_workflow = TRUE)  

# Models specification
rf_spec <- rand_forest(trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "permutation") %>% # We use the ranger engine for the random forest model
                                                       # and we use permutation importance to determine feature importance
                                                       # We will later visualize the feature importance with the vip package
  set_mode("classification")

xgb_spec <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune(), loss_reduction = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow creation
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

xgb_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(xgb_spec)

# We set the seed for reproducibility
set.seed(123)

# Grids for tuning
rf_grid <- grid_latin_hypercube(
  trees(range = c(75, 450)),
  min_n(range = c(2, 20)),
  size = 7
)

xgb_grid <- grid_latin_hypercube(
  trees(range = c(75, 450)),
  tree_depth(range = c(4, 14)),
  learn_rate(range = c(0.01, 0.2)),
  loss_reduction(range = c(0, 10)),
  size = 7
)

# Hyperparameter tuning
rf_results <- tune_grid(
  rf_workflow,
  resamples = bootstraps(train_data, times = 10, strata = Attrition_Flag),
  grid = rf_grid,
  metrics = metric_set(roc_auc),
  control = df_grid_control
  # We also use the defined grid control.
)

xgb_results <- tune_grid(
  xgb_workflow,
  resamples = bootstraps(train_data, times = 10, strata = Attrition_Flag),
  grid = xgb_grid,
  metrics = metric_set(roc_auc),
  control = df_grid_control
  # We also use the defined grid control.
)

####### Selecting the best performing model based on ROC_AUC and training data #######

metrics_rf <- rf_results %>% 
  collect_metrics() %>% 
  pivot_wider(id_cols = c(.config), 
              names_from = .metric, 
              values_from = mean)

metrics_rf_arranged <- arrange(metrics_rf, desc(roc_auc))

metrics_xgb <- xgb_results %>% 
  collect_metrics() %>% 
  pivot_wider(id_cols = c(.config), 
              names_from = .metric, 
              values_from = mean)

metrics_xgb_arranged <- arrange(metrics_xgb, desc(roc_auc))

best_rf_model_roc_auc <- metrics_rf_arranged %>%
  head(1) %>%
  mutate(model_type = "best_RF_roc_auc") %>%
  relocate(model_type, .before = 1)

best_xgb_model_roc_auc <- metrics_xgb_arranged  %>%
  head(1)%>%
  mutate(model_type = "best_boost_roc_auc") %>%
  relocate(model_type, .before = 1)

best_models_combined_roc_auc <- bind_rows(
  best_rf_model_roc_auc,
  best_xgb_model_roc_auc
)

best_models_combined_arranged_roc_auc <- best_models_combined_roc_auc %>%
  head(1) %>%
  arrange(desc(roc_auc))

best_models_combined_arranged_roc_auc
best_models_combined_arranged_roc_auc$.config

# We get the specific configuration of the best performing model
rf_metrics <- rf_results %>%
  collect_metrics()

#install.packages("stringr")
library(stringr)

# Filter for the specific configuration
specific_config <- rf_metrics %>%
  filter(str_detect(.config, best_models_combined_arranged_roc_auc$.config)) %>%
  filter(.metric == "roc_auc")

# this are the specifications for the best performing model
specific_config

# This best performing model RF model can now be used for prediction on unseen data/new data
best_rf <- rf_results %>% select_best( metric = "roc_auc")

# best performing model specifications
final_fit <- extract_workflow(rf_results) %>%
  finalize_workflow(best_rf)%>%
  last_fit(split = data_split)

predictor = final_fit$.workflow[[1]]

predict(object = predictor, new_data = train_data)

# Saving the best RF model to an RDS file
saveRDS(predictor, file = "./best_rf_model.rds")

# We imoport the model again
predictor <- readRDS("./best_rf_model.rds")

final_fit_predictions <- predict(object = predictor, new_data = test_data)
#final_fit_predictions

######## Performance Metrics ######## 

library(dplyr)

# We combine preditions and true values in one tibble/dataframe
combined_data <- bind_cols(test_data$Attrition_Flag, Prediction = final_fit_predictions$.pred_class)

combined_data <- combined_data %>%
  rename("Actual_Value" = ...1)

print(colnames(combined_data))

conf_matrix <- combined_data %>% conf_mat(Actual_Value,Prediction)
print(conf_matrix)

# Calculate Accuracy
accuracy_value <- accuracy(combined_data, truth = Actual_Value, estimate = Prediction)
print("Accuracy")
print(accuracy_value)

# Calculate Recall
recall_value <- recall(combined_data, truth = Actual_Value, estimate = Prediction)
print("Recall")
print(recall_value)

# Calculate Precision
precision_value <- precision(combined_data, truth = Actual_Value, estimate = Prediction)
print("Precision")
print(precision_value)

# we get the probaility predictions
final_fit_predictions_prob <- predict(object = predictor, new_data = test_data, type = "prob")
#final_fit_predictions_prob

# Combine the actual values and predicted probabilities into one tibble
combined_data_prop <- bind_cols(
  Actual_Value = test_data$Attrition_Flag, 
  Predicted_Probability = final_fit_predictions_prob$.pred_0
  
)

roc_auc_value <- roc_auc(combined_data_prop, Actual_Value, Predicted_Probability)
print("ROC_AUC")
print(roc_auc_value)

###### Testing a prediction for a single data point ######

# take from test data the first row to predict
vector_to_predict <- test_data[1,]

# remove the target variable from the vector
vector_to_predict <- vector_to_predict %>% select(-Attrition_Flag)
vector_to_predict

# give the vector to the model to predict
prediction_vector <- predict(object = predictor, new_data = vector_to_predict)

# print the prediction
print("Prediction")
print(as.character(prediction_vector$.pred_class))

predictor

########## ROC_AUC Curve ##########

# Now, based on the previous calculations we can create the ROC curve (recall on
# the y-axis and 1 - precision (FPR) on the x-axis).

library(ggplot2)
library(yardstick)

final_fit_predictions <- final_fit %>% 
  collect_predictions()

ROC_AUC_plot <- final_fit_predictions %>% 
  roc_curve(Attrition_Flag, `.pred_0`) %>% 
  autoplot() + 
  scale_y_continuous(breaks = seq(0, 1, by = .05)) + 
  scale_x_continuous(breaks = seq(0, 1, by = .05)) +
  theme_minimal() +
  # We again rotate the x-axis labels to achieve better readability.
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("ROC Curve for our overall, best perfroming model (Random Forrest)")

ROC_AUC_plot

# Now we save the ROc_AUC plot as a RDS-file
saveRDS(ROC_AUC_plot, file = "./ROC_AUC_plot.rds")

########## Feature Importance ##########

# We install the package "vip" if it is not already installed and then load it 
# into the environment.
if (!requireNamespace("vip", quietly = TRUE)) install.packages("vip")
library(vip)

# The vip-package provides a function vip() that allows us to visualize the
# importance of the features in the model.
# In that way, we can show the 10 most important features in the model.
final_fit_features <- final_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 10) +
  ggtitle("The top 10 Features with the highest importance in the model predicting customer churn")

final_fit_features

# Now we save the feature importance plot as a RDS-file
saveRDS(final_fit_features, file = "./feature_importance.rds")