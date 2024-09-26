# shiny_dashboard
This is a repository containing the files for the R Shiny dashboard group project, which was also subsequently deployed on an AWS EC2 instance. The project deals with utilizing Shiny dashboarding functionalities to visualize different interesting relationships in a classic bank churn dataset. It also utilizes a trained random forest machine learning model that can make live predictions on the dashboard. More detailed information about the dataset itself as well as about the variables it contains can be found in a csv file Variable_explanation.csv, which is also accessed from the dashboard app itself and is provided on one of its tabs.

The files contained in a repository:
- app.R - script to run the shiny app
- BankChurners.csv - the initial dataset on bank churn
- best_rf_model.rds - saved ML random forest model trained in R
- feature_importance.rds - importance of the final features of the random forest, utilized in the dashboard
- final_fit_features.rds - final features of the random forest, utilized in the dashboard
- machine_learning_model_training.R - R script for the training of the random forest model
- ROC_AUC_plot.rds - plot of the ROC (Receiver Operating Characteristic) curve for the final random forest model, utilized in the dashboard
- test_data.csv - a part of BankChurners.csv dataset used for testing the model
- train_data.csv - a part of BankChurners.csv dataset used for training the model
- Variable_explanation.csv - a description of each variable in the dataset, utilized in the dashboard
