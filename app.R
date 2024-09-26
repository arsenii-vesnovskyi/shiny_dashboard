# Libraries ----
library(tidyverse)
library(workflows)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(RColorBrewer)
library(reshape2)

# Data ----
Explanation <- read.csv("Variable_explanation.csv")
BankChurners <- read.csv("BankChurners.csv") 
data <- BankChurners

# Model ----
best_rf_model <- readRDS("best_rf_model.rds")

# Visualization of ROC_AUC Curve -----
roc_curve <- readRDS("ROC_AUC_plot.rds")

# Visualization of Feature Importance -----
feature_importance <- readRDS("feature_importance.rds")

# Confusion Matrix -----

# We manually create the confusion matrix based on the results we obatined in the model training script
# which we also provide as part of this submission.
# The confusion matrix was computed based on the test data.
confusion_matrix <- matrix(c(2103, 22, 86, 321), nrow = 2, byrow = TRUE)
colnames(confusion_matrix) <- c('Predicted: Existing Customer', 'Predicted: Attrited Customer')
rownames(confusion_matrix) <- c('Actual: Existing Customer', 'Actual: Attrited Customer')

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Bank Customer Churn Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      ## TABS ----
      
      
      ### FILTERS ----
      menuItem(
        "Filters", tabName = "filters", startOpen = TRUE, icon = icon("sliders"),
        
        #### Group: Attrition Flag ----
        h3("Attrition Flag"),
        selectInput("attritionFlagFilter", "Attrition Flag", choices = c("All", "Existing Customer", "Attrited Customer")),
        
        #### Group: Demographics ----
        h3("Demographics"),
        
        selectInput("genderFilter", "Gender", choices = c("All", "M", "F")),
        
        sliderInput("ageFilter", "Customer Age", min = 18, max = 100, value = c(18, 100)),
        
        selectInput(
          "incomeFilter", 
          "Income Category", 
          choices = c("All", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown")
        ),
        
        selectInput(
          "educationFilter", 
          "Education Level", 
          choices = c("All", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate", "Unknown")
        ),
        
        # Separator
        div(style = "margin-top: 20px; margin-bottom: 20px; border-bottom: 1px solid #e5e5e5;"),
        
        #### Group: Financials ----
        h3("Financials"),
        selectInput(
          "cardCategoryFilter", 
          "Card Category", 
          choices = c("All", "Blue", "Silver", "Gold", "Platinum")
        ),
        sliderInput(
          "transactionAmountRange",
          "Total Transaction Amount",
          min = 0, 
          max = 20000,   
          value = c(0, 20000),  
          step = 100,   
          pre = "$"     
        ),
        uiOutput("dynamicFilterUI")
      ),
      
      
      # . ----
      ### DESCRIPTION TAB ----
      menuItem("Dataset Summary", tabName = "datasetSummary", icon = icon("list"),
               menuSubItem("Variable Description", tabName = "description", icon = icon("info-circle")),
               menuSubItem("Data Summary", tabName = "dataSummary", icon = icon("table"))
      ),
      
      
      ### RAW DATA TAB ----
      menuItem(
        "Raw Data", icon = icon("database"),
        menuSubItem("Table", tabName = "table", icon = icon("table")),
        menuSubItem("Bivariate Relationship", tabName = "bivariate", icon = icon("line-chart")),
        menuSubItem("Heatmap", tabName = "heatmap", icon = icon("fire"))
      ),
      
      
      ### DEMOGRAPHICS TAB ----
      menuItem(
        "Demographics", icon = icon("users"),
        menuSubItem("Churn Rate Overview", tabName = "churnOverview", icon = icon("bar-chart")),
        menuSubItem("Age & Gender", tabName = "customerDemographics", icon = icon("id-badge")),
        menuSubItem("Education Level", tabName = "educationLevel", icon = icon("graduation-cap")),
        menuSubItem("Income", tabName = "income", icon = icon("wallet")),
        menuSubItem("Family", tabName = "family", icon = icon("user-friends"))
      ),
      
      
      ### FINANCIALS TAB ----
      menuItem(
        "Financials", icon = icon("university"),
        menuSubItem("Activity and Utilization", tabName = "activityUtilization", icon = icon("credit-card")),
        menuSubItem("Card Category", tabName = "cardCategory", icon = icon("credit-card")),
        menuSubItem("Customer Lifetime", tabName = "customerLifetime", icon = icon("chart-line")),
        menuSubItem("Credit Overview", tabName = "creditOverview", icon = icon("dollar-sign")),
        menuSubItem("Avg Open to Buy", tabName = "avgOpenToBuy", icon = icon("unlock-alt")),
        menuSubItem("Spending Patterns", tabName = "spendingPatterns", icon = icon("chart-area"))
      ),
      
      ### MODEL TAB ----
      menuItem(
        "Model", icon = icon("cogs"),
        menuSubItem("Model Prediction", tabName = "modelPrediction", icon = icon("search")),
        menuSubItem("Model Performance", tabName = "modelPerformance", icon = icon("chart-line"))
      )
    )
  ),
  
  # . ----
  ## DASHBOARD BODY ----
  dashboardBody(
    tags$style(
      HTML(
        ".skin-blue .main-header .navbar { background-color: #101356; }",
        ".skin-blue .main-header .logo { background-color: #101356; }",
        ".skin-blue .main-header .logo:hover { background-color: #101356; }",
        ".skin-blue .main-header .navbar .sidebar-toggle { background-color: #101356 ; }"
        
      )
    ),
        
    tabItems(
      ### Description Dashboard ----
      
      #### Tab Description ----
      tabItem(tabName = "description",
              fluidRow(box(title = "Variable Descriptions",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab introduces the different variables to foster a better understanding of the dataset. The table below provides a description of each variable in the dataset. The overall dataset contains 10,127 observations and 21 variables.")
              )
              ),
              fluidRow(
                box(DT::dataTableOutput("descTable"), width = 12)
              )
      ),
      
      #### Tab Data Summary ----
      tabItem(tabName = "dataSummary",
              h2("Data Summary"),
              fluidRow(box(title = "Data Summary",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("Here some key metrics are presented to provide an overview of the dataset.")
              )
              ),
              fluidRow(box(title = "Churn Rate", solidHeader = TRUE, collapsible = FALSE, status = "primary", width = 12,
                           htmlOutput("churnRateText"))),
              fluidRow(uiOutput("churnRateBox"), width = 12),
              
              fluidRow(box(title = "Customers", solidHeader = TRUE, collapsible = FALSE, status = "primary", width = 12,
                           htmlOutput("summaryText"))),
              fluidRow(
                uiOutput("totalCustomersBox"),
                uiOutput("averageAgeBox"),
                uiOutput("genderPercentageInfoBox")
              ),
              
              fluidRow(box(title = "Transactions", solidHeader = TRUE, collapsible = FALSE, status = "primary", width = 12,
                           htmlOutput("transactionSummaryText"))),
              fluidRow(
                column(4, uiOutput("totalTransactionSum")),
                column(4, uiOutput("totalTransactionCount")),
                column(4, uiOutput("averageTransactionAmount"))
              )),
      
      # . ----
      ### Raw Data Dashboard ----
      
      #### Tab Table ----
      tabItem(tabName = "table", 
              DT::dataTableOutput("dataTable")),
      
      #### Tab Bivariate Relationship ----
      tabItem(tabName = "bivariate",
              fluidRow(box(title = "Bivariate Relationship",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("In this tab, you can choose two different variables from the dataset and plot them against one another.")
              )),
              fluidRow(
                column(4, selectInput("xvar", "Choose X Variable:", choices = NULL)),
                column(4, selectInput("yvar", "Choose Y Variable:", choices = NULL)),
              ),
              fluidRow(plotOutput("varPlot"))
      ),
      
      #### Tab Heatmap ----
      tabItem(tabName = "heatmap",
              fluidRow(box(title = "Heatmap",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("The heatmap shows whether there are some variables that might correlate with each other.")
              )
              ),
              fluidPage(plotOutput("heatmapPlot", width = "100%", height = "900px"))),
      
      # . ----
      ### Demographics Dashboard ----
      
      #### Tab Churn Rate Overview ----
      tabItem(tabName = "churnOverview",
              fluidRow(box(title = "Churn Rate Overview",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab provides an overview of the distribution of the Churn Rate across the dataset.")
              )),
              fluidRow(box(plotOutput("churnRatePlot"), width = 12))),
      
      #### Tab Customer Demographics ----
      tabItem(tabName = "customerDemographics", 
              fluidRow(box(title = "Age & Gender",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab shows the distribution of the Age in general and broken down by gender. It also shows the distribution of the two genders across the dataset.")
              )),
              fluidRow(box(plotOutput("ageDistribution"), width = 6), 
                       box(plotOutput("genderDistribution"), width = 6))),
      
      #### Tab Education Level ----
      tabItem(tabName = "educationLevel", 
              fluidRow(box(title = "Educational Level",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("In this tab, you can explore the distribution of the Education Level among the customers. 
                             You can also see a heatmap that plots the Education Level against the Income Category.")
              )),
              fluidRow(box(plotOutput("educationLevelPlot"), width = 12)), 
              fluidRow(box(plotOutput("incomeEducationPlot"), width = 12))),
      
      #### Tab Income  ----
      tabItem(tabName = "income",
              fluidRow(box(title = "Income",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab provides an overview of the distribution of the different Income categories and also shows the churn rate within each of these categories.")
              )),
              fluidRow(box(plotOutput("incomeCategoryChurnPlot"), width = 12))),
      
      #### Tab Family ----
      tabItem(tabName = "family", 
              fluidRow(box(title = "Family",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the family attributes, such as the distribution of the Marital Status and the number of dependents, each broken down into Attrited and Existing Customers. Additionally, it also explores in a heatmap how these two attributes are related to each other.")
              )),
              fluidRow(box(plotOutput("dependentCountPlot"), width = 6), 
                       box(plotOutput("maritalStatusPlot"), width = 6)),
              fluidRow(box(plotOutput("dependentMaritalStatusPlot"), width = 12))),
      
      # . ----
      ### Financials Dashboard ----
      
      #### Tab Activity and Utilization ----
      tabItem(tabName = "activityUtilization", 
              fluidRow(box(title = "Activity & Utilization",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the overview of customer activity and utilization through transaction amount distribution.")
              )),
              fluidRow(
                column(6,  # Allocate half the row width to this slider
                       sliderInput("binWidthTotalTransAmt",
                                   "Bin Width for Total Transaction Amount:",
                                   min = 100,
                                   max = 5000,
                                   value = 2500,
                                   step = 100)
                ),
                column(6,  # Allocate the other half to this slider
                       sliderInput("binWidthCreditUtilization",
                                   "Bin Width for Credit Utilization:",
                                   min = 0,
                                   max = 0.2,
                                   value = 0.1,
                                   step = 0.02)
                )),
              fluidRow(box(plotOutput("totalTransAmtPlot"), width = 6), 
                       box(plotOutput("creditUtilizationPlot"), width = 6)),
              sliderInput("binWidthTransCt",
                          "Bin Width for Total Transaction Count:",
                          min = 1,
                          max = 25,
                          value = 13),
              fluidRow(box(plotOutput("totalTransCtPlot"), width = 12))),      
      
      #### Tab Card Category ----
      tabItem(tabName = "cardCategory", 
              fluidRow(box(title = "Card Category",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the customer card categories, including the distribution of card categories and churn rates across different card types.")
              )),        
              fluidRow(box(plotOutput("cardCategoryPlot"), width = 12)), 
              fluidRow(box(plotOutput("cardCategoryChurnPlot"), width = 12))),
      
      #### Tab Customer Lifetime ----
      tabItem(tabName = "customerLifetime", 
              fluidRow(box(title = "Customer Lifetime",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the customer lifetime metrics, including months on book distribution, CLV distribution, and product holdings per customer.")
              )), 
              sliderInput("binWidthMonth",
                          "Bin Width for Months on Books:",
                          min = 1,
                          max = 25,
                          value = 13),
              fluidRow(box(plotOutput("monthsOnBookPlot"), width = 12)),
              sliderInput("binWidthCLV",
                          "Bin Width for Customer Lifetime Value:",
                          min = 100,
                          max = 5000,
                          value = 2500, 
                          step = 100),
              fluidRow(box(plotOutput("clvPlot"), width = 12)),
              fluidRow(box(plotOutput("productAnalysisPlot"), width = 12))),
      
      #### Tab Credit Overview ----
      tabItem(tabName = "creditOverview", 
              fluidRow(box(title = "Credit Overview",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the credit-related metrics, including the distribution of credit limits and total revolving balances.")
              )), 
              fluidRow(box(plotOutput("creditLimitPlot"), width = 12)),
              sliderInput("binWidthBal",
                          "Bin Width for Total Revolving Balance:",
                          min = 10,
                          max = 500,
                          value = 250, 
                          step = 25),
              fluidRow(box(plotOutput("totalRevolvingBalPlot"), width = 12))),
      
      #### Tab Avg Open to Buy ----
      tabItem(tabName = "avgOpenToBuy", 
              fluidRow(box(title = "Avg Open To Buy ",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the the distribution of average open-to-buy values.")
              )), 
              sliderInput("binWidthAvgOpenToBuy",
                          "Bin Width for Average Open to Buy:",
                          min = 100,
                          max = 5000,
                          value = 2500, 
                          step = 100),
              fluidRow(box(plotOutput("avgOpenToBuyPlot"), width = 12))),
      
      #### Tab Spening Patterns ----
      tabItem(tabName = "spendingPatterns", 
              fluidRow(box(title = "Spending Patterns",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("This tab explores the  spending patterns categorized by income levels.")
              )), 
              fluidRow(box(plotOutput("spendingPatternsPlot"), width = 12))),
      
      # . ----
      ### Model Dashboard ----
      
      #### Tab Model Prediction ----
      tabItem(tabName = "modelPrediction",
              fluidRow(box(title = "Model Prediction",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                           p("In this tab the user can chose the values he/she would provide and based on these values the model will make a prediction whether this customer is likely to churn or not.
                             Given that in reality most customers will stay with the credit card company, most settings will return that the specific customer with the related settings is likely to stay.
                             To obatin a prediction for a customer that is likely to leave the company, you can adjust the default values for the following variables.
                             Card Category = Platinum, Months Inactive = 10, Total Amount Change Q4-Q1 = 0.6, Total Transaction Amount = 8000, Total Count Change Q4-Q1 = 0.5")
              )
              ),
              fluidRow(
                column(6,
                       box(
                         title = "Demographics Filters",
                         solidHeader = TRUE,
                         status = "primary",
                         width = 12,
                         selectInput("genderModelFilter", "Gender", choices = c("M", "F")),
                         sliderInput("ageModelFilter", "Age", min = 18, max = 100, value = 51, step=1),
                         selectInput("educationModelFilter", "Education Level", choices = c("Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate", "Unknown")),
                         selectInput("maritalModelFilter", "Marital Status", choices = c("Single", "Married", "Divorced", "Unknown")),
                         sliderInput("dependentCountModelFilter", "Dependent Count", min = 0, max = 10, value = 3, step=1),
                         selectInput("cardCategoryModelFilter", "Card Category", choices = c("Blue", "Silver", "Gold", "Platinum"))
                       )
                ),
                column(6,
                       box(
                         title = "Financial Filters",
                         solidHeader = TRUE,
                         status = "primary",
                         width = 12,
                         selectInput("incomeModelFilter", "Income Category", choices = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown")),
                         sliderInput("monthsOnBookModelFilter", "Months on Book", min = 0, max = 60, value = 36, step=1),
                         sliderInput("totalRelationshipCountModelFilter", "Total Relationship Count", min = 1, max = 10, value = 4, step=1),
                         sliderInput("monthsInactive12MonModelFilter", "Months Inactive (12 months)", min = 0, max = 12, value = 1, step=1),
                         sliderInput("contactsCount12MonModelFilter", "Contacts Count (12 months)", min = 0, max = 10, value = 0, step=1),
                         sliderInput("creditLimitModelFilter", "Credit Limit", min = 0, max = 100000, value = 3500, step=500),
                         sliderInput("totalRevolvingBalModelFilter", "Total Revolving Balance", min = 0, max = 50000, value = 10000, step=100),
                         sliderInput("avgOpenToBuyModelFilter", "Average Open To Buy", min = 0, max = 100000, value = 0, step=1000),
                         sliderInput("totalAmtChngQ4Q1ModelFilter", "Total Amount Change Q4-Q1", min = 0, max = 5, value = 2.6, step=0.01),
                         sliderInput("totalTransAmtModelFilter", "Total Transaction Amount", min = 0, max = 10000, value = 1900, step=100),
                         sliderInput("totalTransCtModelFilter", "Total Transaction Count", min = 0, max = 150, value = 20, step=1),
                         sliderInput("totalCtChngQ4Q1ModelFilter", "Total Count Change Q4-Q1", min = 0, max = 5, value = 2.33, step=0.01),
                         sliderInput("avgUtilizationRatioModelFilter", "Average Utilization Ratio", min = 0, max = 1, value = 0, step=0.01)
                       )
                ),
                column(6,
                       div(style = "padding-bottom: 20px;", 
                           actionButton("predictButton", "Predict Results")),
                       div(style = "width:200%; height: 100px;", 
                           uiOutput("infoBoxOutput"))
                )
              )
      ),
      #### Tab Model Performance ----------
      tabItem(
        tabName = "modelPerformance",
        fluidRow(box(title = "Model Performance",solidHeader = TRUE, collapsible = TRUE, status = "primary", width = 12,
                     p("This tab shows the predictive performance of the Random Forrest machine learning model we utilize in this application. We trained this model to enable the user that he/she can do predictions based on the input data he/she provides (see previous tab called Model Prediction). The Random Forrest model was the best performing model based on a grid search where we tested Random Forrest and XGBoost models. How we selected and trained the model can be investigated in the model tarining script which we also uploaded as part of the final project submission. Please refer to this other script for more information.")
        )
        ),
        fluidRow(
          # Confusion Matrix Box
          box(width = 6,
              title = HTML("<strong>Confusion Matrix</strong>"),
              solidHeader = TRUE,
              tableOutput("confusionMatrixTable")
          ),
          # Main Performance Metrics Box
          box(
            width = 3,
            title = HTML("<strong>Main Performance Metrics</strong>"),
            solidHeader = TRUE,
            "Accuracy:", textOutput("accuracy"),
            "Recall:", textOutput("recall"),
            "Precision:", textOutput("precision"),
            "ROC_AUC:", textOutput("roc_auc")
          ),
          # Hyperparameters of the Random Forrest model
          box(
            width = 3,
            title = HTML("<strong>Hyperparameters of the model</strong>"),
            solidHeader = TRUE,
            "Number of trees:", textOutput("num_trees"),
            "Mtry:", textOutput("mtry"),
            "Target node size:", textOutput("node_size"),
            "Splitrule:", textOutput("gini")
          ),
          # ROC_AUC Curve Box
          box(width = 12,
              title = HTML("<strong>ROC Curve</strong>"),
              solidHeader = TRUE,
              plotOutput("plotROC_AUC")
          ),
          # Feature Importance  Box
          box(width = 12,
              title = HTML("<strong>Feature Importance of the variables</strong>"),
              solidHeader = TRUE,
              plotOutput("plotFeature_Importance")
          )
        )
      )
      
      
    )
  )
)


# . ----
# Server ----
server <- function(input, output, session) {
  filteredData <- reactive({
    # Ensure all necessary inputs are available before proceeding
    req(input$genderFilter, input$ageFilter, input$incomeFilter, input$educationFilter, 
        input$attritionFlagFilter, input$cardCategoryFilter, input$transactionAmountRange)
    
    # Start with your complete dataset
    filtered <- data
    
    # Filter by Gender if not 'All'
    if (input$genderFilter != "All") {
      filtered <- filtered %>% filter(Gender == input$genderFilter)
    }
    
    # Filter by Age Range
    filtered <- filtered %>%
      filter(Customer_Age >= input$ageFilter[1], Customer_Age <= input$ageFilter[2])
    
    # Filter by Income Category if not 'All'
    if (input$incomeFilter != "All") {
      filtered <- filtered %>% filter(Income_Category == input$incomeFilter)
    }
    
    # Filter by Education Level if not 'All'
    if (input$educationFilter != "All") {
      filtered <- filtered %>% filter(Education_Level == input$educationFilter)
    }
    
    # Filter by Card Category if not 'All'
    if (input$cardCategoryFilter != "All") {
      filtered <- filtered %>% filter(Card_Category == input$cardCategoryFilter)
    }
    
    # Filter by Attrition Flag if not 'All'
    if (input$attritionFlagFilter != "All") {
      filtered <- filtered %>% filter(Attrition_Flag == input$attritionFlagFilter)
    }
    
    # Filter by Total Transaction Amount Range
    filtered <- filtered %>%
      filter(Total_Trans_Amt >= input$transactionAmountRange[1], Total_Trans_Amt <= input$transactionAmountRange[2])
    
    # Return the filtered data
    filtered
  })
  
  ## PLOTS & INFO BOXES ----
  
  # . ----
  ### DESCRIPTION INFO BOXES ----
  
  #### Churn Rate InfoBox ----
  output$churnRateBox <- renderUI({
    data <- filteredData()
    
    churnRate <- data %>%
      group_by(Attrition_Flag) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Churn_Rate = round(Count / sum(Count) * 100, 2))
    
    infoBox(
      title = "Churn Rate",
      value = paste0(churnRate$Churn_Rate[1], "%"),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  ##### Churn Rate Text ----
  output$churnRateText <- renderUI({
    data <- filteredData()
    
    churnRate <- data %>%
      group_by(Attrition_Flag) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Churn_Rate = round(Count / sum(Count) * 100, 2))
    
    # Construct the HTML string with bold churn rate
    HTML(paste(
      "The Churn Rate among the people that fit the applied filters is <strong>",
      churnRate$Churn_Rate[1], "%</strong>"
    ))
  })
  
  
  #### Total Number of Customers InfoBox ----
  output$totalCustomersBox <- renderUI({
    total <- nrow(filteredData())
    infoBox(
      title = "Total Number of Customers",
      value = formatC(total, big.mark = ",", format = "d"),
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  #### Customer Info Boxes ----
  ##### Average Age InfoBox ----
  output$averageAgeBox <- renderUI({
    avgAge <- mean(filteredData()$Customer_Age, na.rm = TRUE)
    infoBox(
      title = "Average Age",
      value = paste0(round(avgAge, 1), " years"),
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  ##### Gender Split InfoBox ----
  output$genderPercentageInfoBox <- renderUI({
    data <- filteredData()
    
    genderTable <- table(data$Gender)
    genderPercentages <- prop.table(genderTable) * 100
    
    malePercentage <- round(genderPercentages["M"], 2)
    femalePercentage <- round(genderPercentages["F"], 2)
    
    combinedPercentage <- paste0(malePercentage, "% / ", femalePercentage, "%")
    
    infoBox(
      title = "Male / Female",
      value = combinedPercentage,
      icon = icon("venus-mars"),
      color = "purple"
    )
  })
  
  ##### Customer Summary Text ----
  output$summaryText <- renderUI({
    # Extract values from the reactive context
    totalCustomers <- nrow(filteredData())
    avgAge <- mean(filteredData()$Customer_Age, na.rm = TRUE)
    genderTable <- table(filteredData()$Gender)
    genderPercentages <- prop.table(genderTable) * 100
    
    # Round and format the values
    malePercentage <- round(genderPercentages["M"], 2)
    femalePercentage <- round(genderPercentages["F"], 2)
    formattedTotal <- formatC(totalCustomers, big.mark = ",", format = "d")
    formattedAvgAge <- round(avgAge, 1)
    
    # Construct the summary sentence with bold numbers
    HTML(paste(
      "With the applied filters, the total Number of Customers is equal to", 
      "<strong>", formattedTotal, "</strong>",
      "with an average age of", 
      "<strong>", formattedAvgAge, "years</strong>",  # Close the tag correctly
      "and a Male / Female Distribution of", 
      "<strong>", malePercentage, "%</strong> /", 
      "<strong>", femalePercentage, "%</strong>"  # Close the tag correctly
    ))
  })
  
  #### Transaction Info Boxes ----
  ##### Total Transactions Amount ----
  output$totalTransactionSum <- renderInfoBox({
    sumTotal <- sum(filteredData()$Total_Trans_Amt, na.rm = TRUE)
    infoBox(
      title = "Total Transactions Amount",
      paste("$", formatC(sumTotal, format = "f", big.mark = ",", digits = 2)),
      icon = icon("shopping-cart"),
      color = "purple"  # Adjust the color as needed
    )
  })
  
  ##### Total Count of All Transactions ----
  output$totalTransactionCount <- renderInfoBox({
    totalCount <- nrow(filteredData())
    infoBox(
      title = "Total Count of All Transactions",
      paste("", formatC(totalCount, big.mark = ",")),
      icon = icon("calculator"),
      color = "blue"  # Adjust the color as needed
    )
  })
  
  ##### Average Amount per Transaction ----
  output$averageTransactionAmount <- renderInfoBox({
    avgAmount <- mean(filteredData()$Total_Trans_Amt, na.rm = TRUE)
    infoBox(
      title = "Average Amount per Transaction",
      paste("$", formatC(avgAmount, format = "f", big.mark = ",", digits = 2)),
      icon = icon("dollar-sign"),
      color = "green"  # Adjust the color as needed
    )
  })
  
  ##### Transactions Summary Text ----
  # Combined Reactive Text for Transaction Summaries
  output$transactionSummaryText <- renderUI({
    # Get the data from the reactive source
    data <- filteredData()
    
    # Calculate the required statistics
    sumTotal <- sum(data$Total_Trans_Amt, na.rm = TRUE)
    totalCount <- nrow(data)
    avgAmount <- mean(data$Total_Trans_Amt, na.rm = TRUE)
    
    # Format the statistics
    formattedSumTotal <- formatC(sumTotal, format = "f", big.mark = ",", digits = 2)
    formattedTotalCount <- formatC(totalCount, big.mark = ",")
    formattedAvgAmount <- formatC(avgAmount, format = "f", big.mark = ",", digits = 2)
    
    # Construct the combined summary sentence with HTML for bold numbers
    HTML(paste(
      "With the applied filters, the Total Transactions Amount is ", "<strong>", "$", formattedSumTotal, "</strong>",
      ", the Total Count of All Transactions ", "<strong>", formattedTotalCount, "</strong>",
      ", and the Average Amount per Transaction is ", "<strong>", "$", formattedAvgAmount, "</strong>"
    ))
  })
  
  
  # . ----
  ### RAW DATA PLOTS ----
  
  #### Description Table ----
  output$descTable <- DT::renderDataTable({
    DT::datatable(Explanation, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  #### Bivariate Relationship Plot ----
  observe({
    # Directly calling filteredData() here to update variable selection inputs
    updateSelectInput(session, "xvar", choices = names(filteredData()))
    updateSelectInput(session, "yvar", choices = names(filteredData()))
  })
  
  # Generate the plot based on selected variables
  output$varPlot <- renderPlot({
    # Ensure different variables are selected
    if (input$xvar != input$yvar && all(c(input$xvar, input$yvar) %in% names(filteredData()))) {
      
      # Calculate frequency of each x-y combination
      df_freq <- filteredData() %>%
        group_by(!!sym(input$xvar), !!sym(input$yvar)) %>%
        summarise(Frequency = n(), .groups = 'drop') %>%
        ungroup()
      
      # Create the scatterplot using frequency for point size
      ggplot(df_freq, aes_string(x = input$xvar, y = input$yvar, size = "Frequency")) +
        geom_point(aes(color = as.numeric(!!sym(input$xvar))), alpha = 0.6) +
        scale_size_continuous(range = c(1, 10), guide = "none") +
        scale_color_gradient(low = "lightblue", high = "darkblue") +  # Use a continuous color scale
        labs(x = input$xvar, y = input$yvar, title = paste("Scatter Plot of", input$xvar, "vs", input$yvar)) +
        theme_minimal()
      
    } else {
      # Display a message if the same variable is selected for both axes
      plot.new()
      title(main = "Please select two different variables.", col.main = "red")
    }
  })
  
  #### Table ---- 
  output$dataTable <- DT::renderDataTable({
    
    DT::datatable(
      filteredData(),
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        lengthMenu = list(c(10, 25, 50, 100), c('10 entries', '25 entries', '50 entries', '100 entries')),
        pageLength = 10,
        searching = TRUE,
        fixedColumns = TRUE,
        stateSave = FALSE
      ),
      class = "display nowrap"
    )
  })
  
  #### Heatmap ----
  output$heatmapPlot <- renderPlot({
    # Obtain the filtered data by invoking the reactive expression
    df <- filteredData()
    
    # Select only numeric data and exclude CLIENTNUM and columns with Naive_Bayes
    numericData <- df %>%
      select_if(is.numeric) %>%
      select(-CLIENTNUM, -matches("Naive_Bayes"))
    
    # Calculate the correlation matrix, which is the basis of the heatmap
    corMatrix <- cor(numericData, use = "pairwise.complete.obs")
    
    # Melt the correlation matrix into a long format for ggplot
    corMatrixLong <- reshape2::melt(corMatrix)
    
    # Create the heatmap
    ggplot(corMatrixLong, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "salmon", mid = "white", high = "darkblue", midpoint = 0, space = "Lab") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), 
        axis.text.y = element_text(angle = 45, vjust = 1, size = 12), 
        legend.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
        legend.text = element_text(size = 12) , 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
      labs(title = "Correlation Heatmap", 
           x = '', 
           y = '', 
           fill = "Correlation")
  })
  
  
  
  # . ----
  ### DEMORAGPHIC PLOTS ----    
  
  #### Tab Churn Rate Overview ----
  ##### Churn Rate Plot ----
  output$churnRatePlot <- renderPlot({
    data_summary <- filteredData() %>%
      group_by(Attrition_Flag) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Fraction = Count / sum(Count),
             Label = paste0(Attrition_Flag, " (", round(Fraction*100), "%)"))
    
    ggplot(data_summary, aes(x = Attrition_Flag, y = Fraction, fill = Attrition_Flag)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),  
        axis.title.x = element_text(size = 12),               
        axis.title.y = element_text(size = 12),               
        axis.text.x = element_text(size = 12),                
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12)
      ) + 
      scale_fill_brewer(palette = "Paired") +
      labs(title = "Churn Rate Overview") +
      geom_text(aes(label = Label), position = position_dodge(width = 0.5), vjust = -0.5)
  })
  
  #### Tab Age & Gender ----
  ##### Age Plot----
  output$ageDistribution <- renderPlot({
    # Ensure the filtered data is stored in a local variable within the reactive context
    data_for_plot <- filteredData()
    
    # Create a copy of the dataset with the gender set to "Both"
    combined_data <- data_for_plot
    combined_data$Gender <- "All"
    
    # Bind the original data with the combined_data
    data_with_combined <- rbind(data_for_plot, combined_data)
    
    # Plot the boxplot
    ggplot(data_with_combined, aes(x = Gender, y = Customer_Age, fill = Gender)) + 
      geom_boxplot() +
      scale_fill_brewer(palette = "Paired") + # Optional: Use a color palette for visual distinction
      theme_minimal() + 
      theme(legend.position = "none", 
            plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) + 
      labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")
  })
  
  ##### Gender Plot----
  output$genderDistribution <- renderPlot({
    # Prepare the summary data
    data_summary <- filteredData() %>%
      count(Gender) %>%
      mutate(Percentage = n / sum(n),
             Label = paste0(Gender, "\n", n, " (", round(Percentage * 100), "%)"))
    
    # Create the stacked bar chart
    ggplot(data_summary, aes(x = "", y = Percentage, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Labels for absolute and relative numbers
      scale_fill_brewer(palette = "Paired") +  # Apply the "Paired" palette
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) + 
      labs(title = "Gender Distribution", x = "Gender", fill = "Gender")
  })
  
  #### Tab Education Level ----
  ##### Education Level Plot----
  output$educationLevelPlot <- renderPlot({
    
    # Define the order of education levels as they appear from left to right in the screenshot
    ordered_levels <- c("Uneducated", 
                        "High School", 
                        "College", 
                        "Graduate", 
                        "Post-Graduate", 
                        "Doctorate", 
                        "Unknown")
    
    # Create the plot with the ordered factor levels
    ggplot(filteredData(), aes(x = factor(Education_Level, levels = ordered_levels))) + 
      geom_bar(fill = "lightblue", stat = "count") + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            axis.ticks = element_blank()
            ) +  # Adjust axis title size
      labs(title = "Education Level Distribution", x = "Education Level", y = "Count")
  })
  
  ##### Income by Education Plot----
  output$incomeEducationPlot <- renderPlot({
    # Fetch the data and apply the specified ordering to both factors
    data <- filteredData()
    
    # Set the levels manually, with "Less than $40K" at the bottom of the axis
    data$Income_Category <- factor(data$Income_Category, 
                                   levels = c("Less than $40K",
                                              "$40K - $60K",
                                              "$60K - $80K",
                                              "$80K - $120K",
                                              "$120K +", 
                                              "Unknown"))
    
    ordered_levels <- c("Uneducated", 
                        "High School", 
                        "College", 
                        "Graduate", 
                        "Post-Graduate", 
                        "Doctorate", 
                        "Unknown")
    data$Education_Level <- factor(data$Education_Level, levels = ordered_levels)
    
    # Count the combinations of Education_Level and Income_Category
    count_data <- data %>%
      count(Education_Level, Income_Category) %>%
      tidyr::spread(key = Income_Category, value = n, fill = 0) # Convert to wide format with counts
    
    # Convert the data back to long format for ggplot, ensuring the factor levels are preserved
    melted_data <- tidyr::gather(count_data, key = "Income_Category", value = "Count", -Education_Level)
    
    # Ensure the ordering is maintained when converting to long format
    melted_data$Income_Category <- factor(melted_data$Income_Category, levels = levels(data$Income_Category))
    melted_data$Education_Level <- factor(melted_data$Education_Level, levels = ordered_levels)
    
    # Plotting
    ggplot(melted_data, aes(x = Education_Level, y = Income_Category, fill = Count)) +
      geom_tile() + # Creates the heatmap tiles
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12), 
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
            axis.ticks = element_blank()) +  
      labs(title = "Count of Income Categories by Education Level", x = "Education Level", y = "Income Category", fill = "Count")
  })
  
  #### Tab Income ----
  
  ##### Churn per Income Plot---- 
  output$incomeCategoryChurnPlot <- renderPlot({
    # Create an ordered factor for Income_Category
    ordered_income <- factor(filteredData()$Income_Category, 
                             levels = c("Less than $40K", 
                                        "$40K - $60K", 
                                        "$60K - $80K", 
                                        "$80K - $120K", 
                                        "$120K +", 
                                        "Unknown"))
    
    # Summarise and mutate the data for plotting
    data_summary <- filteredData() %>%
      mutate(Income_Category = ordered_income) %>%
      group_by(Income_Category, Attrition_Flag) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Label = paste0(Count, " (", round(Percentage, 1), "%)")) %>%
      ungroup()  # Ungroup the data for plotting
    
    # Plot the data using a stacked bar chart
    ggplot(data_summary, aes(x = Income_Category, y = Count, fill = Attrition_Flag)) +
      geom_bar(stat = "identity", position = "stack") +  # Stack the bars
      geom_text(aes(label = Label, y = Count), 
                position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Center labels in the bars
      scale_fill_brewer(palette = "Paired") +  # Apply the "Paired" palette
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),               
        axis.title.y = element_text(size = 12),               
        axis.text.y = element_text(size = 12),
        legend.position = "none",  # Remove the legend
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Title bold and centered
      ) + 
      labs(title = "Churn Rate by Income Category", x = "Income Category", y = "Count")  # Set axis labels and plot title
  })
  
  
  
  
  #### Tab Family ----
  ##### Dependent Count Plot----
  output$dependentCountPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = factor(Dependent_count), fill = Attrition_Flag)) +  # Use Attrition_Flag for fill
      geom_bar(position = "stack") +  # Stack the bars
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +  
      labs(title = "Dependent Count Distribution by Attrition Flag",
           x = "Dependent Count",
           y = "Count") +
      scale_fill_brewer(palette = "Paired")  # Optional: Use a color palette for better distinction
  })
  
  ##### Marital Status Plot----
  output$maritalStatusPlot <- renderPlot({
    data <- filteredData() %>%
      group_by(Marital_Status, Attrition_Flag) %>%  # Group by both Marital_Status and Attrition_Flag
      summarise(Count = n(), .groups = 'drop')  # Count occurrences
    
    ggplot(data, aes(x = Marital_Status, y = Count, fill = Attrition_Flag)) +  # Fill bars based on Attrition_Flag
      geom_bar(stat = "identity", position = "stack") +  # Use stacked bar chart
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) + 
      labs(title = "Marital Status Distribution by Attrition Flag",
           x = "Marital Status",
           y = "Count") +
      scale_fill_brewer(palette = "Paired")  # Use a color palette for better distinction
  })
  
  
  
  
  
  ##### Marital Status by Dependent Plot ----
  output$dependentMaritalStatusPlot <- renderPlot({
    # Fetch the data
    data <- filteredData()
    
    # Assuming "Dependent_count" is already in an appropriate format (numeric or factor)
    # You might need to convert or order "Marital_Status" depending on your dataset specifics
    data$Marital_Status <- factor(data$Marital_Status, 
                                  levels = c("Single", "Married", "Divorced", "Unknown"))
    
    # Count the combinations of Dependent_count and Marital_Status
    count_data <- data %>%
      count(Dependent_count, Marital_Status) %>%
      tidyr::spread(key = Marital_Status, value = n, fill = 0) # Convert to wide format with counts
    
    # Convert the data back to long format for ggplot, ensuring the factor levels are preserved
    melted_data <- tidyr::gather(count_data, key = "Marital_Status", value = "Count", -Dependent_count)
    
    # Ensure the ordering is maintained when converting to long format
    melted_data$Marital_Status <- factor(melted_data$Marital_Status, levels = c("Single", "Married", "Divorced", "Unknown"))
    melted_data$Dependent_count <- factor(melted_data$Dependent_count) # Ensure it's treated as a factor
    
    # Plotting
    ggplot(melted_data, aes(x = Dependent_count, y = Marital_Status, fill = Count)) +
      geom_tile() + # Creates the heatmap tiles
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
            plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) + 
      labs(title = "Count of Marital Status by Dependent Count", x = "Dependent Count", y = "Marital Status", fill = "Count")
  })
  
  # . ----
  ### FINANCIAL PLOTS ----
  
  #### Tab Activity & Utilization ----
  ##### Transaction Amount Plots ----
  output$totalTransAmtPlot <- renderPlot({
    filteredData() %>% 
      ggplot(aes(x = Total_Trans_Amt)) + 
      geom_histogram(binwidth = input$binWidthTotalTransAmt, fill = "steelblue", color = "white") + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Total Transaction Amount Distribution", x = "Total Transaction Amount", y = "Count")
  })
  
  ##### Credit Utilization ----
  output$creditUtilizationPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Avg_Utilization_Ratio)) +
      geom_histogram(binwidth = input$binWidthCreditUtilization, fill = "steelblue", color = "white") + 
      labs(title = "Credit Utilization Trends",
           x = "Average Utilization Ratio", 
           y = "Count of Customers") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
  })
  
  #### Total Transaction Count Plot ----
  output$totalTransCtPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = Total_Trans_Ct)) +
      geom_histogram(binwidth = input$binWidthTransCt, fill = "steelblue", color = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Total Transaction Count Distribution", x = "Total Transaction Count", y = "Frequency")
  })
  
  #### Tab Card Category ---- 
  ##### Card Category Plot ----
  output$cardCategoryPlot <- renderPlot({
    # Prepare the summary data
    data_summary <- filteredData() %>%
      count(Card_Category) %>%  # Count instances of each Card_Category
      mutate(Percentage = n / sum(n) * 100,  # Calculate the percentage of each category
             Label = paste0(n, " (", round(Percentage, 1), "%)")) %>%  # Create label with absolute and relative numbers
      arrange(desc(n))  # Sort the data in descending order based on count
    
    # Create the bar chart
    ggplot(data_summary, aes(x = Card_Category, y = n, fill = Card_Category)) +
      geom_bar(stat = "identity") +  # Use identity to use n values directly
      geom_text(aes(label = Label, y = n), 
                vjust = 1.1,  # Right-justify the text to align it at the end of the bars
                nudge_y = -0.5,  # Nudge the text a little to the right of the bar
                size = 4,  # Adjust the size as necessary
                color = "black") +  # Choose a color that contrasts well with the bar colors
      scale_fill_brewer(palette = "Paired") +  # Use a color palette suitable for categorical data
      coord_flip() +  # Flip the chart to make horizontal bars
      theme_minimal() +
      theme(
        legend.position = "none",  # Remove the legend
        plot.title = element_text(size = 16, face = "bold"),  
        axis.title.x = element_text(size = 12),               
        axis.title.y = element_text(size = 12),               
        axis.text.x = element_text(size = 12),                
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12)
      ) +
      labs(title = "Card Category Distribution", x = "Card Category", y = "Count")  # Label axes and plot
  })
  
  
  
  
  ##### Churn per Card Category Plot ----
  output$cardCategoryChurnPlot <- renderPlot({
    filteredData() %>%
      group_by(Card_Category, Attrition_Flag) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      group_by(Card_Category) %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Label = paste0(Count, " (", round(Percentage, 1), "%)")) %>%
      ungroup() %>%
      ggplot(aes(x = Card_Category, y = Percentage, fill = Attrition_Flag, label = Label)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(position = position_stack(vjust = 0.5), color = "white", size = 5) +
      scale_fill_manual(values = c("Existing Customer" = "steelblue", "Attrited Customer" = "lightblue")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),  # Set y axis labels to size 12
            plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            axis.ticks = element_blank(),
            panel.grid = element_blank(), 
            strip.background = element_blank()) + 
      labs(title = "Churn Rate by Card Category", x = "Card Category", y = "Percentage", fill = "Attrition Flag")
  })
  
  
  
  #### Tab Customer Lifetime ----
  ##### Months on Book Plot ----
  output$monthsOnBookPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = Months_on_book)) +
      geom_histogram(binwidth = input$binWidthMonth, fill = "steelblue", color = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Months on Book Distribution", x = "Months on Book", y = "Frequency")
  })
  
  ##### Customer Lifetime Value ----
  output$clvPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Total_Trans_Amt)) +
      geom_histogram(binwidth = input$binWidthCLV, fill = "steelblue", color = "white") + 
      labs(title = "Customer Lifetime Value (CLV) Distribution",
           x = "Total Transaction Amount (Proxy for CLV)", 
           y = "Count of Customers") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.caption = element_text(hjust = 0.5, color = "black", size = 12)) +
      labs(caption = "Total Transaction Amount serves as proxy for CLV as more detailed Customer Data is unavailable. Higher Transaction Amounts indicate a greater Customer Value over time.")
  })
  
  ##### Months on Book vs CLV ----
  output$monthsOnBookVsClvPlot <- renderPlot({
    filtered <- filteredData()  
    ggplot(filtered, aes(x = Months_on_book, y = Total_Trans_Amt)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      scale_color_brewer(palette = "Paired") +
      labs(title = "Scatter Plot: Months on Book vs CLV",
           x = "Months on Book",
           y = "Customer Lifetime Value (CLV)")
  })
  
  ##### Product Analysis ----
  output$productAnalysisPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = as.factor(Total_Relationship_Count))) +
      geom_bar(fill = "steelblue") + 
      labs(title = "Product Holdings per Customer",
           x = "Number of Products Held", 
           y = "Count of Customers") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12))
  })
  
  #### Tab Total Relationship Count ----
  ##### Total Relationship Count Plot ----
  output$totalRelationshipCountPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = Total_Relationship_Count)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Total Relationship Count Distribution", x = "Total Relationship Count", y = "Count")
  })
  
  #### Tab Credit Overview ----
  ##### Credit Limit Plot ----
  output$creditLimitPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = Credit_Limit)) +
      geom_density(fill = "steelblue", color = "white", adjust = 1) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Credit Limit Distribution", x = "Credit Limit", y = "Density")
  })
  
  
  ##### Total Revolving Bal Plot ----
  output$totalRevolvingBalPlot <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = Total_Revolving_Bal)) +
      geom_histogram(binwidth = input$binWidthBal, fill = "steelblue", color = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(title = "Total Revolving Bal Distribution", x = "Total Revolving Bal", y = "Frequency")
  })
  
  #### Tab Avg Open to Buy ----
  ##### Avg Open to Buy Plot ----
  
  output$avgOpenToBuyPlot <- renderPlot({
    # Obtain the filtered data
    df <- filteredData()
    
    # Calculate mean and median for Avg_Open_To_Buy
    mean_avg_open_to_buy <- mean(df$Avg_Open_To_Buy, na.rm = TRUE)
    median_avg_open_to_buy <- median(df$Avg_Open_To_Buy, na.rm = TRUE)
    
    # Create the histogram with ggplot
    gg <- ggplot(df, aes(x = Avg_Open_To_Buy)) +
      geom_histogram(binwidth = input$binWidthAvgOpenToBuy, fill = "lightblue", color = "white") +  # Adjusted binwidth for more detail
      geom_vline(xintercept = mean_avg_open_to_buy, color = "steelblue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = median_avg_open_to_buy, color = "darkblue", linetype = "dashed", size = 1) +
      theme_minimal() +
      labs(title = "Average Open to Buy Distribution",
           x = "Average Open to Buy",
           y = "Frequency") +
      theme(plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.x = element_text(size = 12),                
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.subtitle = element_text(hjust = 0.5)) +
      annotate("text", x = mean_avg_open_to_buy, y = Inf, label = paste("Mean:", round(mean_avg_open_to_buy, 2)), vjust = 2, color = "steelblue") +
      annotate("text", x = median_avg_open_to_buy, y = Inf, label = paste("Median:", round(median_avg_open_to_buy, 2)), vjust = 1, color = "darkblue")
    
    # Return the plot
    gg
  })
  
  
  #### Tab Spending Patterns ----
  ##### Spending Patterns ----
  output$spendingPatternsPlot <- renderPlot({
    data <- filteredData()
    
    # Define the desired order of Income_Category
    income_order <- c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown")
    
    # Convert Income_Category to a factor with the desired order
    data$Income_Category <- factor(data$Income_Category, levels = income_order)
    
    ggplot(data, aes(x = Income_Category, y = Total_Trans_Amt, color = Income_Category)) +
      geom_boxplot() + 
      labs(title = "Spending Patterns by Income Category",
           x = "Income Category", 
           y = "Total Transaction Amount") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            plot.title = element_text(size = 16, face = "bold"),  
            axis.title.x = element_text(size = 12),               
            axis.title.y = element_text(size = 12),               
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)) +
      scale_color_brewer(palette = "Paired")
  })
  
  
 ### MODEL ----
  #### Model Predictions ------------
  
  observeEvent(input$predictButton, {
    # Ensure that all input values are being captured reactively
    df <- data.frame(
      Customer_Age = input$ageModelFilter,
      Gender = input$genderModelFilter,
      Dependent_count = input$dependentCountModelFilter,
      Education_Level = input$educationModelFilter,
      Marital_Status = input$maritalModelFilter,
      Income_Category = input$incomeModelFilter,
      Card_Category = input$cardCategoryModelFilter,
      Months_on_book = input$monthsOnBookModelFilter,
      Total_Relationship_Count = input$totalRelationshipCountModelFilter,
      Months_Inactive_12_mon = input$monthsInactive12MonModelFilter,
      Contacts_Count_12_mon = input$contactsCount12MonModelFilter,
      Credit_Limit = input$creditLimitModelFilter,
      Total_Revolving_Bal = input$totalRevolvingBalModelFilter,
      Avg_Open_To_Buy = input$avgOpenToBuyModelFilter,
      Total_Amt_Chng_Q4_Q1 = input$totalAmtChngQ4Q1ModelFilter,
      Total_Trans_Amt = input$totalTransAmtModelFilter,
      Total_Trans_Ct = input$totalTransCtModelFilter,
      Total_Ct_Chng_Q4_Q1 = input$totalCtChngQ4Q1ModelFilter,
      Avg_Utilization_Ratio = input$avgUtilizationRatioModelFilter
    )
    
    # Convert character variables to factors if required
    df <- df %>% mutate(across(where(is.character), as.factor))
    
    # Predict using the best_rf_model
    prediction <- ifelse(predict(best_rf_model, new_data = df) == 0,
                         'The customer is likely to stay',
                         'The customer is likely to churn')
    
    # Update the UI with the new prediction
    output$infoBoxOutput <- renderUI({
      infoBox(
        title = "Prediction",
        value = prediction,
        width = 100
      )
    })
  }, ignoreNULL = FALSE)
  
  #### Model Results ------
  
  # Render the confusion matrix table for the UI
  # The confusion matrix was created at the beginning of the script (see above).
  output$confusionMatrixTable <- renderTable({
    confusion_matrix
  }, rownames = TRUE) # We set rownames = TRUE to display row names on the table
  
  # We calculated the metrics in the script in which we also did the model training in 
  # In shiny we just display the predictive performance results which we obtained based on the test_data
  # If you want to recalculate the performance metrics, please run the script in which we did the model training
  # which we provide as well.
  # The performance metrics were computed based on the test data. 
  output$accuracy <- renderText({
    sprintf("%.2f%%", 0.957 * 100)
  })
  
  output$recall <- renderText({
    sprintf("%.2f%%", 0.990 * 100)
  })
  
  output$precision <- renderText({
    sprintf("%.2f%%", 0.961 * 100)
  })
  
  output$roc_auc <- renderText({
    sprintf("%.2f%%", 0.987 * 100)
  })
  
  # We provide the values for the hyperparameters of the utilized model here
  # These hyperparameters were found in the model training script.
  # Please refer to that script for more information.
  output$num_trees <- renderText({
    sprintf("433")
  })
  
  output$mtry <- renderText({
    sprintf("5")
  })
  
  output$node_size <- renderText({
    sprintf("3")
  })
  
  output$gini <- renderText({
    sprintf("Gini")
  })
  
  # Plot ROC Curve
  output$plotROC_AUC <- renderPlot({
    plot(roc_curve, main="ROC_AUC Curve", col="#1c61b6")
  })
  
  # Plot Feature Importance
  output$plotFeature_Importance <- renderPlot({
    plot(feature_importance, main="Feature Importance of the variables", col="#1c61b6")
  })
  
  
  
}

# Run ----
shinyApp(ui, server)

