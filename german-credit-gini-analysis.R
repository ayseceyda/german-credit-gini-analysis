getwd()
data <- read.csv("gc.csv")

# add libraries

library(MLmetrics)
library(sqldf)
library(dplyr)
library(knitr)

str(data) # check types

data = data[,-1]  # remove "id" column

# NA checking

sum(is.na(data$Saving.accounts)) #saving acc column has 183 null values
sum(is.na(data$Checking.account)) #checking acc column has 394 null values

data$Saving.accounts <- as.character(data$Saving.accounts)
data$Saving.accounts[is.na(data$Saving.accounts)] <- "none" # when you see NA transform it to "none"

data$Checking.account <- as.character(data$Checking.account)
data$Checking.account[is.na(data$Checking.account)] <- "none" # when you see NA transform it to "none"

# checking for NAs and column 5 & 6

sum(is.na(data))

# change Risk features' values from (1 and 2) to (0 and 1)

data$Risk[data$Risk == "good"]  <- 0
data$Risk[data$Risk == "bad"]  <- 1
data$Risk = as.numeric(data$Risk) # transform target risks' type chr to num
str(data)

# split the data into train and test sets

set.seed(12345)
train <- sample(1:nrow(data), nrow(data) * 0.8 ,replace = FALSE)
data_train <- data[train,]
data_test <- data[-train,]

################################## glm #####################################################

glm.model <- glm(Risk ~ ., family = binomial, data = data_train)

glm.pred <- predict(glm.model, data_test, type="response")

test_df <- data.frame(glm.pred, data_test$Risk)

names(test_df)[1] <- "Predicted_Risk"
names(test_df)[2] <- "Actual_Risk"
test_df

MLmetrics_gini <- Gini(test_df$Predicted_Risk, test_df$Actual_Risk)
MLmetrics_gini

### Calculating AUC and Gini using sqldf ###

# Predicted_Risk on DESC order

query_DESC=sqldf("select * from test_df order by Predicted_Risk DESC")

# Adding "Row_Number" column 

query_Row_Number=sqldf("select Actual_Risk, Predicted_Risk, ROW_NUMBER() OVER (ORDER BY Predicted_Risk DESC) as Row_Number FROM query_DESC")

# Adding "Rank" column 

query_Rank_Number <- query_Row_Number %>%
  group_by(Actual_Risk) %>%
  mutate(Rank = row_number())

query_Rank_Number

# gini = auc * 2 - 1

attach(query_Rank_Number)

query_AUC = sqldf("WITH POSITIVE_SCORES AS (
  select Predicted_Risk as p_pos
  from query_Rank_Number
  where Actual_Risk = 1
),

NEGATIVE_SCORES AS (
  select Predicted_Risk as p_neg
  from query_Rank_Number
  where Actual_Risk = 0
)

select avg(case 
    when p_pos > p_neg then 1 
    when p_pos = p_neg then 0.5 
    else 0 
    end) as auc_score
from
POSITIVE_SCORES
cross join
NEGATIVE_SCORES")

attach(query_AUC)
auc_score
sqldf_gini = (auc_score*2) - 1
sqldf_gini

### Comparison from MLmetrics' Gini and sqldf's Gini ###

cat("MLmetrics_gini: ", MLmetrics_gini)
cat("sqldf_gini: ", sqldf_gini)
