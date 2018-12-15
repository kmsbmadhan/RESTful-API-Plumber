
library(mltools)
library(caTools)
library(DMwR)
library(data.table)
claim <- read.csv("claims.csv", header = TRUE)

#Feature Engineered 
claim_sub <- subset(claim, select = -c(Month,WeekOfMonth,DayOfWeek,WeekOfMonth,Make,DayOfWeekClaimed,MonthClaimed,WeekOfMonthClaimed,
                                       PolicyType,VehiclePrice,PolicyNumber,RepNumber,DriverRating,Days_Policy_Claim,PastNumberOfClaims,AgeOfVehicle,
                                       AgeOfPolicyHolder,PoliceReportFiled,WitnessPresent,NumberOfSuppliments,NumberOfCars,Year,Days_Policy_Accident))
                                       
                                      

#Split the dataset
set.seed(123)
split = sample.split(claim_sub$FraudFound_P, SplitRatio = 0.70)
claim1_train = subset(claim_sub, split == TRUE)
claim1_test = subset(claim_sub, split == FALSE)

names(claim1_train)

#OHE for test set
claim1_test <- one_hot(as.data.table(claim1_test))

#Sample the train dataset

claim1_train$FraudFound_P <- as.factor(claim1_train$FraudFound_P)
table(claim1_train$FraudFound_P)
??SMOTE

claim_Smote <- SMOTE(FraudFound_P ~ ., claim1_train, perc.over = 600,perc.under=118)

table(claim_Smote$FraudFound_P)

#One hot encoding
claim_Smote$FraudFound_P <- as.integer(claim_Smote$FraudFound_P)
claim_cat <- one_hot(as.data.table(claim_Smote))
claim_cat <- as.data.frame(claim_cat)
table(claim_cat$FraudFound_P)



# Encoding the target feature as factor only after one-hot encoding
claim_Smote$FraudFound_P <- as.factor(claim_Smote$FraudFound_P)
claim_Smote$Make <- as.factor(claim_Smote$Make)
typeof(claim_Smote$FraudFound_P)
levels(claim_Smote$FraudFound_P) <- c('0','1')
levels(claim_Smote$FraudFound_P)

names(claim_cat)
##
set.seed(42)
training_rows <- sample(1:nrow(claim_Smote), size = floor(0.7*nrow(claim_Smote)))
train_df <- claim_Smote[training_rows, ]
test_df <- claim_Smote[-training_rows, ]

claim_glm <- glm(FraudFound_P ~ ., 
                   data = train_df, family = binomial(link = "logit"))

test_predictions <- predict(claim_glm, newdata = test_df, type = "response") >= 0.5
test_actuals <- test_df$FraudFound_P == 1
accuracy <- table(test_predictions, test_actuals)
print(accuracy)
print(paste0("Accuracy: ", round(100 * sum(diag(accuracy))/sum(accuracy), 2), "%"))

library(plumber)
saveRDS(claim_glm, file = "model.Rds", compress = TRUE)

names(claim_Smote)
