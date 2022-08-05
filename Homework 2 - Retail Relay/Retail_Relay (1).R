library("readxl")

defaultW <- getOption("warn") 

options(warn = -1) 

testFile1 <- file.choose()
retailTest <- read_excel(testFile1, sheet="relay test.csv",col_names = TRUE)

trainFile1 <- file.choose()
retailTrain <- read_excel(trainFile1, sheet = "relay train.csv", col_names=TRUE)

# data exploration
names(retailTrain)

head(retailTrain)

summary(retailTrain)


# converting categorical variables to factors
retailTrain$retained <- as.factor(retailTrain$retained) 
retailTrain$paperless <- as.factor(retailTrain$paperless)
retailTrain$refill <- as.factor(retailTrain$refill)
retailTrain$doorstep <- as.factor(retailTrain$doorstep)
retailTrain$train <- as.factor(retailTrain$train)

# converting test data 
retailTest$retained <- as.factor(retailTest$retained) 
retailTest$paperless <- as.factor(retailTest$paperless)
retailTest$refill <- as.factor(retailTest$refill)
retailTest$doorstep <- as.factor(retailTest$doorstep)
retailTest$train <- as.factor(retailTest$train)


#Logistic Model Function
logisticmodel <- function(equation, test, train){
  
  glm.fit<- glm(equation
              , data=train
              , family = binomial())
  
  print(summary(glm.fit))
  

  # predict retention
  pred_prob <- predict(glm.fit, test, type="response")
  
  test$pretain <- ifelse(pred_prob >= 0.5, "1", "0")
  
  # confusion matrix
  ctab_test <- table(test$retained, test$pretain)
  
  print(ctab_test)
  print("")
  # Accuracy in Test dataset
  Accuracy <- sum(diag(ctab_test))/sum(ctab_test)*100
  Accuracy <- paste("Hit Rate:",  as.character(round(Accuracy, 4)))
  
  # Recall: True Positive Rate
  Recall <- (ctab_test[2, 2]/sum(ctab_test[2, ]))*100
  RecallResults <- paste("Recall:",  as.character(round(Recall, 4)))
  
  # True Negative Rate
  TNR <- (ctab_test[1, 1]/sum(ctab_test[1, ]))*100
  TNR <- paste("True Negative Rate:",  as.character(round(TNR, 4)))
  
  # Precision
  Precision <- (ctab_test[2, 2]/sum(ctab_test[, 2]))*100
  PrecisionResults <- paste("Precision:",  as.character(round(Precision, 4)))
  
  # F Score
  F_Score <- (2 * Precision * Recall / (Precision + Recall))/100
  F_Score <- paste("F Score:",  as.character(round(F_Score, 4)))

  Results <- paste(Accuracy, RecallResults, TNR, PrecisionResults, F_Score, sep = "\n")
  cat(Results[1])
}

# logistic model For Question 1
#question 1 equation
variables1 <- c("esent", "eclickrate", "avgorder", "ordfreq", "paperless", "refill", "doorstep")
q1 <- as.formula(paste("retained",paste(variables1, collapse = " + "), sep =" ~ "))

q1
logisticmodel(q1, test = retailTest, train = retailTrain)

# logistic model for Question 2
#question 2 equation
variables2 <- c("avgorder", "ordfreq", "paperless", "refill", "doorstep")
q2 <- as.formula(paste("retained",paste(variables2, collapse = " + "), sep =" ~ "))

q2
logisticmodel(q2, test = retailTest, train = retailTrain)

# logistic model for Question 3.

q3 <- "retained ~ esent"
q3
logisticmodel(q3, test = retailTest, train = retailTrain)


# logisitc model for Question 4
# dummy variable for weekend
weekendDays <- c("Friday", "Saturday", "Sunday")
retailTest$isWeekend <- as.factor(ifelse(retailTest$favday %in% weekendDays, "1", "0"))
retailTrain$isWeekend <- as.factor(ifelse(retailTrain$favday %in% weekendDays, "1", "0"))

variables4 <- c("esent", "eclickrate", "avgorder", "ordfreq", "paperless", "refill", "doorstep", "isWeekend")
q4 <- as.formula(paste("retained",paste(variables4, collapse = " + "), sep =" ~ "))

q4
logisticmodel(q4, test = retailTest, train = retailTrain)

# transformation on esent 
# question 5

retailTrain$esentLogistic <-1/(1+exp(retailTrain$esent))
retailTest$esentLogistic <- 1/(1+exp(retailTest$esent))

retailTrain$esentLog <-log(retailTrain$esent)
retailTest$esentLog <- log(retailTest$esent)

# removing neg inf from data
retailTrain[retailTrain == -Inf] <- 0
retailTest[retailTest == -Inf] <- 0

# box plots of data
boxplot(retailTrain$esent)
boxplot(retailTrain$esentLogistic)
boxplot(retailTrain$esentLog)


variables4Mod <- c("esentLog", "eclickrate", "avgorder", "ordfreq", "paperless", "refill", "doorstep", "isWeekend")
q4Mod <- as.formula(paste("retained",paste(variables4Mod, collapse = " + "), sep =" ~ "))

logisticmodel(q4Mod, test = retailTest, train = retailTrain )
