###############################
## Lasso ###
###############################

#software eisen
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(magrittr)
library(readr)
library(randomForest)

library(purrr)

library(rpart)
library(rpart.plot)

library(caret) 
library(class)
library(e1071)
library(purrr)
library(stringr)
library(glmnet)
library(forcats)

# set wd to source folder
active_dir <- rstudioapi::getActiveDocumentContext()$path %>% dirname()
setwd(file.path(active_dir, 'source_files'))

#---------------------------
# joined dataset laden 
#---------------------------

ese_school_na_omit <- readRDS('ese_data_joined_na_omit.rds')
#ese_school_na_omit <- filter(ese_school_na_omit, educ == 'Econ')

dataset <- ese_school_na_omit %<>% select(-brin_laatste,
                                          -cohort,
                                          -opleiding,
                                          -jaar_laatste,
                                          -vooropleiding,
                                          -mr_drs,
                                          -kand_numb,
                                          -kand_pass,
                                          -fte.tot,
                                          -fte.ft,
                                          -fte.pt,
                                          -natuur_tech)
str(dataset)

dataset$gemiddelde_laatste <- dataset$gemiddelde_laatste/10

# Split DS in training/test stratified 50%
# create train / test sample
set.seed(123)
ese_data_train_sample <- createDataPartition(dataset$bsa, 1, 0.7)

ese_train <- dataset[ese_data_train_sample$Resample1, ]
ese_test <- dataset[-ese_data_train_sample$Resample1, ]


X.test <- model.matrix(~ . + 0, data = ese_test[, -1])
#X.test <- X.test[,-1] #remove NT_dummy
X.test <- scale(X.test)

y.test <- as.vector(ese_test$bsa)

y <- as.vector(ese_train$bsa)                    # y variable
X <- model.matrix(~ . + 0, data = ese_train[, -1])  # Predictor variables (as a matrix, not dataframe)


#X <- X[, -1]                                    # Remove dummy for NT-profile
X <- scale(X)                                     # Make columns z-scores

X.verrijkt <- X

# 10-fold cross validation for lassso regression 
result.cv <- cv.glmnet(X, y, alpha = 1, 
                       lambda = 10^seq(-2, 5, length.out = 50),
                       standardize = TRUE, standardize.response = TRUE,
                       intercept = FALSE, nfolds = 10, family = "binomial")

print(result.cv$lambda.min)      # Best cross validated lambda
print(result.cv$lambda.1se)      # Conservative est. of best lambda (1 stdev)
plot(result.cv) 

print(result.cv$lambda.min)      # Best cross validated lambda

# Final run with best cross validated lambda
result <- glmnet(X, y, alpha = 1, lambda = result.cv$lambda.min,
                 standardize = TRUE, standardize.response = TRUE,
                 intercept = FALSE, family = "binomial")   




result$beta #-result_ese$beta #coefficients
result_verrijkt <- result$beta
  
#Predicting met GLM net
resultaat <- as.factor(predict(result, newx = X.test, type = 'class'))

#confusion matrix
confusionMatrix(data = resultaat, reference = as.factor(y.test), positive = 'Positief', mode = "everything")

###############################
## GLM net op ESE only data ###
###############################
ese_data <- readRDS('ese_data.rds')
#ese_data <- filter(ese_data, educ == 'Econ')

#kruistterm gebruiken
dataset <- ese_data %<>% select(-brin_laatste,
                                          -cohort,
                                          -opleiding,
                                          -jaar_laatste,
                                          -natuur_tech,
                                          -vooropleiding,
                                          -mr_drs)
str(dataset)

# Split DS in training/test stratified 50%
# create train / test sample
set.seed(123)
ese_data_train_sample <- createDataPartition(dataset$bsa, 1, 0.7)

ese_train <- dataset[ese_data_train_sample$Resample1, ]
ese_test <- dataset[-ese_data_train_sample$Resample1, ]


X.test <- model.matrix(~ . + 0, data = ese_test[, -1])
X.test <- scale(X.test)

y.test <- as.vector(ese_test$bsa)

y <- as.vector(ese_train$bsa)                    # y variable
X <- model.matrix(~ . + 0, data = ese_train[, -1])  # Predictor variables (as a matrix, not dataframe)


X <- scale(X)                                     # Make columns z-scores
X.ese <- X

# 10-fold cross validation for lassso regression 
library(glmnet)
result.cv <- cv.glmnet(X, y, alpha = 1, 
                       lambda = 10^seq(-2, 5, length.out = 50),
                       standardize = TRUE, standardize.response = TRUE,
                       intercept = FALSE, nfolds = 10, family = "binomial")

print(result.cv$lambda.min)      # Best cross validated lambda
print(result.cv$lambda.1se)      # Conservative est. of best lambda (1 stdev)
plot(result.cv) 

print(result.cv$lambda.min)      # Best cross validated lambda

# Final run with best cross validated lambda
result_ese <- glmnet(X, y, alpha = 1, lambda = result.cv$lambda.min,
                 standardize = TRUE, standardize.response = TRUE,
                 intercept = FALSE, family = "binomial")   


result_ese$beta #coefficients

# result_ese <- result_ese$beta

#Predicting met GLM net
resultaat_ese <- as.factor(predict(result_ese, newx = X.test, type = 'class'))
resultaat_ese_train <- as.factor(predict(result_ese, newx = X, type = 'class'))

#confusion matrix
confusionMatrix(data = resultaat_ese, reference = as.factor(y.test), positive = 'Positief', mode = "everything")
  confusionMatrix(data = resultaat_ese_train, reference = as.factor(y), positive = 'Positief', mode = "everything")

###Combining both coefficient from 
result$beta
result_ese$beta

#-------------------------
### ESE data only weighted
#-------------------------

# Equal weights
n1 <- sum(y == "Negatief")
n2 <- sum(y == "Positief")

#create weight vector for observations
weight.vector <- NaN


for (i in 1:length(y)) {
  
  if (y[i] == 'Negatief') {
    weight.vector[i] <- (n1 + n2)/(2*n1)
    }
  if (y[i] == 'Positief') {
    weight.vector[i] <- (n1 + n2)/(2*n2)
  }
}


weight.vector

#test
table(weight.vector)

# 10-fold cross validation for weighted lassso regression 
result.cv.weighted <- cv.glmnet(X, y, alpha = 1, 
                       lambda = 10^seq(-2, 5, length.out = 50),
                       standardize = TRUE, standardize.response = TRUE,
                       intercept = FALSE, nfolds = 10, family = "binomial", weights = weight.vector)

print(result.cv.weighted$lambda.min)      # Best cross validated lambda
print(result.cv.weighted$lambda.1se)      # Conservative est. of best lambda (1 stdev)
plot(result.cv.weighted) 

print(result.cv.weighted$lambda.min)      # Best cross validated lambda

# Final run with best cross validated lambda
result.weighted <- glmnet(X, y, alpha = 1, lambda = result.cv.weighted$lambda.min,
                 standardize = TRUE, standardize.response = TRUE,
                 intercept = FALSE, family = "binomial", weights = weight.vector)   


result.weighted$beta #coefficients

#Predicting met GLM net
resultaat.weighted <- as.factor(predict(result.weighted, newx = X.test, type = 'class'))

#confusion matrix
confusionMatrix(data = resultaat.weighted, reference = as.factor(y.test), positive = 'Positief', mode = "everything")

# sum(sum(cm$table[1,1]))/sum(cm$table[,1]) #negative sensitivity
# sum(sum(cm$table[1,1]))/sum(cm$table[1,]) #negative precision


#------------------------------------
#Tuner for beste alpha weighted lasso
#------------------------------------

alpha.vector <- seq(0,1, length.out = 30)
index <- seq(alpha.vector)
resultaat.weighted <- NaN
vector.neg_pred <- double()
vector.specific <- double()
vector.kappa <- double()

for (i in index) {

  
  
result.cv.weighted <- cv.glmnet(X, y, alpha = alpha.vector[i], 
                                lambda = 10^seq(-2, 5, length.out = 50),
                                standardize = TRUE, standardize.response = TRUE,
                                intercept = FALSE, nfolds = 10, family = "binomial")

result.weighted <- glmnet(X, y, alpha = alpha.vector[i], lambda = result.cv.weighted$lambda.min,
                          standardize = TRUE, standardize.response = TRUE,
                          intercept = FALSE, family = "binomial")   

resultaat.weighted <- as.factor(predict(result.weighted, newx = X.test, type = 'class'))
cm <- confusionMatrix(data = resultaat.weighted, reference = as.factor(y.test), positive = 'Positief', mode = "everything")

neg_sens <- sum(sum(cm$table[1,1]))/sum(cm$table[,1]) #negative sensitivity = specificity
neg_prec <- sum(sum(cm$table[1,1]))/sum(cm$table[1,]) #negative precision
kappa_cm <- cm$overall[2]

vector.neg_pred <- c(vector.neg_pred, neg_sens)
vector.specific <- c(vector.specific, neg_prec)
vector.kappa <- c(vector.kappa, kappa_cm)

}

#diagnostics
summary(vector.neg_pred)
summary(vector.specific)
summary(vector.kappa)

testdf <- cbind(vector.neg_pred, vector.specific, vector.kappa, alpha.vector)

#best_kappa <- testdf[,3] %>% which.max() #row id (index) voor de beste waarde (in dit geval kappa)

#In een regel maximale waarde verkijgen
testdf[testdf[,3] 
       %>% which.max(),]


#------------
#Bandbreedte voor meerdere steekproeven
#-----------------------------
ese_data <- readRDS('ese_data.rds')
#ese_data <- filter(ese_data, educ == 'Econ')

dataset <- ese_data %<>% select(-brin_laatste,
                                -cohort,
                                -opleiding,
                                -jaar_laatste,
                                -natuur_tech,
                                -vooropleiding,
                                -mr_drs)
str(dataset)

iterations = 250
index <- seq(1:iterations)
result.table <- NaN

vector.neg_pred <- double()
vector.specific <- double()
vector.kappa <- double()


for (i in index) {
  ese_data_train_sample <- createDataPartition(dataset$bsa, 1, 0.7)
  
  ese_train <- dataset[ese_data_train_sample$Resample1, ]
  ese_test <- dataset[-ese_data_train_sample$Resample1, ]
  
  
  X.test <- model.matrix(~ . + 0, data = ese_test[, -1])
  X.test <- X.test[,-1] #remove NT_dummy
  X.test <- scale(X.test)
  
  y.test <- as.vector(ese_test$bsa)
  
  y <- as.vector(ese_train$bsa)                    # y variable
  X <- model.matrix(~ . + 0, data = ese_train[, -1])  # Predictor variables (as a matrix, not dataframe)
  
  
  X <- X[, -1]                                    # Remove dummy for NT-profile
  X <- scale(X)                                     # Make columns z-scores
  
  
  # Equal weights
  n1 <- sum(y == "Negatief")
  n2 <- sum(y == "Positief")
  
  #create weight vector for observation
  weight.vector <- NaN
  
  for (w in 1:length(y)) {
    
    if (y[w] == 'Negatief') {
      weight.vector[w] <- (n1 + n2)/(2*n1)
    }
    if (y[w] == 'Positief') {
      weight.vector[w] <- (n1 + n2)/(2*n2)
    }
  }
  

  result.cv.weighted <- cv.glmnet(X, y, alpha = 0.1034483, 
                                  lambda = 10^seq(-2, 5, length.out = 50),
                                  standardize = TRUE, standardize.response = TRUE,
                                  intercept = FALSE, nfolds = 10, family = "binomial")
  
  result.weighted <- glmnet(X, y, alpha = 0.1034483, lambda = result.cv.weighted$lambda.min,
                            standardize = TRUE, standardize.response = TRUE,
                            intercept = FALSE, family = "binomial")   
  
  resultaat.weighted <- as.factor(predict(result.weighted, newx = X.test, type = 'class'))
  cm <- confusionMatrix(data = resultaat.weighted, reference = as.factor(y.test), positive = 'Positief', mode = "everything")
  
  
  neg_sens <- sum(sum(cm$table[1,1]))/sum(cm$table[,1]) #negative sensitivity = specificity
  neg_prec <- sum(sum(cm$table[1,1]))/sum(cm$table[1,]) #negative precision
  kappa_cm <- cm$overall[2]
  
  vector.neg_pred <- c(vector.neg_pred, neg_sens)
  vector.specific <- c(vector.specific, neg_prec)
  vector.kappa <- c(vector.kappa, kappa_cm)
  
  print(index[i])
  #cat('iteration completed ', index[i], '/n')
  
}

summary(vector.neg_pred)
summary(vector.specific)
quantile(vector.kappa)

df <- as.data.frame(vector.kappa)

ggplot(data = df, aes(x = "", y = vector.kappa)) + geom_boxplot() + labs(title = 'Verdeling 250 steekproeven',x = '', y = 'Kappa value') 


df <- cbind(vector.neg_pred, vector.specific, vector.kappa, index)
#best_kappa <- testdf[,3] %>% which.max() #row id (index) voor de beste waarde (in dit geval kappa)

#In een regel maximale waarde verkijgen
df[df[,3] 
       %>% which.max(),]

############ Plot results Lasso op nette manier conform huisstijl######
library(wesanderson)
library(forcats)
library(forecast)
theme_set(theme_classic())
plot_color <- wes_palette('FantasticFox1')
plot_color_2 <- c(plot_color[5], plot_color[3])

#plotfunction maken
plot_coeflass <- function(beta_in, plot_subtit, ese_no_var, X.train) {
  var <- as_factor(row.names(beta_in))
  val <- beta_in[,1]
  srce_ese <- rep('ESE-data', ese_no_var)
  srce_schl <- rep('Schooldata', (ncol(X.train)-ese_no_var))
  srce <- c(srce_ese, srce_schl)
  plot_data <- data_frame(var, val)
  ggplot(plot_data, aes(x = fct_reorder(var, val), y = val, color = srce)) + 
    coord_flip() + 
    geom_hline(yintercept = 0, color = 'grey') +
    geom_point(size = 2) + 
    theme_classic() + 
    theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'grey')) + 
    labs(title = 'Coëfficiënten Lasso Regressie', subtitle = plot_subtit, y = 'Coëfficiënt', x = 'Variabele') +
    scale_color_manual(values = plot_color_2, name = "Herkomst variabele")
}

pl_lasso.verrijkt <- plot_coeflass(result_verrijkt, 'Verrijkte dataset', 9, X.verrijkt)

pl_lasso.ese <- plot_coeflass(result_ese, 'ESE-dataset', 9, X.ese)



