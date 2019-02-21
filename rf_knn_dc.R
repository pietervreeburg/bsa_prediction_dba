set.seed(123)

# --------------------
# Eindopdracht DBA, modeling
# --------------------

# libraries
library(ggplot2) # plots
library(gridExtra) # arrange plots
library(wesanderson) # plot colors

library(dplyr) # data wrangling
library(tidyr) # tidy data
library(magrittr) # pipes
library(forcats) # factor helpers

library(randomForest) # random forst
library(caret) # ml helpers
library(class) # knn
library(rpart) # decision tree
library(rpart.plot) # decision tree helpers

library(ROCR)

# set wd to source folder
active_dir <- rstudioapi::getActiveDocumentContext()$path %>% dirname()
setwd(file.path(active_dir, 'source_files'))

# load data
org_ese <- readRDS('ese_data.rds')
org_ese_blok1 <- readRDS('ese_data_blok1.rds')
org_join_ese <- readRDS('ese_data_joined_na_omit.rds')

# custom plot functions
plot_vip <- function(rf_in, plot_subtit) {
  imp <- varImpPlot(rf_in)
  var <- as_factor(row.names(imp))
  val <- imp[,1]
  plot_data <- data_frame(var, val)
  ggplot(plot_data, aes(x = fct_reorder(var, val), y = val)) + geom_point(size = 2) + coord_flip() + theme_classic() + 
    theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'grey')) + 
    labs(title = 'Variable Importance Plot', subtitle = plot_subtit, y = 'Gemiddelde afname in accuracy', x = 'Variabele')
}


plot_rf <- function(rf_in, plot_subtit) {
  plot_data <- cbind(num_tree = 1:500, as_data_frame(rf$err.rate)) %>% gather(Type, Value, -1)
  ggplot(plot_data, aes(x = num_tree, y = Value, color = Type)) + geom_line(size = 1) + theme_classic() +
    labs(title = 'Random forest', subtitle = plot_subtit, x = 'Aantal bomen', y = 'Fout')
}

# # --------------------
# # Descriptives (ESE data, whole dataset)
# # --------------------
ese <- org_ese

theme_set(theme_classic())
plot_color <- wes_palette('FantasticFox1')
plot_color_2 <- c(plot_color[3], plot_color[5])

# total students per year
pl_tot_stud <- ggplot(ese, aes(x = cohort)) + geom_bar() + labs(title = 'Totaal aantal studenten', x = 'Cohort', y = 'Aantal studenten')

# educ per year
pl_educ <- ggplot(ese, aes(x = cohort, fill = opleiding)) + geom_bar(position = 'dodge') + scale_fill_manual(values = plot_color_2) +
  labs(title = 'Opleiding per cohort', x = 'Cohort', y = 'Aantal studenten')

# sex per year
pl_sex <- ggplot(ese, aes(x = cohort, fill = geslacht)) + geom_bar(position = 'fill') + scale_fill_manual(values = plot_color_2) +
  labs(title = 'Geslacht per cohort', x = 'Cohort', y = '% geslacht')

# ethnicity per year
ese$herkomst <- ese$etniciteit %>% recode('Autochtoon' = 'Westers',
                                          'W-allochtoon' = 'Westers',
                                          'NW-allochtoon' = 'Niet-westers') %>% as_factor()
pl_ethnic <- ggplot(ese, aes(x = cohort, fill = herkomst)) + geom_bar(position = 'fill') + scale_fill_manual(values = plot_color_2) +
  labs(title = 'Herkomst per cohort', x = 'Cohort', y = '% herkomst')

# age per year
pl_age <- ggplot(ese, aes(x = leeftijd)) + geom_histogram(binwidth = 1) + theme_bw() + facet_wrap(~cohort) +
  labs(title = 'Leeftijd per cohort', x = 'Leeftijd', y = 'Leeftijd')

# prior grade per year
pl_grade <- ggplot(ese, aes(x = as.factor(cohort), y = gemiddelde_laatste)) + geom_boxplot() +
  labs(title = 'Gemiddeld cijfer vooropleiding per cohort', x = 'Cohort', y = 'Gemiddeld cijfer vooropleiding')

# bsa per year
ese$bsa %<>% fct_infreq()
pl_bsa <- ggplot(ese, aes(x = cohort, fill = bsa)) + geom_bar(position = 'fill') + scale_fill_manual(values = plot_color_2) +
  labs(title = 'BSA\'s per cohort', x = 'Cohort', y = '% BSA')

grid.arrange(pl_tot_stud, pl_educ, pl_sex, pl_ethnic, ncol = 2)
# pl_age
grid.arrange(pl_grade, pl_bsa)

# bsa relations
pl_bsa_age <- ggplot(ese) + aes(x = bsa, y = leeftijd) + geom_boxplot() +
  labs(title = 'Leeftijd naar BSA', x = 'BSA', y = 'Leeftijd')

pl_bsa_grade <- ggplot(ese) + aes(x = bsa, y = gemiddelde_laatste) + geom_boxplot() +
  labs(title = 'Gem. vooropl. naar BSA', x = 'BSA', y = 'Gemiddeld cijfer vooropleiding')

pl_nt <- ggplot(ese) + aes(x = opleiding, fill = natuur_tech) + geom_bar(position = 'fill') +
  scale_fill_manual(values = plot_color_2,  labels = c('Nee', 'Ja'), name = 'N&T-profiel') +
  labs(title = 'N&T-profiel naar opleiding', x = 'Opleiding', y = '% N&T-profiel')

pl_bsa_nt_educ <- ggplot(ese) + aes(x = natuur_tech, fill = bsa) + geom_bar(position = 'fill') +
  scale_fill_manual(values = plot_color_2) + # scale because of the mapping scale (legend) (in addition to x and y scale)
  scale_x_discrete(labels = c('Nee', 'Ja')) +
  labs(title = 'BSA\'s naar N&T-profiel per opl.', x = 'N&T-profiel', y = '% BSA') +
  facet_wrap(~ opleiding) +
  theme(strip.background = element_blank())

grid.arrange(pl_bsa_age, pl_bsa_grade, pl_nt, pl_bsa_nt_educ, ncol = 2)

# correlation nt_profile & educ=
nt_educ_tab <- table(ese$natuur_tech, ese$opleiding)
chisq_nt_educ_tab <- chisq.test(nt_educ_tab)
sqrt(chisq_nt_educ_tab$statistic / sum(nt_educ_tab))
prop.table(nt_educ_tab, margin = 2)

# --------------------
# Datamining for feature selection (ESE data, whole dataset)
# --------------------
ese <- org_ese

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -opl_natuur_tech)
str(ese)

# train randomforest (default parameters)
rf <- randomForest(bsa ~ ., data = ese, ntree = 500, importance = TRUE)
plot_rf(rf, 'ESE data')
plot_vip(rf, 'ESE data')

# --------------------
# Datamining for feature selection (ESE data, Ectrie)
# --------------------
ese <- org_ese

# filter educ
ese %<>% filter(opleiding == 'Ectrie')

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -opl_natuur_tech,
                -opleiding)
str(ese)

# train randomforest (default parameters)
rf <- randomForest(bsa ~ ., data = ese, ntree = 500, importance = TRUE)
plot_rf(rf, 'ESE data, Econometrie')
plot_vip(rf, 'ESE data, Econometrie')

# --------------------
# Datamining for feature selection (ESE data, Econ)
# --------------------
ese <- org_ese

# filter educ
ese %<>% filter(opleiding == 'Econ')

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -opl_natuur_tech,
                -opleiding)
str(ese)

# train randomforest (default parameters)
rf <- randomForest(bsa ~ ., data = ese, ntree = 500, importance = TRUE)
plot_rf(rf, 'ESE data, Economie')
plot_vip(rf, 'ESE data, Economie')

# --------------------
# Predictive modelling (ESE data)
# --------------------
ese <- org_ese

# create train / test sample
set.seed(123)
ese_train_sample <- createDataPartition(ese$bsa, 1, 0.7) %>% unlist()
naive_acc_test <- max(prop.table(table(ese$bsa[-ese_train_sample]))) # predict majory class

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -vooropleiding,
                -mr_drs,
                -cohort,
                -opleiding,
                -natuur_tech)
str(ese)

# --------------------
# Random forest
# --------------------

# get samples
ese_rf_train <- ese[ese_train_sample, ]
ese_rf_test <- ese[-ese_train_sample, ]

# tune randomforest
# plot one random forest with default values to determine an appropriate number of trees
# default mtry: sqrt(p), default ntree: 500
plot(randomForest(bsa ~ ., data = ese_rf_train)) # 500 does not add much, but as processing times are low the default is ok
num_tree_class <- 500

# tune mtry value
# grid search + / - 2 around default mtry value
default_mtry <- round(sqrt(length(ese_rf_train)), 0) # default: sqrt(p) for classification forests
range_mod <- 2
range_mtry <- (default_mtry - range_mod):(default_mtry + range_mod) %>% (function(x) {x[x>0]})
rand_for_list <- list()
for (i in 1:length(range_mtry)) {
  rand_for_list[[i]] <- randomForest(bsa ~ ., data = ese_rf_train, 
                                     ntree = num_tree_class, mtry = range_mtry[i], do.trace = FALSE)
}

# collect oob error for all mtry values
oob_error <- sapply(rand_for_list, function(x) {x$err.rate[ , 'OOB']}) %>% data.frame()
colnames(oob_error) <- sapply(rand_for_list, function(x) {x$mtry})
oob_error <- cbind(trees = 1:num_tree_class, oob_error)
best_mtry_class <- range_mtry[which.min(oob_error[num_tree_class, -1])] # mtry value with lowest mse for final iteration

# plot oob mse
oob_error <- gather(oob_error, mtry, error, -trees)
oob_error$mtry <- factor(oob_error$mtry) %>% fct_inorder()
plot_oob_class <- ggplot(oob_error, aes(trees, error, color = mtry)) + geom_line() + theme_bw()

plot_oob_class

# final forest wsith best_mtry
rf_ese <- randomForest(bsa ~ ., data = ese_rf_train, mtry = best_mtry_class, 
                   ntree = num_tree_class, importance = TRUE, do.trace = FALSE)
plot_rf(rf_ese, 'ESE data')
plot_vip(rf_ese, 'ESE data')

# predict on train & testdata
rf_ese_train <- rf_ese$predicted
rf_ese_test <- predict(rf_ese, newdata = select(ese_rf_test, -bsa))
rf_ese_test_prob <- predict(rf_ese, newdata = select(ese_rf_test, -bsa), type = 'prob')

# performance
perf_rf_ese_train <- confusionMatrix(data = rf_ese_train, reference = ese_rf_train$bsa, positive = 'Positief', mode = "everything")
perf_rf_ese_test <- confusionMatrix(data = rf_ese_test, reference = ese_rf_test$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_rf_ese_test$table)
perf_rf_ese_train$overall['Accuracy']
perf_rf_ese_test$overall['Accuracy']
perf_rf_ese_test$overall['Kappa']
perf_rf_ese_test$byClass['Sensitivity']
perf_rf_ese_test$byClass['Specificity']

# check voting thresholds
votes_rf_ese <- data.frame(predict(rf_ese, newdata = select(ese_rf_test, -bsa), type = 'vote', norm.votes = FALSE),
                           'class_pred' = rf_ese_test) # predict(rf_ese, newdata = select(ese_rf_test, -bsa), type = 'response'))

# roc data
roc_rf_ese <- prediction(rf_ese_test_prob[, 2], ese_rf_test$bsa) %>% performance('tpr', 'fpr')
roc_data <- cbind('x' = unlist(roc_rf_ese@x.values), 'y' = unlist(roc_rf_ese@y.values)) %>% as_data_frame()
ggplot(data = roc_data) + aes(x = as.numeric(x), y = as.numeric(y)) + geom_line() +
  labs(title = 'ROC plot for random forest', x = 'fpr', y = 'tpr')

# --------------------
# Epilogue: rf_ese with cutoff point
# --------------------
# cutoff tuner
pos_prop <- seq(1:99)/100
tune_results_acc <- double()
tune_results_sens <- double()
tune_results_spec <- double()
tune_results_kappa <- double()
for (i in pos_prop) {
  cat('positive prop:', i, '\n', sep = ' ')
  cut <- c(1-i, i)
  rf_ese_cutoff_test <- predict(rf_ese, newdata = select(ese_rf_test, -bsa), cutoff = cut)
  perf_rf_ese_cutoff_test <- confusionMatrix(data = rf_ese_cutoff_test, reference = ese_rf_test$bsa, positive = 'Positief', mode = "everything")
  acc <- round(perf_rf_ese_cutoff_test$overall['Accuracy'], 4)
  sens <- round(perf_rf_ese_cutoff_test$byClass['Sensitivity'], 4)
  spec <- round(perf_rf_ese_cutoff_test$byClass['Specificity'], 4)
  kappa <- round(perf_rf_ese_cutoff_test$overall['Kappa'], 4)
  tune_results_acc <- c(tune_results_acc, acc)
  tune_results_sens <- c(tune_results_sens, sens)
  tune_results_spec <- c(tune_results_spec, spec)
  tune_results_kappa <- c(tune_results_kappa, kappa)
}
tune_results <- data_frame('neg_prop' = 1 - pos_prop, 
                           'pos_prop' = pos_prop, 
                           'kappa' = tune_results_kappa,
                           'acc' = tune_results_acc,
                           'sens' = tune_results_sens, 
                           'spec' = tune_results_spec)
best_kappa <- tune_results[which(tune_results_kappa >= max(tune_results_kappa)), ]
best_kappa
best_kappa <- best_kappa[1, ] # get first best kappa. It probably better to do this manually

ggplot(data = tune_results) + aes(x = pos_prop, y = kappa) + geom_line() + 
  geom_vline(xintercept = best_kappa$pos_prop, color = plot_color[5]) +
  labs(title = 'Tuning resultaten voor Random Forest, Kappa', x = 'Perc. bomen voor positief', y = 'Kappa')

# predict with best cutoff
cut <- c(best_kappa$neg_prop, best_kappa$pos_prop)
rf_ese_cutoff_test <- predict(rf_ese, newdata = select(ese_rf_test, -bsa), cutoff = cut)
rf_ese_cutoff_test_prob <- predict(rf_ese, newdata = select(ese_rf_test, -bsa), cutoff = cut, type = 'prob')
rf_ese_cutoff_test_vote <- predict(rf_ese, newdata = select(ese_rf_test, -bsa), cutoff = cut, type = 'vote', norm.votes = FALSE)

# check voting thresholds
votes_rf_ese_cutoff <- data.frame(rf_ese_cutoff_test_vote, 'class_pred' = rf_ese_cutoff_test)

# roc for rf_ese & rf_ese_cutoff
# ER ZIT GEEN VERSCHIL TUSSEN BEIDE ROC CURVES OMDAT DE GERAPPORTEERDE PROBS NIET WIJZIGEN. ALLEEN DE VOTING THRESHOLD WORDT AANGEPAST
# MAAR LATEN ZITTEN
# roc_rf_ese_cutoff <- prediction(rf_ese_cutoff_test_prob[, 2], ese_rf_test$bsa) %>% performance('tpr', 'fpr')

# --------------------
# KNN
# --------------------

# feature preprocessing
ese_knn <- cbind(ese, 'educ_nt_profile_dum' = class2ind(ese$opl_natuur_tech),
                 'etniciteit_dum' = class2ind(ese$etniciteit),
                 'geslacht_dum' = class2ind(ese$geslacht))
ese_knn %<>% select(-opl_natuur_tech, -etniciteit, -geslacht)
ese_knn <- data.frame('bsa' = ese_knn$bsa, ese_knn %>% select(-bsa) %>% scale())

# get samples
ese_knn_train <- ese_knn[ese_train_sample, ]
ese_knn_test <- ese_knn[-ese_train_sample, ]

# create folds
folds <- createFolds(ese_knn_train$bsa, k = 10)

# tune K hyperparam
k_max <- 20
tune_results <- rep(0, k_max)
label_col <- which(names(ese_knn_train) == 'bsa')

for (K in 1:k_max) {
  print(paste('K:', K))
  cv_results <- lapply(folds, function(x) {
    knn_cv <- knn(ese_knn_train[-x , -label_col], ese_knn_train[x , -label_col], ese_knn_train[-x, label_col], K)
    conf_tab <- table(ese_knn_train[x , label_col], knn_cv)
    err <- 1 - (sum(diag(conf_tab)) / sum(conf_tab))
    return(err)
  })
  tune_results[K] <- mean(unlist(cv_results))
}

# scree plot & store optimal K
plot(tune_results, type="b")
optim_k <- which.min(tune_results) # manually set to 8 (elbow)

# predict on train & test data
knn_ese_train <- knn(ese_knn_train[ , -label_col], ese_knn_train[ , -label_col], ese_knn_train[ , label_col], 8)
knn_ese_test <- knn(ese_knn_train[ , -label_col], ese_knn_test[ , -label_col], ese_knn_train[ , label_col], 8)

# performance
perf_knn_ese_train <- confusionMatrix(data = knn_ese_train, reference = ese_knn_train$bsa, positive = 'Positief', mode = "everything")
perf_knn_ese_test <- confusionMatrix(data = knn_ese_test, reference = ese_knn_test$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_knn_ese_test$table)
perf_knn_ese_train$overall['Accuracy']
perf_knn_ese_test$overall['Accuracy']
perf_knn_ese_test$overall['Kappa']
perf_knn_ese_test$byClass['Sensitivity']
perf_knn_ese_test$byClass['Specificity']

# --------------------
# Decision tree
# --------------------

# get samples
ese_dc_train <- ese[ese_train_sample, ]
ese_dc_test <- ese[-ese_train_sample, ]

# grow decision tree
tree_param <- rpart.control(cp = 0.0004)
dc_ese <- rpart(bsa ~ gemiddelde_laatste + opl_natuur_tech + leeftijd,
                data = ese_dc_train, control = tree_param)
plotcp(dc_ese)

# get optimal cp with 1 std error rule
ind_min <- which.min(dc_ese$cptable[, 'xerror'])
err_thres <- dc_ese$cptable[ind_min, "xerror"] + dc_ese$cptable[ind_min, "xstd"]
ind_1sd <- which(dc_ese$cptable[, "xerror"] <= err_thres)[1]

# prune tree
dc_ese_pruned <- prune(dc_ese, cp = dc_ese$cptable[ind_1sd, "CP"])
rpart.plot(dc_ese_pruned, type = 4)

# predict on train & test data
dc_ese_train <- predict(dc_ese_pruned, select(ese_dc_train, -bsa), type = 'class')
dc_ese_test <- predict(dc_ese_pruned, select(ese_dc_test, -bsa), type = 'class')

# performance
perf_dc_ese_train <- confusionMatrix(data = dc_ese_train, reference = ese_dc_train$bsa, positive = 'Positief', mode = "everything")
perf_dc_ese_test <- confusionMatrix(data = dc_ese_test, reference = ese_dc_test$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_dc_ese_test$table)
perf_dc_ese_test$overall['Accuracy']
perf_dc_ese_train$overall['Accuracy']
perf_dc_ese_test$overall['Kappa']
perf_dc_ese_test$byClass['Sensitivity']
perf_dc_ese_test$byClass['Specificity']

# --------------------
# Predictive modelling (ESE data joined with school data)
# --------------------
join_ese <- org_join_ese

# create train / test sample
set.seed(123)
join_ese_train_sample <- createDataPartition(join_ese$bsa, 1, 0.7) %>% unlist()

# --------------------
# Random forest
# --------------------

# drop unneeded variables
join_ese %<>% select(-brin_laatste,
                      -jaar_laatste,
                      -vooropleiding,
                      -mr_drs,
                      -cohort,
                      -kand_numb,
                      -kand_pass,
                      -fte.tot,
                      -fte.ft,
                      -fte.pt,
                      -opleiding,
                      -natuur_tech)
str(join_ese)

# get samples
join_ese_rf_train <- join_ese[join_ese_train_sample, ]
join_ese_rf_test <- join_ese[-join_ese_train_sample, ]

# tune randomforest
# plot one random forest with default values to determine an appropriate number of trees
# default mtry: sqrt(p), default ntree: 500
plot(randomForest(bsa ~ ., data = join_ese_rf_train)) # 500 does not add much, but as processing times are low the default is ok
num_tree_class <- 500

# tune mtry value
# grid search + / - 2 around default mtry value
default_mtry <- round(sqrt(length(join_ese_rf_train)), 0) # default: sqrt(p) for classification forests
range_mod <- 2
range_mtry <- (default_mtry - range_mod):(default_mtry + range_mod) %>% (function(x) {x[x>0]})
rand_for_list <- list()
for (i in 1:length(range_mtry)) {
  rand_for_list[[i]] <- randomForest(bsa ~ ., data = join_ese_rf_train, 
                                     ntree = num_tree_class, mtry = range_mtry[i], do.trace = FALSE)
}


plot_oob_class

# final forest wsith best_mtry
rf_join_ese <- randomForest(bsa ~ ., data = join_ese_rf_train, mtry = best_mtry_class, 
                   ntree = num_tree_class, importance = TRUE, do.trace = FALSE)
plot_rf(rf_join_ese, 'ESE & school data')
plot_vip(rf_join_ese, 'ESE & school data') 

# predict on train & testdata
rf_join_ese_train <- rf_join_ese$predicted
rf_join_ese_test <- predict(rf_join_ese, newdata = select(join_ese_rf_test, -bsa))

# performance
perf_rf_join_ese_train <- confusionMatrix(data = rf_join_ese_train, reference = join_ese_rf_train$bsa, 
                                          positive = 'Positief', mode = "everything")
perf_rf_join_ese_test <- confusionMatrix(data = rf_join_ese_test, reference = join_ese_rf_test$bsa, 
                                         positive = 'Positief', mode = "everything")

prop.table(perf_rf_join_ese_test$table)
perf_rf_join_ese_test$overall['Accuracy']
perf_rf_join_ese_train$overall['Accuracy']
perf_rf_join_ese_test$overall['Kappa']
perf_rf_join_ese_test$byClass['Sensitivity']
perf_rf_join_ese_test$byClass['Specificity']

# --------------------
# Predictive modelling (ESE data with blok 1)
# --------------------
ese <- org_ese_blok1

# create train / test sample
set.seed(123)
ese_train_sample <- createDataPartition(ese$bsa, 1, 0.7) %>% unlist()
naive_acc_test_blok1 <- max(prop.table(table(ese$bsa[-ese_train_sample]))) # predict majory class

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -vooropleiding,
                -mr_drs,
                -cohort,
                -opleiding,
                -natuur_tech)
str(ese)
summary(ese)

# --------------------
# Random forest
# --------------------

# get samples
ese_rf_train <- ese[ese_train_sample, ]
ese_rf_test <- ese[-ese_train_sample, ]

# tune randomforest
# plot one random forest with default values to determine an appropriate number of trees
# default mtry: sqrt(p), default ntree: 500
plot(randomForest(bsa ~ ., data = ese_rf_train)) # 500 does not add much, but as processing times are low the default is ok
num_tree_class <- 500

# tune mtry value
# grid search + / - 2 around default mtry value
default_mtry <- round(sqrt(length(ese_rf_train)), 0) # default: sqrt(p) for classification forests
range_mod <- 2
range_mtry <- (default_mtry - range_mod):(default_mtry + range_mod) %>% (function(x) {x[x>0]})
rand_for_list <- list()
for (i in 1:length(range_mtry)) {
  rand_for_list[[i]] <- randomForest(bsa ~ ., data = ese_rf_train, 
                                     ntree = num_tree_class, mtry = range_mtry[i], do.trace = FALSE)
}

# collect oob error for all mtry values
oob_error <- sapply(rand_for_list, function(x) {x$err.rate[ , 'OOB']}) %>% data.frame()
colnames(oob_error) <- sapply(rand_for_list, function(x) {x$mtry})
oob_error <- cbind(trees = 1:num_tree_class, oob_error)
best_mtry_class <- range_mtry[which.min(oob_error[num_tree_class, -1])] # mtry value with lowest mse for final iteration

# plot oob mse
oob_error <- gather(oob_error, mtry, error, -trees)
oob_error$mtry <- factor(oob_error$mtry) %>% fct_inorder()
plot_oob_class <- ggplot(oob_error, aes(trees, error, color = mtry)) + geom_line() + theme_bw()

plot_oob_class

# final forest wsith best_mtry
rf_ese <- randomForest(bsa ~ ., data = ese_rf_train, mtry = best_mtry_class, 
                       ntree = num_tree_class, importance = TRUE, do.trace = FALSE)
plot_rf(rf_ese, 'ESE met blok 1 data')
plot_vip(rf_ese, 'ESE met blok 1 data')

# predict on train & testdata
rf_ese_train <- rf_ese$predicted
rf_ese_test <- predict(rf_ese, newdata = select(ese_rf_test, -bsa))

# performance
perf_rf_ese_train <- confusionMatrix(data = rf_ese_train, reference = ese_rf_train$bsa, positive = 'Positief', mode = "everything")
perf_rf_ese_test <- confusionMatrix(data = rf_ese_test, reference = ese_rf_test$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_rf_ese_test$table)
perf_rf_ese_test$overall['Accuracy']
perf_rf_ese_train$overall['Accuracy']
perf_rf_ese_test$overall['Kappa']
perf_rf_ese_test$byClass['Sensitivity']
perf_rf_ese_test$byClass['Specificity']

# --------------------
# Decision tree
# --------------------

# get samples
ese_dc_train <- ese[ese_train_sample, ]
ese_dc_test <- ese[-ese_train_sample, ]

# grow decision tree
tree_param <- rpart.control(cp = 0.0004)
dc_ese <- rpart(bsa ~ gehaald_blok1 + gemiddelde_blok1 + opl_natuur_tech + gemiddelde_laatste,
                data = ese_dc_train, control = tree_param)
plotcp(dc_ese)

# get optimal cp with 1 std error rule
ind_min <- which.min(dc_ese$cptable[, 'xerror'])
err_thres <- dc_ese$cptable[ind_min, "xerror"] + dc_ese$cptable[ind_min, "xstd"]
ind_1sd <- which(dc_ese$cptable[, "xerror"] <= err_thres)[1]

# prune tree
dc_ese_pruned <- prune(dc_ese, cp = dc_ese$cptable[ind_1sd, "CP"])
rpart.plot(dc_ese_pruned, type = 4)

# predict on test data
dc_ese_test <- predict(dc_ese_pruned, select(ese_dc_test, -bsa), type = 'class')
dc_ese_train <- predict(dc_ese_pruned, select(ese_dc_train, -bsa), type = 'class')

# performance
perf_dc_ese_test <- confusionMatrix(data = dc_ese_test, reference = ese_dc_test$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_dc_ese_test$table)
perf_dc_ese_test$overall['Accuracy']
perf_dc_ese_train$overall['Accuracy']
perf_dc_ese_test$overall['Kappa']
perf_dc_ese_test$byClass['Sensitivity']
perf_dc_ese_test$byClass['Specificity']

# --------------------
# Experimental from here, finish for blog post
# --------------------

# # assess tree stability
# library(stablelearner)
# stab_tree <- stabletree(dc_ese_pruned, B = 500)
# barplot(stab_tree)
# summary(stab_tree)
# plot(stab_tree)
# 
# # experiment: build stable tree
# library(dtree)
# stab_tree <- dtree(bsa ~ pass_rate_blok1 + gpa_blok1 + educ + VOOROPL_GEM_CIJFER_LAATSTE, data = ese_dc_train,
#                    methods = 'rpart', tuneLength = 5)
# summary(stab_tree)
# rpart.plot(stab_tree$rpart.out, type = 4)

# --------------------
# Check Lasso (ESE data)
# --------------------
library(glmnet)

ese <- org_ese

# create train / test sample
set.seed(123)
ese_train_sample <- createDataPartition(ese$bsa, 1, 0.7) %>% unlist()

# drop unneeded variables
ese %<>% select(-brin_laatste,
                -jaar_laatste,
                -vooropleiding,
                -mr_drs,
                -cohort,
                -opleiding,
                -natuur_tech)
str(ese)

# get samples
ese_lasso_train <- ese[ese_train_sample, ]
ese_lasso_test <- ese[-ese_train_sample, ]

# train model
y_train <- ese_lasso_train$bsa
x_train <- model.matrix(~ . + 0, select(ese_lasso_train, -bsa))[,-1] %>% scale()
# alternative with caret for full encoding
x_train_model <- dummyVars(~ . , select(ese_lasso_train, -bsa))
x_train_alt <- predict(x_train_model, select(ese_lasso_train, -bsa)) %>% scale()

cv_glm_ese <- cv.glmnet(x_train, y_train, alpha = 1, intercept = FALSE, family = 'binomial', nfolds = 10)   
plot(cv_glm_ese)
lambda_min <- cv_glm_ese$lambda.min

glm_ese <- glmnet(x_train, y_train, alpha = 1, intercept = FALSE, family = 'binomial', lambda = lambda_min)   
glm_ese$beta

# predict
x_test <- model.matrix(~ . + 0, select(ese_lasso_test, -bsa))[,-1] %>% scale()
x_test_model <- dummyVars(~ . , select(ese_lasso_test, -bsa))
x_test_alt <- predict(x_test_model, select(ese_lasso_test, -bsa)) %>% scale()

glm_ese_predict_test <- predict(glm_ese, x_test, type = 'class')
glm_ese_predict_train <- predict(glm_ese, x_train, type = 'class')

# performance
perf_glm_ese_test <- confusionMatrix(data = glm_ese_predict_test, reference = ese_lasso_test$bsa, positive = 'Positief', mode = "everything")
perf_glm_ese_train <- confusionMatrix(data = glm_ese_predict_train, reference = ese_lasso_train$bsa, positive = 'Positief', mode = "everything")

prop.table(perf_glm_ese_test$table)
perf_glm_ese_test$overall['Accuracy']
perf_glm_ese_train$overall['Accuracy']
perf_glm_ese_test$overall['Kappa']
perf_glm_ese_test$byClass['Sensitivity']
perf_glm_ese_test$byClass['Specificity']