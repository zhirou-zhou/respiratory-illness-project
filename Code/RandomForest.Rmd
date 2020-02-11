---
title: "Random Forest"
author: "Zhirou Zhou"
date: "12/13/2019"
output: html_document
---

```{r random forest for is_illness}
# Prepare 10-fold cross-validation blocked by Alias
nfold = 10
data_illness = data_illness %>% group_by(Alias)
data_illness$id = c()
data_illness$id = data_illness %>% group_indices(Alias)
size = floor(length(unique(data_illness$id)) / nfold)

set.seed(1211)
fold_id = sample(1:length(unique(data_illness$id)), length(unique(data_illness$id)))
g1_id = fold_id[1:size]
g2_id = fold_id[(size + 1):(size * 2)]
g3_id = fold_id[(size*2 + 1):(size * 3)]
g4_id = fold_id[(size*3 + 1):(size * 4)]
g5_id = fold_id[(size*4 + 1):(size * 5)]
g6_id = fold_id[(size*5 + 1):(size * 6)]
g7_id = fold_id[(size*6 + 1):(size * 7)]
g8_id = fold_id[(size*7 + 1):(size * 8)]
g9_id = fold_id[(size*8 + 1):(size * 9)]
g10_id = fold_id[(size*9 + 1):length(fold_id)]

data_g1 = as.data.frame(data_illness[data_illness$id %in% g1_id, ])
data_g2 = as.data.frame(data_illness[data_illness$id %in% g2_id, ])
data_g3 = as.data.frame(data_illness[data_illness$id %in% g3_id, ])
data_g4 = as.data.frame(data_illness[data_illness$id %in% g4_id, ])
data_g5 = as.data.frame(data_illness[data_illness$id %in% g5_id, ])
data_g6 = as.data.frame(data_illness[data_illness$id %in% g6_id, ])
data_g7 = as.data.frame(data_illness[data_illness$id %in% g7_id, ])
data_g8 = as.data.frame(data_illness[data_illness$id %in% g8_id, ])
data_g9 = as.data.frame(data_illness[data_illness$id %in% g9_id, ])
data_g10 = as.data.frame(data_illness[data_illness$id %in% g10_id, ])
data_fold = list(data_g1, data_g2, data_g3, data_g4, data_g5, data_g6, data_g7,data_g8, data_g9,
                 data_g10)

# Oversample the data within each group to balance `is_illness`
over_sample = function(data) {
  times = floor(table(data$is_illness)[1] / table(data$is_illness)[2])
  data_true = as.data.frame(data[which(data$is_illness == TRUE), ])
  data_false = as.data.frame(data[which(data$is_illness == FALSE), ])
  data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
  data = as.data.frame(rbind(data_true, data_false))
  return(data)
}

data_fold = lapply(data_fold, over_sample)
data_fold_test = data_fold[[1]]
for (i in 2:length(data_fold)) {
  data_fold_test = rbind(data_fold_test, data_fold[[i]])
}

# Generate the grid of `mtry` and `nodesize`
hyper_grid = expand.grid(mtry = seq(10, 60, by = 10), nodesize  = seq(50, 1000, by = 100))

# 10-fold Cross-validation
forest_is = list()
forest_is_predict = list()
for (i in 1:nfold) {
  test = data_fold[[i]]
  test_1 = as.data.frame(subset(test, select = -c(Alias, visit_id, precedes_illness)))
  test = as.data.frame(subset(test, select = -c(Alias, visit_id, is_illness, precedes_illness)))
  n_train = seq(1, nfold, 1)
  n_train = n_train[n_train != i]
  h = n_train[1]
  train = data_fold[[h]]
  for (j in 2:(length(n_train))) {
    k = n_train[j]
    train = as.data.frame(rbind(train, data_fold[[k]]))
  }
  train = as.data.frame(subset(train, select = -c(Alias, visit_id, precedes_illness)))
  for (l in 1:nrow(hyper_grid)) {
  forest_is[[l]] = randomForest(is_illness ~ ., data = train, importance = TRUE, mtry = hyper_grid$mtry[i],
                                nodesize = hyper_grid$nodesize[i], replace = FALSE)
  }
  forest_is_predict[[i]] = lapply(forest_is, predict, test)
}

# Combine the predictions by each random forest
predict_forest = list()
predict_fold = forest_is_predict[[1]]

for (j in 1:nrow(hyper_grid)) {
  prediction = predict_fold[[j]]
  names(prediction) = NULL
  predict_forest[[j]] = as.factor(prediction)
}

for (i in 2:nfold) {
  predict_fold = forest_is_predict[[i]]
  for (j in 1:nrow(hyper_grid)) {
    prediction = predict_fold[[j]]
    names(prediction) = names(predict_forest[[j]])
    predict_forest[[j]] = c(as.character(predict_forest[[j]]), as.character(prediction))
    predict_forest[[j]] = as.factor(predict_forest[[j]])
  }
}

# Tuning parameters by confusion matrices
forest_is_conf = list()
forest_is_conf = lapply(predict_forest, confusionMatrix, 
                              data_fold_test$is_illness, positive = "TRUE")
prune = hyper_grid %>% 
               mutate(test_sensitivity = map_dbl(forest_is_conf, get_sensitivity), 
                      test_specificity = map_dbl(forest_is_conf, get_specificity), 
                      test_precision = map_dbl(forest_is_conf, get_precision), 
                      test_recall = map_dbl(forest_is_conf, get_recall), 
                      test_f1 = map_dbl(forest_is_conf, get_f1))
prune = prune[order(prune$test_precision, decreasing = TRUE), ]
head(prune)
save(forest_is, forest_is_predict, forest_is_conf, prune, file = "forest_is.rdata")

prune_mtry = prune[which(prune$nodesize == 450), ]
prune_nodesize = prune[which(prune$mtry == 30), ]
prune_mtry_err = prune_mtry %>% gather('metric', 'err', test_sensitivity, test_specificity, test_precision,
                                       test_recall, test_f1)
prune_nodesize_err = prune_nodesize %>% gather('metric', 'err', test_sensitivity, test_specificity,
                                               test_precision, test_recall, test_f1)
plot_mtry = ggplot(prune_mtry_err, aes(x = mtry, color = metric, y = err)) + geom_line()
plot_nodesize = ggplot(prune_nodesize_err, aes(x = nodesize, color = metric, y = err)) + geom_line()
plot_mtry
plot_nodesize

# Generate the optimal forest
data_true = as.data.frame(data_illness[which(data_illness$is_illness == TRUE), ])
data_false = as.data.frame(data_illness[which(data_illness$is_illness == FALSE), ])
times = floor(table(data_illness$is_illness)[1] / table(data_illness$is_illness)[2])
data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
data_opti = as.data.frame(rbind(data_true, data_false))
data_train = as.data.frame(subset(data_opti, select = -c(Alias, visit_id, precedes_illness)))
data_test = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness, precedes_illness)))
data_test_1 = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, precedes_illness)))
opti_forest_is = randomForest(is_illness ~ ., data = data_train, importance = TRUE, mtry = 100, 
                              nodesize = 1000, replace = FALSE)

# 10-fold Cross-validation using optimal forest
opti_is = list()
opti_predict_is_class = list()
opti_predict_is_prob = list()
for (i in 1:nfold) {
  test = data_fold[[i]]
  test_1 = as.data.frame(subset(test, select = -c(Alias, visit_id, precedes_illness)))
  test = as.data.frame(subset(test, select = -c(Alias, visit_id, is_illness, precedes_illness)))
  n_train = seq(1, nfold, 1)
  n_train = n_train[n_train != i]
  h = n_train[1]
  train = data_fold[[h]]
  for (j in 2:(length(n_train))) {
    k = n_train[j]
    train = as.data.frame(rbind(train, data_fold[[k]]))
  }
  train = as.data.frame(subset(train, select = -c(Alias, visit_id, precedes_illness)))
  opti_is[[i]] = randomForest(is_illness ~ ., data = train, importance = TRUE, mtry = 100,
                                nodesize = 1000, replace = FALSE)
  opti_predict_is_class[[i]] = predict(opti_is[[i]], test)
  opti_predict_is_prob[[i]] = predict(opti_is[[i]], test, type = "prob")
}
save(opti_is, opti_predict_is_class, opti_predict_is_prob, file = "opti_forest_is.rdata")

# Get class and confidence predictions
opti_predict_is_class_c = opti_predict_is_class[[1]]
names(opti_predict_is_class_c) = NULL
opti_predict_is_class_c = as.factor(opti_predict_is_class_c)
for (i in 2:nfold) {
  predict = opti_predict_is_class[[i]]
  names(predict) = NULL
  opti_predict_is_class_c = c(as.character(opti_predict_is_class_c), as.character(predict))
  opti_predict_is_class_c = as.factor(opti_predict_is_class_c)
}

opti_predict_is_prob_c = as.data.frame(opti_predict_is_prob[[1]])
for (i in 2:nfold) {
  predict = as.data.frame(opti_predict_is_prob[[i]])
  opti_predict_is_prob_c = rbind(opti_predict_is_prob_c, predict)
}

# Check the optimal threshold for predictions
opti_predict_is_class_thre = list()
threshold = seq(0.1, 0.9, 0.05)
for (i in 1:length(threshold)) {
  opti_predict_is_class_thre[[i]] = ifelse(opti_predict_is_prob_c[, 2] > threshold[i], TRUE, FALSE)
  opti_predict_is_class_thre[[i]] = factor(opti_predict_is_class_thre[[i]], levels = c(FALSE, TRUE))
}
confusion_is = list()
confusion_is = lapply(opti_predict_is_class_thre, confusionMatrix, data_fold_test$is_illness, 
                      positive = "TRUE")
threshold = as.data.frame(threshold)
threshold = threshold %>% mutate(sensitivity = map_dbl(confusion_is, get_sensitivity), 
                                 specificity = map_dbl(confusion_is, get_specificity),
                                 ppv = map_dbl(confusion_is, get_ppv),
                                 npv = map_dbl(confusion_is, get_npv), 
                                 f1 = map_dbl(confusion_is, get_f1))
threshold_err = threshold %>% gather('metric', 'err', sensitivity, specificity, ppv, npv, f1)
plot_threshold = ggplot(threshold_err, aes(x = threshold, color = metric, y = err)) + geom_line()
plot_threshold

# Generate the new class prediction using optimal threshold
opti_predict_is_class = list()
for (i in 1:nfold) {
  prob = opti_predict_is_prob[[i]]
  opti_predict_is_class[[i]] = ifelse(prob[, 2] > 0.375, TRUE, FALSE)
  opti_predict_is_class[[i]] = factor(opti_predict_is_class[[i]], levels = c(FALSE, TRUE))
}

# Compare performance of each fold using confusion matrices
opti_conf = list()
for (i in 1:nfold) {
  opti_conf[[i]] = confusionMatrix(opti_predict_is_class[[i]], data_fold[[i]]$is_illness, positive = "TRUE")
}

fold = as.data.frame(seq(1, 10, 1))
colnames(fold) = "fold"
fold = fold %>% mutate(test_sensitivity = map_dbl(opti_conf, get_sensitivity), 
                      test_specificity = map_dbl(opti_conf, get_specificity), 
                      test_ppv = map_dbl(opti_conf, get_ppv), 
                      test_npv = map_dbl(opti_conf, get_npv), 
                      test_f1 = map_dbl(opti_conf, get_f1))

# Generate ROC curve for each fold
pred_is = list()
perf_is_plot = list()
for (i in 1:nfold) {
  predict = as.data.frame(opti_predict_is_prob[[i]])
  pred_is[[i]] = prediction(predict[, 2], data_fold[[i]]$is_illness)
  perf_is_plot[[i]] = performance(pred_is[[i]], "sens", "spec")
}

plot(perf_is_plot[[1]])
plot(perf_is_plot[[2]], add = TRUE, col = 2)
plot(perf_is_plot[[3]], add = TRUE, col = 3)
plot(perf_is_plot[[4]], add = TRUE, col = 4)
plot(perf_is_plot[[5]], add = TRUE, col = 5)
plot(perf_is_plot[[6]], add = TRUE, col = 6)
plot(perf_is_plot[[7]], add = TRUE, col = 7)
plot(perf_is_plot[[8]], add = TRUE, col = 8)
plot(perf_is_plot[[9]], add = TRUE, col = 9)
plot(perf_is_plot[[10]], add = TRUE, col = 10)

# Generate the overall confusion matrix and AUC
pred_is_c = prediction(opti_predict_is_prob_c[, 2], data_fold_test$is_illness)
perf_is_auc = performance(pred_is_c, measure = "auc")
perf_is_auc@y.values[[1]]
confusion_is = confusionMatrix(opti_predict_is_class_c, data_fold_test$is_illness, positive = "TRUE")
confusion_is
```

```{r random forest for precedes_illness}
# Oversample the data within each group to balance `precedes_illness`
over_sample_precedes = function(data) {
  times = floor(table(data$precedes_illness)[1] / table(data$precedes_illness)[2])
  data_true = as.data.frame(data[which(data$precedes_illness == TRUE), ])
  data_false = as.data.frame(data[which(data$precedes_illness == FALSE), ])
  data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
  data = as.data.frame(rbind(data_true, data_false))
  return(data)
}

data_fold = lapply(data_fold, over_sample_precedes)
data_fold_test = data_fold[[1]]
for (i in 2:length(data_fold)) {
  data_fold_test = rbind(data_fold_test, data_fold[[i]])
}

# Generate the grid for `mtry` and `nodesize`
hyper_grid = expand.grid(mtry = c(20, 60, 100), nodesize  = c(50, 500, 1000))

# 10-fold Cross-validation
forest_precedes = list()
forest_precedes_predict = list()
for (i in 1:nfold) {
  test = data_fold[[i]]
  test_1 = as.data.frame(subset(test, select = -c(Alias, visit_id, is_illness)))
  test = as.data.frame(subset(test, select = -c(Alias, visit_id, is_illness, precedes_illness)))
  n_train = seq(1, nfold, 1)
  n_train = n_train[n_train != i]
  h = n_train[1]
  train = data_fold[[h]]
  for (j in 2:(length(n_train))) {
    k = n_train[j]
    train = as.data.frame(rbind(train, data_fold[[k]]))
  }
  train = as.data.frame(subset(train, select = -c(Alias, visit_id, is_illness)))
  for (l in 1:nrow(hyper_grid)) {
  forest_precedes[[l]] = randomForest(is_precedes ~ ., data = train, importance = TRUE, 
                                mtry = hyper_grid$mtry[i],
                                nodesize = hyper_grid$nodesize[i], 
                                replace = FALSE)
  }
  trees_precedes_predict[[i]] = lapply(trees_precedes, predict, test)
}

# Combine the predictions by each forest
predict_forest = list()
predict_fold = forest_is_predict[[1]]

for (j in 1:nrow(hyper_grid)) {
  prediction = predict_fold[[j]]
  names(prediction) = NULL
  predict_forest[[j]] = as.factor(prediction)
}

for (i in 2:nfold) {
  predict_fold = forest_is_predict[[i]]
  for (j in 1:nrow(hyper_grid)) {
    prediction = predict_fold[[j]]
    names(prediction) = names(predict_forest[[j]])
    predict_forest[[j]] = c(as.character(predict_forest[[j]]), as.character(prediction))
    predict_forest[[j]] = as.factor(predict_forest[[j]])
  }
}

# Tuning parameters by confusion matrices
forest_precedes_conf = list()
forest_precedes_conf = lapply(predict_forest, confusionMatrix, 
                              data_fold_test$precedes_illness, positive = "TRUE")
prune_precedes = hyper_grid %>% 
               mutate(test_sensitivity = map_dbl(forest_precedes_conf, get_sensitivity), 
                      test_specificity = map_dbl(forest_precedes_conf, get_specificity), 
                      test_precision = map_dbl(forest_precedes_conf, get_precision), 
                      test_recall = map_dbl(forest_precedes_conf, get_recall), 
                      test_f1 = map_dbl(forest_precedes_conf, get_f1))
prune_precedes = prune_precedes[order(prune_precedes$test_precision, decreasing = TRUE), ]
head(prune_precedes)

save(forest_precedes, forest_precedes_predict, forest_precedes_conf, prune_precedes, 
     file = "forest_precedes.rdata")
#load("forest_precedes.rdata")

prune_precedes_mtry = prune_precedes[which(prune_precedes$mtry == 60), ]
prune_precedes_nodesize = prune_precedes[which(prune_precedes$nodesize == 500), ]
prune_precedes_mtry_err = prune_precedes_mtry %>% gather('metric', 'err', test_sensitivity, test_specificity,
                                                         test_precision, test_recall, test_f1)
prune_precedes_nodesize_err = prune_precedes_nodesize %>% gather('metric', 'err', test_sensitivity,
                                                                 test_specificity, test_precision, 
                                                                 test_recall, test_f1)

plot_mtry = ggplot(prune_precedes_mtry_err, aes(x = mtry, color = metric, y = err)) + geom_line()
plot_nodesize = ggplot(prune_precedes_nodesize_err, aes(x = nodesize, color = metric, y = err)) + geom_line()
plot_mtry
plot_nodesize

# Generate the optimal forest
data_true = as.data.frame(data_illness[which(data_illness$precedes_illness == TRUE), ])
data_false = as.data.frame(data_illness[which(data_illness$precedes_illness == FALSE), ])
times = floor(table(data_illness$precedes_illness)[1] / table(data_illness$precedes_illness)[2])
data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
data_opti = as.data.frame(rbind(data_true, data_false))
data_train = as.data.frame(subset(data_opti, select = -c(Alias, visit_id, is_illness)))
data_test = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness, precedes_illness)))
data_test_1 = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness)))

opti_forest_precedes = randomForest(precedes_illness ~ ., data = data_train, importance = TRUE, 
                              mtry = 20, nodesize = 100, replace = FALSE)

# 10-fold Cross-validation using optimal forest
opti_precedes = list()
opti_predict_precedes_class = list()
opti_predict_precedes_prob = list()
for (i in 1:nfold) {
  test = data_fold[[i]]
  test_1 = as.data.frame(subset(test, select = -c(Alias, visit_id, precedes_illness)))
  test = as.data.frame(subset(test, select = -c(Alias, visit_id, is_illness, precedes_illness)))
  n_train = seq(1, nfold, 1)
  n_train = n_train[n_train != i]
  h = n_train[1]
  train = data_fold[[h]]
  for (j in 2:(length(n_train))) {
    k = n_train[j]
    train = as.data.frame(rbind(train, data_fold[[k]]))
  }
  train = as.data.frame(subset(train, select = -c(Alias, visit_id, precedes_illness)))
  opti_precedes[[i]] = randomForest(precedes_illness ~ ., data = data_train, importance = TRUE, 
                              mtry = 20,
                              nodesize = 100, 
                              replace = FALSE)
  opti_predict_precedes_class[[i]] = predict(opti_precedes[[i]], test)
  opti_predict_precedes_prob[[i]] = predict(opti_precedes[[i]], test, type = "prob")
}

save(opti_precedes, opti_predict_precedes_class, opti_predict_precedes_prob, 
     file = "opti_forest_precedes.rdata")
#load("opti_forest_precedes.rdata")

# Get class and confidence predictions
opti_predict_precedes_class_c = opti_predict_precedes_class[[1]]
names(opti_predict_precedes_class_c) = NULL
opti_predict_precedes_class_c = as.factor(opti_predict_precedes_class_c)
for (i in 2:nfold) {
  predict = opti_predict_precedes_class[[i]]
  names(predict) = NULL
  opti_predict_precedes_class_c = c(as.character(opti_predict_precedes_class_c), as.character(predict))
  opti_predict_precedes_class_c = as.factor(opti_predict_precedes_class_c)
}

opti_predict_precedes_prob_c = as.data.frame(opti_predict_precedes_prob[[1]])
for (i in 2:nfold) {
  predict = as.data.frame(opti_predict_precedes_prob[[i]])
  opti_predict_precedes_prob_c = rbind(opti_predict_precedes_prob_c, predict)
}

# Check the optimal threshold for predictions
opti_predict_precedes_class_thre = list()
threshold = seq(0.1, 0.9, 0.05)
for (i in 1:length(threshold)) {
  opti_predict_precedes_class_thre[[i]] = ifelse(opti_predict_precedes_prob_c[, 2] > threshold[i], 
                                                 TRUE, FALSE)
  opti_predict_precedes_class_thre[[i]] = factor(opti_predict_precedes_class_thre[[i]], 
                                                 levels = c(FALSE, TRUE))
}
confusion_precedes = list()
confusion_precedes = lapply(opti_predict_precedes_class_thre, confusionMatrix, data_fold_test$precedes_illness, 
                      positive = "TRUE")
threshold = as.data.frame(threshold)
threshold = threshold %>% mutate(sensitivity = map_dbl(confusion_precedes, get_sensitivity), 
                                 specificity = map_dbl(confusion_precedes, get_specificity),
                                 ppv = map_dbl(confusion_precedes, get_ppv),
                                 npv = map_dbl(confusion_precedes, get_npv), 
                                 f1 = map_dbl(confusion_precedes, get_f1))
threshold_err = threshold %>% gather('metric', 'err', sensitivity, specificity, ppv, npv, f1)
plot_threshold = ggplot(threshold_err, aes(x = threshold, color = metric, y = err)) + geom_line()
plot_threshold

# Generate the new class prediction using optimal threshold
opti_predict_precedes_class = list()
for (i in 1:nfold) {
  prob = opti_predict_precedes_prob[[i]]
  opti_predict_precedes_class[[i]] = ifelse(prob[, 2] > 0.625, TRUE, FALSE)
  opti_predict_precedes_class[[i]] = factor(opti_predict_precedes_class[[i]], levels = c(FALSE, TRUE))
}

# Compare performance of each fold using confusion matrices
opti_conf = list()
for (i in 1:nfold) {
  opti_conf[[i]] = confusionMatrix(opti_predict_precedes_class[[i]], data_fold[[i]]$precedes_illness, positive = "TRUE")
}

fold = as.data.frame(seq(1, 10, 1))
colnames(fold) = "fold"
fold = fold %>% mutate(test_sensitivity = map_dbl(opti_conf, get_sensitivity), 
                      test_specificity = map_dbl(opti_conf, get_specificity), 
                      test_ppv = map_dbl(opti_conf, get_ppv), 
                      test_npv = map_dbl(opti_conf, get_npv), 
                      test_f1 = map_dbl(opti_conf, get_f1))

# Generate ROC curve for each fold
pred_precedes = list()
perf_precedes_plot = list()
for (i in 1:nfold) {
  predict = as.data.frame(opti_predict_precedes_prob[[i]])
  pred_precedes[[i]] = prediction(predict[, 2], data_fold[[i]]$precedes_illness)
  perf_precedes_plot[[i]] = performance(pred_precedes[[i]], "sens", "spec")
}

plot(perf_precedes_plot[[1]])
plot(perf_precedes_plot[[2]], add = TRUE, col = 2)
plot(perf_precedes_plot[[3]], add = TRUE, col = 3)
plot(perf_precedes_plot[[4]], add = TRUE, col = 4)
plot(perf_precedes_plot[[5]], add = TRUE, col = 5)
plot(perf_precedes_plot[[6]], add = TRUE, col = 6)
plot(perf_precedes_plot[[7]], add = TRUE, col = 7)
plot(perf_precedes_plot[[8]], add = TRUE, col = 8)
plot(perf_precedes_plot[[9]], add = TRUE, col = 9)
plot(perf_precedes_plot[[10]], add = TRUE, col = 10)

# Generate the overall confusion matrix and AUC
pred_precedes_c = prediction(opti_predict_precedes_prob_c[, 2], data_fold_test$precedes_illness)
perf_precedes_auc = performance(pred_precedes_c, measure = "auc")
perf_precedes_auc@y.values[[1]]

confusion_precedes = confusionMatrix(opti_predict_precedes_class_c, data_fold_test$precedes_illness, 
                                     positive = "TRUE")
confusion_precedes
```
