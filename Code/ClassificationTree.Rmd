---
title: "Classification Tree"
author: "Zhirou Zhou"
date: "12/13/2019"
output: html_document
---

```{r classification tree for is_illness}
data_illness$is_illness = factor(data_illness$is_illness)
data_illness$precedes_illness = factor(data_illness$precedes_illness)

# Prepare 10-fold cross-validation blocked by Alias
nfold = 10
data_illness = data_illness %>% group_by(Alias)
data_illness$id = c()
data_illness$id = data_illness %>% group_indices(Alias)
size = floor(length(unique(data_illness$id)) / nfold)

set.seed(300)
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
#prop.table(table(data_fold[[1]]$is_illness))

# Function to get sensitivity, specificity, precision and recall
get_sensitivity = function(x) {
  sensitivity = x$byClass[1]
}

get_specificity = function(x) {
  specificity = x$byClass[2]
}

get_ppv = function(x) {
  ppv = x$byClass[3]
}

get_npv = function(x) {
  npv = x$byClass[4]
}

get_f1 = function(x) {
  recall = x$byClass[7]
}

# Set the grid of values of `minbucket` and `cp`
hyper_grid = expand.grid(minbucket = c(30, 100, 200, 300, 400, 900, 1200, 1500), cp = seq(0.01, 0.25, 0.02))

# 10-fold Cross-validation
trees_is = list()
trees_is_predict = list()
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
  trees_is[[l]] = rpart(is_illness ~ ., data = train, 
                        control = list(minbucket = hyper_grid$minbucket[l], 
                                       maxdepth = 30, xval = 10, cp = hyper_grid$cp[l], 
                                       parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
  }
  trees_is_predict[[i]] = lapply(trees_is, predict, test, type = "class")
}

# Combine the predictions by each tree
predict_tree = list()
predict_fold = trees_is_predict[[1]]
for (j in 1:nrow(hyper_grid)) {
  prediction = predict_fold[[j]]
  names(prediction) = NULL
  predict_tree[[j]] = as.factor(prediction)
}

for (i in 2:nfold) {
  predict_fold = trees_is_predict[[i]]
  for (j in 1:nrow(hyper_grid)) {
    prediction = predict_fold[[j]]
    names(prediction) = names(predict_tree[[j]])
    predict_tree[[j]] = c(as.character(predict_tree[[j]]), as.character(prediction))
    predict_tree[[j]] = as.factor(predict_tree[[j]])
  }
}

# Tune parameters by confusion matrices
trees_is_conf = list()
trees_is_conf = lapply(predict_tree, confusionMatrix, data_fold_test$is_illness, positive = "TRUE")
prune = hyper_grid %>% 
               mutate(test_sensitivity = map_dbl(trees_is_conf, get_sensitivity), 
                      test_specificity = map_dbl(trees_is_conf, get_specificity), 
                      test_ppv = map_dbl(trees_is_conf, get_ppv), 
                      test_npv = map_dbl(trees_is_conf, get_npv), 
                      test_f1 = map_dbl(trees_is_conf, get_f1))
prune = prune[order(prune$f1, decreasing = TRUE), ]
head(prune)

save(trees_is, trees_is_predict, trees_is_conf, prune, file = "trees_is.rdata")

prune_cp = prune[which(prune$minbucket == 50), ]
prune_minbucket = prune[which(prune$cp == 0.03), ]
prune_cp_err = prune_cp %>% gather('metric', 'err', test_sensitivity, test_specificity, test_ppv, test_npv,
                                   test_f1)
prune_minbucket_err = prune_minbucket %>% gather('metric', 'err', test_sensitivity, test_specificity, 
                                               test_ppv, test_npv, test_f1)
plot_cp = ggplot(prune_cp_err, aes(x = cp, color = metric, y = err)) + geom_line()
plot_minbucket = ggplot(prune_minbucket_err, aes(x = minbucket, color = metric, y = err)) + geom_line()
plot_cp
plot_minbucket

# Grow the tree and plot using optimal `minsplit` and `cp`
data_true = as.data.frame(data_illness[which(data_illness$is_illness == TRUE), ])
data_false = as.data.frame(data_illness[which(data_illness$is_illness == FALSE), ])
times = floor(table(data_illness$is_illness)[1] / table(data_illness$is_illness)[2])
data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
data_opti = as.data.frame(rbind(data_true, data_false))
data_train = as.data.frame(subset(data_opti, select = -c(Alias, visit_id, precedes_illness)))
data_test = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness, precedes_illness)))
data_test_1 = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, precedes_illness)))
opti_tree_is = rpart(is_illness ~ ., data = data_train, 
                        control = list(minbucket = 50, maxdepth = 30, xval = 10, cp = 0.03, 
                                       parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
rparty_tree_is = as.party(opti_tree_is)
rparty_tree_is
plot(rparty_tree_is, main = "Pruned Classification Tree for is_illness", 
     tp_args = list(text = "vertical", ymax = 1))

# Cross-validation by optimal tree
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
  trees_is[[i]] = rpart(is_illness ~ ., data = train, 
                        control = list(minbucket = 50, 
                                       maxdepth = 30, xval = 10, cp = 0.03, 
                                       parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
  opti_predict_is_class[[i]] = predict(trees_is[[i]], test, type = "class")
  opti_predict_is_prob[[i]] = predict(trees_is[[i]], test, type = "prob")
}

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

# Compare performance of each fold using confusion matrices
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

```{r classification tree for precedes_illness}
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

# Set the sequence of values of `minsplit` and `cp`
hyper_grid = expand.grid(minsplit = c(100, 300, 600, 900, 1200, 2700, 3600, 3700, 3800, 3900), 
                         cp = seq(0.005, 0.11, 0.01))

# 10-fold Cross-validation
trees_precedes_predict = list()
trees_precedes = list()
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
  trees_precedes[[l]] = rpart(precedes_illness ~ ., data = train, 
                              control = list(minsplit = hyper_grid$minsplit[l], 
                                             maxdepth = 30, xval = 10, cp = hyper_grid$cp[l], 
                                             parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
  }
  trees_precedes_predict[[i]] = lapply(trees_precedes, predict, test, type = "class")
}

# Combine the predictions by each tree
predict_tree = list()
predict_fold = trees_precedes_predict[[1]]
for (j in 1:nrow(hyper_grid)) {
  prediction = predict_fold[[j]]
  names(prediction) = NULL
  predict_tree[[j]] = as.factor(prediction)
}

for (i in 2:nfold) {
  predict_fold = trees_precedes_predict[[i]]
  for (j in 1:nrow(hyper_grid)) {
    prediction = predict_fold[[j]]
    names(prediction) = names(predict_tree[[j]])
    predict_tree[[j]] = c(as.character(predict_tree[[j]]), as.character(prediction))
    predict_tree[[j]] = as.factor(predict_tree[[j]])
  }
}

# Tuning parameters by confusion matrices
trees_precedes_conf = list()
trees_precedes_conf = lapply(predict_tree, confusionMatrix, data_fold_test$precedes_illness, positive = "TRUE")
prune_precedes = hyper_grid %>% 
               mutate(test_sensitivity = map_dbl(trees_precedes_conf, get_sensitivity), 
                      test_specificity = map_dbl(trees_precedes_conf, get_specificity), 
                      test_precision = map_dbl(trees_precedes_conf, get_precision), 
                      test_recall = map_dbl(trees_precedes_conf, get_recall), 
                      test_f1 = map_dbl(trees_precedes_conf, get_f1))
prune_precedes = prune_precedes[order(prune_precedes$test_precision, decreasing = TRUE), ]
head(prune_precedes)
save(trees_precedes, trees_precedes_predict, trees_precedes_conf, prune_precedes, 
     file = "trees_precedes.rdata")

prune_precedes_cp = prune_precedes[which(prune_precedes$minsplit == 300), ]
prune_precedes_minsplit = prune_precedes[which(prune_precedes$cp == 0.005), ]
prune_precedes_cp_err = prune_precedes_cp %>% gather('metric', 'err', test_sensitivity, test_specificity, test_precision, test_recall, test_f1)
prune_precedes_minsplit_err = prune_precedes_minsplit %>% gather('metric', 'err', test_sensitivity, test_specificity, test_precision, test_recall, test_f1)
plot_cp = ggplot(prune_precedes_cp_err, aes(x = cp, color = metric, y = err)) + geom_line()
plot_minsplit = ggplot(prune_precedes_minsplit_err, aes(x = minsplit, color = metric, y = err)) + geom_line()
plot_cp
plot_minsplit

# Prune the tree and plot using optimal `minsplit` and `cp`
data_true = as.data.frame(data_illness[which(data_illness$precedes_illness == TRUE), ])
data_false = as.data.frame(data_illness[which(data_illness$precedes_illness == FALSE), ])
times = floor(table(data_illness$precedes_illness)[1] / table(data_illness$precedes_illness)[2])
data_true = data_true[rep(seq_len(nrow(data_true)), each = times), ]
data_opti = as.data.frame(rbind(data_true, data_false))
data_train = as.data.frame(subset(data_opti, select = -c(Alias, visit_id, is_illness)))
data_test = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness, precedes_illness)))
data_test_1 = as.data.frame(subset(data_illness, select = -c(Alias, visit_id, is_illness)))

opti_tree_precedes = rpart(precedes_illness ~ ., data = data_train, 
                           control = list(minsplit = 1200, 
                                          maxdepth = 30, xval = 10, cp = 0.005, 
                                          parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
rparty_tree_precedes = as.party(opti_tree_precedes)
rparty_tree_precedes
plot(rparty_tree_precedes, tp_args = list(text = "vertical", ymax = 1), 
     main = "Pruned Classification Tree for precedes_illness")
plot(rparty_tree_precedes, tp_args = list(text = "vertical", ymax = 1))

# 10-fold Cross-validation using optimal tree
opti_predict_precedes_class = list()
opti_predict_precedes_prob = list()
trees_precedes = list()
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
  trees_precedes[[i]] = rpart(precedes_illness ~ ., data = train, 
                           control = list(minsplit = 1200, 
                                          maxdepth = 30, xval = 10, cp = 0.005, 
                                          parms = list(loss = matrix(c(0, 1, 3, 0), nrow = 2))))
  opti_predict_precedes_class[[i]] = predict(trees_precedes[[i]], test, type = "class")
  opti_predict_precedes_prob[[i]] = predict(trees_precedes[[i]], test, type = "prob")
}

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
  opti_predict_precedes_class_thre[[i]] = ifelse(opti_predict_precedes_prob_c[, 2] > threshold[i], TRUE, FALSE)
  opti_predict_precedes_class_thre[[i]] = factor(opti_predict_precedes_class_thre[[i]], 
                                                 levels = c(FALSE, TRUE))
}
confusion_precedes = list()
confusion_precedes = lapply(opti_predict_precedes_class_thre, confusionMatrix, 
                            data_fold_test$precedes_illness, positive = "TRUE")
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
  prob = opti_predict_is_prob[[i]]
  opti_predict_is_class[[i]] = ifelse(prob[, 2] > 0.65, TRUE, FALSE)
  opti_predict_is_class[[i]] = factor(opti_predict_is_class[[i]], levels = c(FALSE, TRUE))
}

# Compare performance of each fold using confusion matrices
opti_conf = list()
for (i in 1:nfold) {
  opti_conf[[i]] = confusionMatrix(opti_predict_precedes_class[[i]], data_fold[[i]]$precedes_illness, 
                                   positive = "TRUE")
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
