---
title: "Feature Selection"
author: "Zhirou Zhou"
date: "12/13/2019"
output: html_document
---

```{r feature selection}
apply(data_illness[, c(4, 5, 10:25, 27:46, 48:105)], MARGIN = 2, table)
apply(data_illness[, c(6:9, 26, 47)], MARGIN = 2, sd)

# check correlation
data_cor = data_illness[, c(6:8, 26, 9, 47)]
cor(data_cor)

# collapse infrequent discrete classes
for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 50] == 4) {
    data_illness[i, 50] = 3
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 53] > 5) {
    data_illness[i, 53] = 5
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 103] > 49) {
    data_illness[i, 103] = 49
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 104] > 73) {
    data_illness[i, 104] = 73
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 105] > 31) {
    data_illness[i, 105] = 31
  }
}

# duplicate subjects with infrequent binary category
for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 55] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 15), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 58] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 5), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 63] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 10), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 64] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 5), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 68] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 10), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 70] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 5), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 71] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 5), ])
  }
}

for (i in 1:nrow(data_illness)) {
  if (data_illness[i, 91] == TRUE) {
    data_illness = rbind(data_illness, data_illness[rep(i, 3), ])
  }
}

# remove extremely unbalanced categorical variables
data_illness$`C. pneu` = NULL
data_illness$`MMR (dose 1)` = NULL

# remove highly correlated features
data_illness$birth.length = NULL
data_illness$birth.weights = NULL
data_illness$head.circumference = NULL

# save `ndata_illness`
save(data_illness, file = "ndata_illness.RData")
```
