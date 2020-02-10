---
title: "Preprocessing"
author: "Zhirou Zhou"
date: "12/13/2019"
output: html_document
---

```{r setup, warning = FALSE, message = FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(MASS)
library(tidyverse)
library(phytools)
library(zoo)
library(glmnet)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(partykit)
library(caret)
library(purrr)
library(randomForest)
```

```{r read in data, message = FALSE, warning = FALSE}
files = data_frame(files = list.files('train_360_DOL/', pattern = '*.tsv', full.names = TRUE)) %>% 
    mutate(names = str_match(files, '//([a-z12_]+)')[,2])

files = files %>% rowwise() %>% 
    mutate(data = list(read_tsv(files)), nrow = nrow(data), nfeature = ncol(data))

get_tbl = function(name) filter(files, names == name) %>% .$data %>% .[[1]]

# Read in 14 tables
base = get_tbl('base')
demo = get_tbl('demo')
fam = get_tbl('fam')
oxy = get_tbl('oxy')
preg = get_tbl('preg')
flowcytometry = get_tbl('flowcytometry')
fup1 = get_tbl('fup1')
fup2 = get_tbl('fup2')
illness_control = get_tbl('illness_control')
nas_microbiome = get_tbl('nas_microbiome')
rec_microbiome = get_tbl('rec_microbiome')
thr_microbiome = get_tbl('thr_microbiome')
tlda = get_tbl('tlda')
vaccines = get_tbl('vaccines')
#nas_rooted_tree = read.newick("nas_rooted_tree.nwk")
#rec_rooted_tree = read.newick("rec_rooted_tree.nwk")
```

```{r preprocess base, warning = FALSE}
# Preprocessing `base`, resulting in `base_new`
base = base[order(base$Alias), ]
base_new = base %>% select(Alias)

# Scale variables
base_new$birth.weights = (base$`Birth Weight (gms)` - mean(base$`Birth Weight (gms)`)) / sd(base$`Birth Weight (gms)`)
base_new$head.circumference = (base$`Head Circumference (cm)` - mean(base$`Head Circumference (cm)`)) / sd(base$`Head Circumference (cm)`)
base_new$birth.length = (base$`Birth Length (cm)` - mean(base$`Birth Length (cm)`)) / sd(base$`Birth Length (cm)`)
base_new$temp = (base$`Temp at first NICU admission (C)` - mean(base$`Temp at first NICU admission (C)`)) / sd(base$`Temp at first NICU admission (C)`)

base_new = base_new %>% mutate(apgar.1 = base$`APGAR at 1 min`)
base_new = base_new %>% mutate(apgar.5 = base$`APGAR at 5 min`)

# Combine variables about birth order
base_new$multiple = ifelse(base$`Multiple Birth` == "No", 0, 
                           base$`Birth Order` / base$`Total # of Births`)

# Change to categorical variables
base_new$birth.location = as.factor(ifelse(base$`Birth Location` == "Born inside the study center", 1, 0))
base_new$stablization = as.factor(ifelse(base$`Stabilization Procedure Provided` == "Yes", 1, 0))
base_new$supo2 = as.factor(ifelse(is.na(base$`Supplemental O2`), 0, 1))
base_new$cpap = as.factor(ifelse(is.na(base$CPAP), 0, 1))
base_new$ventilation = as.factor(ifelse(is.na(base$`Non-invasive positive pressure ventilation with flow inflating or self inflating bag`), 0, 1))
base_new$tpiece = as.factor(ifelse(is.na(base$`T-Piece resuscitator`), 0, 1))
base_new$intubation = as.factor(ifelse(is.na(base$Intubation), 0, 1))
base_new$chest.compression = as.factor(ifelse(is.na(base$`Chest Compression`), 0, 1))
base_new$cardiac.drugs = as.factor(ifelse(is.na(base$`Cardiac drugs (Epinenphrine)`), 0, 1))
base_new$surfactant.admin = as.factor(ifelse(is.na(base$`Surfactant administration`), 0, 1))
base_new$prophylactic.indomethacin = as.factor(ifelse(base$`Prophylactic Indomethacin given within first 24 hours of life` == "Yes", 1, 0))

sum(is.na(base_new))
```

```{r preprocess demo, warning = FALSE}
# Preprocessing `demo`, resulting in `demo_new`
demo = demo[order(demo$Alias), ]
demo_new = demo %>% select(Alias)

# Change to categorical variables
demo_new$gender = as.factor(ifelse(demo$Gender == "Male", 1, 0))

demo_new = demo_new %>% mutate(birth.season = demo$`Birth Season`)
demo_new$birth.season = demo_new$birth.season %>% as.factor()
levels(demo_new$birth.season) = c(3, 1, 0, 2)
demo_new$birth.season = demo_new$birth.season %>% as.numeric()

# Scale variables
demo_new$gestational.age = demo$`GA at Birth (BLIS Calculated)` - 39

sum(is.na(demo_new))
```

```{r preprocess fam, warning = FALSE}
# Preprocessing `fam`, resulting in `fam_new`
fam = fam[order(fam$Alias), ]
fam_new = fam %>% select(Alias)

# Change to categorical variables
fam_new = fam_new %>% mutate(education = fam$`Mother Education`)
fam_new$education = fam_new$education %>% as.factor()
levels(fam_new$education) = c(1, 4, 5, 2, 0, 3, NA)
fam_new$education = fam_new$education %>% as.numeric()

# fill the unknown as means
for (i in 1:nrow(fam_new)) {
  if (is.na(fam_new$education[i]) == TRUE) {
    fam_new$education[i] = mean(fam_new$education, na.rm = TRUE)
  }
}

sum(is.na(fam_new))
```

```{r preprocess oxy, warning = FALSE}
# Preprocessing `oxy`, resulting in `oxy_new`
oxy = oxy[order(oxy$Alias), ]
oxy_new = oxy %>% select(Alias)

# Change to categorical variables
for (i in 1:nrow(oxy)) {
  if (oxy$`Auc O2exposure 7d`[i] == 0) {
    oxy_new$auc[i] = 0
  } else if (oxy$`Auc O2exposure 7d`[i] > 0 & oxy$`Auc O2exposure 7d`[i] <= 51) {
    oxy_new$auc[i] = 1
  } else if (oxy$`Auc O2exposure 7d`[i] > 51 & oxy$`Auc O2exposure 7d`[i] <= 100) {
    oxy_new$auc[i] = 2
  } else if (oxy$`Auc O2exposure 7d`[i] > 100 & oxy$`Auc O2exposure 7d`[i] <= 500) {
    oxy_new$auc[i] = 3
  } else if (oxy$`Auc O2exposure 7d`[i] > 500 & oxy$`Auc O2exposure 7d`[i] <= 1000) {
    oxy_new$auc[i] = 4
  } else if (oxy$`Auc O2exposure 7d`[i] > 1000 & oxy$`Auc O2exposure 7d`[i] <= 2000) {
    oxy_new$auc[i] = 5
  } else if (oxy$`Auc O2exposure 7d`[i] > 2000 & oxy$`Auc O2exposure 7d`[i] <= 4000) {
    oxy_new$auc[i] = 6
  } else {oxy_new$auc[i] = 7}
}

for (i in 1:nrow(oxy_new)) {
  if ((oxy$`Auc O2exposure 14d`[i] - oxy$`Auc O2exposure 7d`[i]) > oxy$`Auc O2exposure 7d`[i]) {
    oxy_new$auc[i] = oxy_new$auc[i] + 1
  }
}

oxy_new$auc = as.factor(oxy_new$auc)

sum(is.na(oxy_new))
```

```{r preprocess preg and join, warning = FALSE}
# Preprocessing `preg`, resulting in `preg_new`
preg = preg[order(preg$Alias), ]
preg_new = preg %>% select(Alias)

# Change to categorical variables
# diabetes
for (i in 1:nrow(preg)) {
  if (preg[i, 1] == "No") {
    preg_new$diabetes[i] = 0
  } else if(is.na(preg[i, 2]) == FALSE & preg[i, 2] == "Yes") {
    preg_new$diabetes[i] = 1
  } else if (is.na(preg[i, 2]) == TRUE) {
    preg_new$diabetes[i] = NA
  } else {preg_new$diabetes[i] = 2}
}
# fill the unknown as means
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$diabetes[i]) == TRUE) {
    preg_new$diabetes[i] = mean(preg_new$diabetes, na.rm = TRUE)
  }
}

# hypertension
for (i in 1:nrow(preg)) {
  if (preg[i, 3] == "No") {
    preg_new$hypertension[i] = 0
  } else if(is.na(preg[i, 4]) == FALSE & preg[i, 4] == "Yes") {
    preg_new$hypertension[i] = 1
  } else if (is.na(preg[i, 4]) == TRUE) {
    preg_new$hypertension[i] = NA
  } else {preg_new$hypertension[i] = 2}
}
# fill the unknown as means
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$hypertension[i]) == TRUE) {
    preg_new$hypertension[i] = mean(preg_new$hypertension, na.rm = TRUE)
  }
}

# asthma
for (i in 1:nrow(preg)) {
  if (preg[i, 5] == "No") {
    preg_new$asthma[i] = 0
  } else if(is.na(preg[i, 6]) == FALSE & preg[i, 6] == "Yes") {
    preg_new$asthma[i] = 1
  } else if (is.na(preg[i, 6]) == TRUE) {
    preg_new$asthma[i] = NA
  } else {preg_new$asthma[i] = 2}
}
# fill the unknown as means
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$asthma[i]) == TRUE) {
    preg_new$asthma[i] = mean(preg_new$asthma, na.rm = TRUE)
  }
}

# membrane rupture
for (i in 1:nrow(preg)) {
  if (preg[i, 27] == "No") {
    preg_new$rupture[i] = 0
  } else if(is.na(preg[i, 28]) == FALSE & preg[i, 28] == "No") {
    preg_new$rupture[i] = 1
  } else if (is.na(preg[i, 28]) == TRUE) {
    preg_new$rupture[i] = NA
  } else {preg_new$rupture[i] = 2}
}
# fill the unknown as means
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$rupture[i]) == TRUE) {
    preg_new$rupture[i] = mean(preg_new$rupture, na.rm = TRUE)
  }
}

# placental pathology
for (i in 1:nrow(preg)) {
  if (preg[i, 30] == "No") {
    preg_new$placental.pathology[i] = 0
  } else if(is.na(preg[i, 31]) == FALSE & preg[i, 31] == "No") {
    preg_new$placental.pathology[i] = 1
  } else if (is.na(preg[i, 31]) == TRUE) {
    preg_new$placental.pathology[i] = NA
  } else {preg_new$placental.pathology[i] = 2}
}
# fill the unknown as means
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$placental.pathology[i]) == TRUE) {
    preg_new$placental.pathology[i] = mean(preg_new$placental.pathology, na.rm = TRUE)
  }
}

# other binary variables
preg_new$other.respiratory.illness = as.factor(ifelse(preg[, 7] == "Yes", 1, 0))
preg_new$prolong.pregnancy = as.factor(ifelse(preg[, 9] == "Yes", 1, 0))
preg_new$mother.smoke = as.factor(ifelse(preg[, 19] == "Yes", 1, 0))
preg_new$other.smoke = as.factor(ifelse(preg[, 20] == "Yes", 1, 0))
preg_new$alcohol = as.factor(ifelse(preg[, 21] == "Yes", 1, 0))
preg_new$placental.abruption = as.factor(ifelse(preg[, 26] == "Yes", 1, 0))
preg_new$chorioamnionitis = as.factor(ifelse(preg[, 29] == "Yes", 1, 0))
preg_new$antibiotics = as.factor(ifelse(preg[, 32] == "Yes", 1, 0))
preg_new$corticosteroids = as.factor(ifelse(preg[, 38] == "Yes", 1, 0))
preg_new$magnesium.sulfate = as.factor(ifelse(preg[, 41] == "Yes", 1, 0))
preg_new$onset = as.factor(ifelse(preg[, 43] == "Yes", 0, 1))
preg_new$delivery = as.factor(ifelse(preg[, 44] == "Caesarean Section", 0, 1))
preg_new$preeclampsia = as.factor(ifelse(preg[, 43] == "Yes", 1, 0))

# Scale BMI
preg_new$bmi = as.numeric(unlist(preg[, 45]))
for (i in 1:nrow(preg_new)) {
  if (is.na(preg_new$bmi[i]) == TRUE) {
    preg_new$bmi[i] = mean(preg_new$bmi, na.rm = TRUE)
  }
}
preg_new$bmi = (preg_new$bmi - mean(preg_new$bmi)) / sd(preg_new$bmi)

sum(is.na(oxy_new))

# Join first five dataset as `profiles`
profiles = full_join(base_new, demo_new, by = "Alias") %>% full_join(fam_new, by = "Alias") %>% 
           full_join(oxy_new, by = "Alias") %>% full_join(preg_new, by = "Alias")
sum(is.na(profiles))
```

```{r preprocess illness_control and join}
# Preprocessing `illness_control`, resulting in `illness_new`
illness_new = illness_control[,c(2,3,5,1,4)]
illness_new = illness_new[order(illness_new$Alias, illness_new$visit_id), ]
sum(is.na(illness_new))

# Join `profiles` to `illness_new`, result in `data_illness`
data_illness = left_join(illness_new, profiles, by = c("Alias"))
sum(is.na(data_illness))
```

```{r preprocess fup1 and join}
# Preprocessing `fup1`, resulting in `fup1_new`
fup1 = fup1[order(fup1$Alias, fup1$pCGA), ]
fup1_new = fup1 %>% select(Alias, pCGA)

# Change to categorical variables
fup1_new$receive.breast.milk = as.factor(ifelse(fup1[, 1] == "Yes", 1, 0))
fup1_new$non.milk.foods = as.factor(ifelse(fup1[, 5] == "Yes", 1, 0))

# Interpolate missing values
fup1_new = fup1_new %>% group_by(Alias) %>% 
           mutate(receive.breast.milk = ifelse(is.na(receive.breast.milk),
                                               na.locf(fup1_new$receive.breast.milk, 
                                                       fromLast = TRUE), receive.breast.milk))
fup1_new = fup1_new %>% group_by(Alias) %>% 
           mutate(non.milk.foods = ifelse(is.na(non.milk.foods), 
                                          na.locf(fup1_new$non.milk.foods, fromLast = TRUE),
                                          non.milk.foods))

# Join `fup1_new` to `data_illness`
data_illness = left_join(data_illness, fup1_new, by = c("Alias", "pCGA"))
first_value = function(x) {x[1]}
data_illness = data_illness %>% group_by(Alias, visit_id, pCGA) %>% summarise_all(first_value)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate(receive.breast.milk = ifelse(is.na(receive.breast.milk),
                                               na.locf(data_illness$receive.breast.milk, 
                                                       fromLast = TRUE), receive.breast.milk))
data_illness = data_illness %>% group_by(Alias) %>% 
               mutate(non.milk.foods = ifelse(is.na(non.milk.foods), 
                                          na.locf(data_illness$non.milk.foods, fromLast = TRUE),
                                          non.milk.foods))
sum(is.na(data_illness))
```

```{r preprocess fup2 and join, warning = FALSE}
# Preprocessing `fup2`, resulting in `fup2_new`
fup2 = fup2[order(fup2$Alias, fup2$pCGA), ]
fup2_new = fup2 %>% select(Alias, pCGA)

# Number of smokers at home
fup2_new = fup2_new %>% mutate(smoker = fup2$`How many smokers in home`)
for (i in 1:nrow(fup2_new)) {
  if (is.na(fup2_new$smoker[i])) {
    fup2_new$smoker[i] = 0
  } else if (fup2[i, 3] == "Yes") {
    fup2_new$smoker[i] = fup2_new$smoker[i] + 1
  }
}

# Exposed to smoke at home
fup2_new = fup2_new %>% mutate(exposed.to.smoke = fup2$`Exposed to smoke at HOME`)
fup2_new$exposed.to.smoke = fup2_new$exposed.to.smoke %>% as.factor()
levels(fup2_new$exposed.to.smoke) = c(0, 1)

# heater, stove or fireplace
fup2_new = fup2_new %>% mutate(fire = fup2$`Kerosene heater, wood burning stove, or fireplace`)
fup2_new$fire = fup2_new$fire %>% as.factor()
levels(fup2_new$fire) = c(0, 1)

# Number of pets
fup2_new = fup2_new %>% mutate(pets = fup2$`How many pets`)
for (i in 1:nrow(fup2_new)) {
  if (is.na(fup2_new$pets[i])) {
    fup2_new$pets[i] = 0
  } else if (fup2[i, 15] == "Yes") {
    fup2_new$pets[i] = fup2_new$pets[i] - 1
  }
}

# Interpolate missing values
fup2_new = fup2_new %>% group_by(Alias) %>% 
           mutate(fire = ifelse(is.na(fire), na.locf(fup2_new$fire, fromLast = TRUE), fire))
sum(is.na(fup2_new))

# Join `fup2_new` to `data_illness`
data_illness = left_join(data_illness, fup2_new, by = c("Alias", "pCGA"))
data_illness = data_illness %>% group_by(Alias) %>% 
               mutate(smoker = ifelse(is.na(smoker), na.locf(data_illness$smoker, fromLast = TRUE), smoker),
                      exposed.to.smoke = ifelse(is.na(exposed.to.smoke), 
                                                na.locf(data_illness$exposed.to.smoke, fromLast = TRUE),
                                                exposed.to.smoke), 
                      fire = ifelse(is.na(fire), na.locf(data_illness$fire, fromLast = TRUE), fire), 
                      pets = ifelse(is.na(pets), na.locf(data_illness$pets, fromLast = TRUE), pets))

sum(is.na(data_illness))
```

```{r preprocess tlda and join}
# Preprocessing `tlda`, resulting in `tlda_new`
tlda_new = tlda %>% select(Target, Positive, Alias, visit_id) %>% spread(Target, Positive)
tlda_new = tlda_new[order(tlda_new$Alias, tlda_new$visit_id), ]

carry_forward = function(x) {
  for(i in 1:(length(x)-1)) if(is.na(x[i])) x[i] = x[i + 1]
  x
}
carry_forward_2 = function(x) {
  for(i in 1:(length(x)-1)) if(is.na(x[i])) x[i] = x[i + 2]
  x
}
carry_backward = function(x) {
  for(i in seq_along(x)[-1]) if(is.na(x[i])) x[i] = x[i - 1]
  x
}
tlda_new = tlda_new %>% group_by(Alias) %>% mutate_at(vars(Adeno:Urea), carry_backward)
sum(is.na(tlda_new))

# Join `tlda_new` to `data_illness`
data_illness = left_join(data_illness, tlda_new, by = c("Alias", "visit_id"))
data_illness = data_illness %>% group_by(Alias) %>% mutate_at(vars(Adeno:Urea), carry_backward) %>%
               mutate_at(vars(Adeno:Urea), carry_forward) %>% mutate_at(vars(Adeno:Urea), carry_forward_2)
data_illness = data_illness %>% group_by(Alias) %>% mutate_at(vars(Adeno:Urea), carry_backward) %>%
               mutate_at(vars(Adeno:Urea), carry_forward) %>% mutate_at(vars(Adeno:Urea), carry_forward_2)

sum(is.na(data_illness))
```

```{r preprocess vaccines and join, warning = FALSE}
# Preprocessing `vaccines`, resulting in `vaccines_new`
vaccines_new = vaccines %>% select(Alias, pCGA, Key, `Vaccination Type`)
vaccines_new$`Vaccination Type` = 1
vaccines_new = vaccines_new %>% spread(Key, `Vaccination Type`)
vaccines_new = vaccines_new[order(vaccines_new$Alias, vaccines_new$pCGA), ]

vaccines_new = vaccines_new %>% group_by(Alias) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_backward)
vaccines_new[is.na(vaccines_new)] = 0
sum(is.na(vaccines_new))

# Join `vaccines_new` to `data_illness`
data_illness = left_join(data_illness, vaccines_new, by = c("Alias", "pCGA"))

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_backward) %>%
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward_2)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_backward) %>%
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward_2)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_backward) %>%
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward) %>% 
               mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), carry_forward_2)

# fill the unknown as most frequent result among same `pCGA`
input_mode = function(x) {
  uniqx = unique(na.omit(x))
  mode = uniqx[which.max(tabulate(match(x, uniqx)))]
  for(i in seq_along(x)) if(is.na(x[i])) x[i] = mode
  x
}

data_illness = data_illness %>% group_by(pCGA) %>% 
             mutate_at(vars(`DTaP (2 months)`:`RSV prophylaxis (1st dose)`), input_mode)

sum(is.na(data_illness))
```

```{r preprocess microbiome and join}
# Preprocessing three datasets regarding microbiome, resulting in `microbiome_new`
nas_microbiome = nas_microbiome[order(nas_microbiome$Alias, nas_microbiome$visit_id, nas_microbiome$pCGA), ]
nas_new = nas_microbiome %>% select(Alias, visit_id, pCGA)
nas_new$nas_microbiome = 0
for (i in 1:nrow(nas_microbiome)) {
  nas_new$nas_microbiome[i] = sum(nas_microbiome[i, -c(1:5)] > 0)
}

rec_microbiome = rec_microbiome[order(rec_microbiome$Alias, rec_microbiome$visit_id, rec_microbiome$pCGA), ]
rec_new = rec_microbiome %>% select(Alias, visit_id, pCGA)
rec_new$rec_microbiome = 0
for (i in 1:nrow(rec_microbiome)) {
  rec_new$rec_microbiome[i] = sum(rec_microbiome[i, -c(1:5)] > 0)
}

thr_microbiome = thr_microbiome[order(thr_microbiome$Alias, thr_microbiome$visit_id, thr_microbiome$pCGA), ]
thr_new = thr_microbiome %>% select(Alias, visit_id, pCGA)
thr_new$thr_microbiome = 0
for (i in 1:nrow(thr_microbiome)) {
  thr_new$thr_microbiome[i] = sum(thr_microbiome[i, -c(1:5)] > 0)
}

microbiome_new = full_join(nas_new, rec_new, by = c("Alias", "visit_id", "pCGA"))
microbiome_new = full_join(microbiome_new, thr_new, by = c("Alias", "visit_id", "pCGA"))
microbiome_new = microbiome_new %>% group_by(Alias) %>% 
                 mutate(nas_microbiome = ifelse(is.na(nas_microbiome), 
                                                na.locf(microbiome_new$nas_microbiome, fromLast = TRUE),
                                                nas_microbiome)) %>% 
                 mutate(rec_microbiome = ifelse(is.na(rec_microbiome), 
                                                na.locf(microbiome_new$rec_microbiome, fromLast = TRUE),
                                                rec_microbiome)) %>% 
                 mutate(thr_microbiome = ifelse(is.na(thr_microbiome), 
                                                na.locf(microbiome_new$thr_microbiome, fromLast = TRUE),
                                                thr_microbiome))
sum(is.na(microbiome_new))

# Join `microbiome_new` to `data_illness`
data_illness = left_join(data_illness, microbiome_new, by = c("Alias", "visit_id", "pCGA"))
data_illness = data_illness %>% group_by(Alias, visit_id, pCGA) %>% summarise_all(first_value)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_backward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward_2)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_backward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward_2)

data_illness = data_illness %>% group_by(Alias) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_backward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward) %>% 
               mutate_at(vars(nas_microbiome:thr_microbiome), carry_forward_2)

# fill the unknown as means
for (i in 1:nrow(data_illness)) {
  if (is.na(data_illness$nas_microbiome[i]) == TRUE) {
    data_illness$nas_microbiome[i] = mean(data_illness$nas_microbiome, na.rm = TRUE)
  }
  if (is.na(data_illness$rec_microbiome[i]) == TRUE) {
    data_illness$rec_microbiome[i] = mean(data_illness$rec_microbiome, na.rm = TRUE)
  }
  if (is.na(data_illness$thr_microbiome[i]) == TRUE) {
    data_illness$thr_microbiome[i] = mean(data_illness$thr_microbiome, na.rm = TRUE)
  }
}

sum(is.na(data_illness))
```
