# 1) The kidney disease dataset was loaded into R. Imputation of missing data performed using “mice” package for continuous data and using mode to impute nominal missing data 
kd<-read.csv("kd.csv",header=TRUE)
##Missing data "1012" values
sum(is.na(kd))
library(mice)
imputed_Data=mice(kd, m=1, maxit = 50, method = 'pmm', seed = 500)
densityplot(imputed_Data)
kidneydisease<-complete(imputed_Data,1)
sum(is.na(kidneydisease))
##Nomianal data are missing in 9 columns "234" values
rbc_mode <- names(table(kidneydisease$rbc))[which.max(table(kidneydisease$rbc))]
kidneydisease$rbc[is.na(kidneydisease$rbc)] <- rbc_mode
pc_mode <- names(table(kidneydisease$pc))[which.max(table(kidneydisease$pc))]
kidneydisease$pc[is.na(kidneydisease$pc)] <- pc_mode
pcc_mode <- names(table(kidneydisease$pcc))[which.max(table(kidneydisease$pcc))]
kidneydisease$pcc[is.na(kidneydisease$pcc)] <- pcc_mode
ba_mode <- names(table(kidneydisease$ba))[which.max(table(kidneydisease$ba))]
kidneydisease$ba[is.na(kidneydisease$ba)] <- ba_mode
dm_mode <- names(table(kidneydisease$dm))[which.max(table(kidneydisease$dm))]
kidneydisease$dm[is.na(kidneydisease$dm)] <- dm_mode
htn_mode <- names(table(kidneydisease$htn))[which.max(table(kidneydisease$htn))]
kidneydisease$htn[is.na(kidneydisease$htn)] <- htn_mode
cad_mode <- names(table(kidneydisease$cad))[which.max(table(kidneydisease$cad))]
kidneydisease$cad[is.na(kidneydisease$cad)] <- cad_mode
appet_mode <- names(table(kidneydisease$appet))[which.max(table(kidneydisease$appet))]
kidneydisease$appet[is.na(kidneydisease$appet)] <- appet_mode
pe_mode <- names(table(kidneydisease$pe))[which.max(table(kidneydisease$pe))]
kidneydisease$pe[is.na(kidneydisease$pe)] <- pe_mode
ane_mode <- names(table(kidneydisease$ane))[which.max(table(kidneydisease$ane))]
kidneydisease$ane[is.na(kidneydisease$ane)] <- ane_mode
sum(is.na(kidneydisease))

# 2) Converting Nominal variables to numeric form by assigning 1s and 0s
unique_classification <- unique(kidneydisease$classification)
print(unique_classification)
## Classifying them into 1 and 0
kidneydisease$classify <- ifelse(kidneydisease$classification %in% "ckd", 1,0)
print(kidneydisease$classify)
sapply(kd, function(x) sum(is.na(x)))
sapply(kidneydisease, function(x) sum(is.na(x))) 
kidneydisease$RBC <- ifelse(kidneydisease$rbc %in% "normal", 0,1)
print(kidneydisease$RBC)
kidneydisease$PC <- ifelse(kidneydisease$pc %in% "normal", 0,1)
print(kidneydisease$PC)
kidneydisease$PCC <- ifelse(kidneydisease$pcc %in% "notpresent", 0,1)
print(kidneydisease$PCC)
kidneydisease$BA <- ifelse(kidneydisease$ba %in% "notpresent", 0,1)
print(kidneydisease$BA)
kidneydisease$BA <- ifelse(kidneydisease$ba %in% "notpresent", 0,1)
print(kidneydisease$BA)
kidneydisease$HTN <- ifelse(kidneydisease$htn %in% "no", 0,1)
print(kidneydisease$HTN)
kidneydisease$DM <- ifelse(kidneydisease$dm %in% "no", 0,1)
print(kidneydisease$DM)
kidneydisease$APPET <- ifelse(kidneydisease$appet %in% "good", 1,0)
print(kidneydisease$APPET)
kidneydisease$PE <- ifelse(kidneydisease$pe %in% "no", 0,1)
print(kidneydisease$PE)
kidneydisease$ANE <- ifelse(kidneydisease$ane %in% "no", 0,1)
print(kidneydisease$ANE)
kidneydisease$CAD <- ifelse(kidneydisease$cad %in% "yes", 1,0)
print(kidneydisease$CAD)	
kidneydisease$AGE <- ifelse(kidneydisease$age <= 52, 0, 1)
print(kidneydisease$AGE)
attributes <- c("age", "bp", "sg", "al", "su", "bgr", "bu", "sc", "sod", "pot", "hemo", "wc", "rc", "RBC", "PC", "PCC", "BA","HTN", "DM", "APPET", "PE", "ANE", "CAD","classify")
kd_data <- kidneydisease[, attributes]
ncol(kidneydisease)
colnames(kidneydisease)
##PieChart
class_counts <- table(kidneydisease$classify)
colors <- c("#8B0000", "#006400")  # Dark red for CKD, dark green for non-CKD
pie(class_counts, labels = paste0(names(class_counts), " (", round(100 * class_counts / sum(class_counts), 1), "%)"),
    col = colors)
title("Distribution of Kidney Disease Classes")
legend("topright", legend = names(class_counts), fill = colors)

# 3) Estimating population parameters using Bootstrap
selected_cols <- c("age", "bp", "sg", "al", "su", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")
bs_dataset <- kidneydisease[selected_cols]
head(bs_dataset)
library(boot)
bootstrap_results <- data.frame(variable = character(),
                                min = numeric(),
                                lower_ci = numeric(),
                                median = numeric(),
                                mean = numeric(),
                                upper_ci = numeric(),
                                max = numeric(),
                                stringsAsFactors = FALSE)
num_samples <- 1000  
for (var in colnames(bs_dataset)) {
  bootstrap_samples <- boot(bs_dataset[[var]], statistic = function(data, i) mean(data[i]), R = num_samples)
  bootstrap_ci <- boot.ci(bootstrap_samples, type = "perc") 
  bootstrap_results <- rbind(bootstrap_results, data.frame(variable = var,
                                                           min = min(bs_dataset[[var]]),
                                                           lower_ci = bootstrap_ci$percent[4],
                                                           median = median(bs_dataset[[var]]),
                                                           mean = mean(bootstrap_samples$t),
                                                           upper_ci = bootstrap_ci$percent[5],
                                                           max = max(bs_dataset[[var]]),
                                                           stringsAsFactors = FALSE))
}
print(bootstrap_results)

# 4) Bootstap resampling techniques performed separately on individual attributes to test if there is significance difference in means between the two groups based on bootstrap confidence interval:
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_age <- kidneydisease$age[kidneydisease$classify == 1]
nonckd_age <- kidneydisease$age[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_age, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_age, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (age):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (age):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (age) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (age) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_bp <- kidneydisease$bp[kidneydisease$classify == 1]
nonckd_bp <- kidneydisease$bp[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_bp, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_bp, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (bp):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (bp):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (bp) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (bp) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_sg <- kidneydisease$sg[kidneydisease$classify == 1]
nonckd_sg <- kidneydisease$sg[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_sg, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_sg, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (sg):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (sg):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (sg) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (sg) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_al <- kidneydisease$al[kidneydisease$classify == 1]
nonckd_al <- kidneydisease$al[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_al, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_al, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (al):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (al):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (al) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (al) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_su <- kidneydisease$su[kidneydisease$classify == 1]
nonckd_su <- kidneydisease$su[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_su, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_su, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (su):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (su):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (su) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (su) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)

mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_bgr <- kidneydisease$bgr[kidneydisease$classify == 1]
nonckd_bgr <- kidneydisease$bgr[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_bgr, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_bgr, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (bgr):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (bgr):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (bgr) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (bgr) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_bu <- kidneydisease$bu[kidneydisease$classify == 1]
nonckd_bu <- kidneydisease$bu[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_bu, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_bu, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (bu):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (bu):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (bu) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (bu) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_sc <- kidneydisease$sc[kidneydisease$classify == 1]
nonckd_sc <- kidneydisease$sc[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_sc, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_sc, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (sc):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (sc):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (sc) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (sc) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_sod <- kidneydisease$sod[kidneydisease$classify == 1]
nonckd_sod <- kidneydisease$sod[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_sod, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_sod, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (sod):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (sod):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (sod) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (sod) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_pot <- kidneydisease$pot[kidneydisease$classify == 1]
nonckd_pot <- kidneydisease$pot[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_pot, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_pot, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (pot):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (pot):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (pot) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (pot) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_hemo <- kidneydisease$hemo[kidneydisease$classify == 1]
nonckd_hemo <- kidneydisease$hemo[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_hemo, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_hemo, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (hemo):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (hemo):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (hemo) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (hemo) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_pcv <- kidneydisease$pcv[kidneydisease$classify == 1]
nonckd_pcv <- kidneydisease$pcv[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_pcv, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_pcv, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (pcv):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (pcv):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (pcv) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (pcv) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_wc <- kidneydisease$wc[kidneydisease$classify == 1]
nonckd_wc <- kidneydisease$wc[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_wc, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_wc, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (wc):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (wc):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (wc) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (wc) between CKD and non-CKD groups.\n")
}
##--------------------------------------------------------------------------------------------
library(boot)
library(dplyr)
mean_func <- function(data, indices) {
  mean(data[indices])
}
ckd_rc <- kidneydisease$rc[kidneydisease$classify == 1]
nonckd_rc <- kidneydisease$rc[kidneydisease$classify == 0]
bootstrap_res_ckd <- boot(ckd_rc, mean_func, R = 1000)
bootstrap_res_nonckd <- boot(nonckd_rc, mean_func, R = 1000)
ci_ckd <- boot.ci(bootstrap_res_ckd, type = "bca")
ci_nonckd <- boot.ci(bootstrap_res_nonckd, type = "bca")
cat("CI for CKD group (rc):", ci_ckd$bca[4], "-", ci_ckd$bca[5], "\n")
cat("CI for non-CKD group (rc):", ci_nonckd$bca[4], "-", ci_nonckd$bca[5], "\n")
if (ci_ckd$bca[4] <= ci_nonckd$bca[5] && ci_ckd$bca[5] >= ci_nonckd$bca[4]) {
  cat("No significant difference in means (rc) between CKD and non-CKD groups.\n")
} else {
  cat("Significant difference in means (rc) between CKD and non-CKD groups.\n")
}

#5) T-test:
  library(dplyr)
# Subset the data for CKD and non-CKD groups
ckd_data <- kidneydisease %>% filter(classify == 1)
nonckd_data <- kidneydisease %>% filter(classify == 0)
# Perform t-tests for each variable
variables <- c("age", "bp", "sg", "al", "su", "bgr", "bu", "sc", "sod", "pot", "hemo")
for (var in variables) {
  cat("Variable:", var, "\n")
  # Perform t-test
  t_test_result <- t.test(ckd_data[[var]], nonckd_data[[var]])
  # Print the results
  cat("t-value:", t_test_result$statistic, "\n")
  cat("p-value:", t_test_result$p.value, "\n")
  if (t_test_result$p.value >= 0.05) {
    cat("No significant difference in means between CKD and non-CKD groups.\n")
  } else {
    cat("Significant difference in means between CKD and non-CKD groups.\n")
  }
  cat("\n")
}

# 6) Monte Carlo Simulations to evaluate the Classification performed using LDA method and estimating probabilities based on the same model 
library(MASS)
library(caret)
num_simulations <- 10000
accuracy_values <- numeric(num_simulations)
precision_values <- numeric(num_simulations)
sensitivity_values <- numeric(num_simulations)
f1_score_values <- numeric(num_simulations)
class_probabilities <- matrix(0, nrow = nrow(kidneydisease), ncol = 2)
for (i in 1:num_simulations) {
  sampled_data <- kidneydisease[sample(nrow(kidneydisease), replace = TRUE), ]
  lda_model <- lda(classify ~ age + bp + sg + al + su + RBC + PC + PCC + BA + bgr + bu + sc + sod + pot + hemo + wc + rc + HTN + DM + APPET + PE + ANE + CAD, prior=c(0.375, 0.625), data = sampled_data)
  predicted_labels <- predict(lda_model, kidneydisease)$class
  predicted_labels <- factor(predicted_labels, levels = c("0", "1"))
  actual_labels <- factor(kidneydisease$classify, levels = c("0", "1"))
  accuracy <- sum(predicted_labels == actual_labels) / length(actual_labels)
  confusion_matrix <- confusionMatrix(predicted_labels, actual_labels)
  precision <- confusion_matrix$byClass["Precision"]
  sensitivity <- confusion_matrix$byClass["Recall"]
  f1_score <- confusion_matrix$byClass["F1"]
  accuracy_values[i] <- accuracy
  precision_values[i] <- precision
  sensitivity_values[i] <- sensitivity
  f1_score_values[i] <- f1_score
  predicted_probabilities <- predict(lda_model, kidneydisease, type = "posterior")
  class_probabilities <- class_probabilities + predicted_probabilities$posterior
}
mean_accuracy <- mean(accuracy_values)
mean_precision <- mean(precision_values)
mean_sensitivity <- mean(sensitivity_values)
mean_f1_score <- mean(f1_score_values)
sd_accuracy <- sd(accuracy_values)
sd_precision <- sd(precision_values)
sd_sensitivity <- sd(sensitivity_values)
sd_f1_score <- sd(f1_score_values)
print(paste("Mean Accuracy:", mean_accuracy))
print(paste("Mean Precision:", mean_precision))
print(paste("Mean Sensitivity (Recall):", mean_sensitivity))
print(paste("Mean F1 Score:", mean_f1_score))
print(paste("Standard Deviation Accuracy:", sd_accuracy))
print(paste("Standard Deviation Precision:", sd_precision))
print(paste("Standard Deviation Sensitivity (Recall):", sd_sensitivity))
print(paste("Standard Deviation F1 Score:", sd_f1_score))
confusion_matrix <- table(actual_labels, predicted_labels)
confusion_matrix
average_probabilities <- class_probabilities / num_simulations
probabilities <- round(average_probabilities, 4)
probabilities_with_classify <- cbind(probabilities, kidneydisease$classification)
correct_classification <- ifelse(probabilities_with_classify[, 2] >= 0.625 & probabilities_with_classify[, 3] == 1 |
                                   probabilities_with_classify[, 2] < 0.625 & probabilities_with_classify[, 3] == 0, "Incorrect", "Correct")
probabilities_with_classify <- cbind(probabilities_with_classify, correct_classification)
print(probabilities_with_classify)
correctly_classified_percentage <- sum(probabilities_with_classify[, "correct_classification"] == "Correct") / nrow(probabilities_with_classify) * 100
print(paste("Percentage of Correctly Classified Instances:", correctly_classified_percentage, "%"))

# 7) Feature Selection of the attributes using Permutation 
library(pROC)
full_model <- glm(classify ~ age + bp + sg + al + su + RBC + PC + PCC + BA + bgr + bu + sc + sod + pot + hemo + wc + rc + HTN + DM + APPET + PE + ANE + CAD, data = kidneydisease, family = binomial)
initial_probs <- predict(full_model, newdata = kidneydisease, type = "response")
initial_auc <- roc(kidneydisease$classify, initial_probs)$auc
num_permutations <- 10000
feature_importance <- numeric(length = length(coef(full_model)) - 1)
for (i in 2:length(coef(full_model))) {
  permuted_data <- kidneydisease
  permuted_data[, i] <- sample(permuted_data[, i])  # Permute the feature values
  permuted_probs <- predict(full_model, newdata = permuted_data, type = "response")
  permuted_auc <- roc(permuted_data$classify, permuted_probs)$auc
  feature_importance[i - 1] <- initial_auc - permuted_auc
}
feature_df <- data.frame(Attribute = names(coef(full_model))[-1], Importance = feature_importance)
anked_features <- feature_df[order(feature_df$Importance, decreasing = TRUE), ]
ranked_features$Importance <- round(ranked_features$Importance, 3)
print(ranked_features)

# 8) Permutation technique to test the significance of the correlation matrix
selected_cols <- c("age", "bp", "sg", "al", "su", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")
data <- kidneydisease[, selected_cols]
n_permutations <- 1000
permuted_corr_matrix <- cor(data)
permuted_corr_matrices <- list()
for (i in 1:n_permutations) {
  permuted_data <- data
  for (col in 1:ncol(permuted_data)) {
    permuted_data[, col] <- sample(permuted_data[, col])
  }
  permuted_corr_matrix <- cor(permuted_data)
  permuted_corr_matrices[[i]] <- permuted_corr_matrix
}
permuted_corr_matrix <- Reduce(`+`, permuted_corr_matrices) / n_permutations
round(permuted_corr_matrix, 3)
correlation_matrix <- cor(kidneydisease[, selected_cols])
round(correlation_matrix ,3)
permuted_corr_matrix <- Reduce(`+`, permuted_corr_matrices) / n_permutations
n_permutations <- 1000
n_variables <- ncol(data)
p_values <- matrix(0, nrow = n_variables, ncol = n_variables)
for (i in 1:n_variables) {
  for (j in 1:n_variables) {
    if (i != j) {
      observed_corr <- correlation_matrix[i, j]
      permuted_corrs <- sapply(permuted_corr_matrices, function(m) m[i, j])
      p_values[i, j] <- sum(abs(permuted_corrs) >= abs(observed_corr)) / n_permutations
      cat("p-value for correlation between", colnames(data)[i], "and", colnames(data)[j], ":", p_values[i, j], "\n")
    }
  }
}

# 9) Permutation technique to compare the LR Models
full_model <- glm(classify ~ sg + al + sc + hemo + bu + bgr + HTN + bp, data = kidneydisease, family = binomial)
original_model <- glm(classify ~ age + bp + sg + al + su + RBC + PC + PCC + BA + bgr + bu + sc + sod + pot + hemo + wc + rc + HTN + DM + APPET + PE + ANE + CAD, data = kidneydisease, family = binomial)
num_permutations <- 1000
observed_diff <- roc_response(full_model, kidneydisease) - roc_response(original_model, kidneydisease)
permuted_diffs <- numeric(num_permutations)
for (i in 1:num_permutations) {
  # Permute the outcome variable
  permuted_data <- kidneydisease
  permuted_data$classify <- sample(permuted_data$classify)
  permuted_full_model <- glm(classify ~ sg + al + sc + hemo + bu + bgr + HTN + bp, data = permuted_data, family = binomial)
  permuted_original_model <- glm(classify ~ age + bp + sg + al + su + RBC + PC + PCC + BA + bgr + bu + sc + sod + pot + hemo + wc + rc + HTN + DM + APPET + PE + ANE + CAD, data = permuted_data, family = binomial)
  permuted_diff <- roc_response(permuted_full_model, permuted_data) - roc_response(permuted_original_model, permuted_data)
  permuted_diffs[i] <- permuted_diff
}
p_value <- mean(abs(permuted_diffs) >= abs(observed_diff))
print(paste("Observed Difference in AUC-ROC:", observed_diff))
print(paste("Permutation Test p-value:", p_value))
if (observed_diff > 0 && p_value < 0.05) {
  print("Based on the AUC-ROC comparison and permutation testing, the full model is significantly better than the original model in terms of predictive accuracy for classifying kidney disease.")
} else {
  print("There is no significant difference in predictive accuracy between the full model and the original model for classifying kidney disease.")
}

