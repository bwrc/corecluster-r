graphics.off()
rm(list = ls())

library(corecluster)
library(xtable)
library(clue)

## ----------------------------------------------------------------------------------------------------
## Define paths
## ----------------------------------------------------------------------------------------------------

datadir_in_true      <- "results/truedist/"
datadir_in_bootstrap <- "results/bootstrap/"

## ----------------------------------------------------------------------------------------------------

alg_list <- c("hclust", "kmeanspp", "rf", "svm", "tclust", "tkmeans")

## ----------------------------------------------------------------------------------------------------

res_synthetic <- sapply(alg_list, function(i) get_agreement(datadir_in_true, datadir_in_bootstrap, "synthetic", i), USE.NAMES = TRUE, simplify = FALSE)
res_kdd       <- sapply(alg_list, function(i) get_agreement(datadir_in_true, datadir_in_bootstrap, "kdd", i),  USE.NAMES = TRUE, simplify = FALSE)

## ----------------------------------------------------------------------------------------------------

confusion_matrix_to_latex(alg_list, res_synthetic, res_kdd)

## ----------------------------------------------------------------------------------------------------

