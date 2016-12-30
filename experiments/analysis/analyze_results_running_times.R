graphics.off()
rm(list = ls())

library(corecluster)
library(xtable)
library(clue)

## ----------------------------------------------------------------------------------------------------
## Define paths
## ----------------------------------------------------------------------------------------------------

datadir_in_true        <- "results/truedist/"
datadir_in_bootstrap   <- "/tmp/res/"

## ----------------------------------------------------------------------------------------------------
classifier_list    <- c("hclust", "kmeanspp", "rf", "svm", "tclust", "tkmeans")
    prefix             <- "res"

    ds                 <- "synthetic"
    res_synthetic_true <- get_reslist(classifier_list, dataset = ds, base_path = datadir_in_true, prefix = prefix, is_list = FALSE)
    res_synthetic_true <- sapply(res_synthetic_true, function(i) get_running_time(i))

    ds                 <- "kdd"
    res_kdd_true <- get_reslist(classifier_list, dataset = ds, base_path = datadir_in_true, prefix = prefix, is_list = FALSE)
    res_kdd_true <- sapply(res_kdd_true, function(i) get_running_time(i))


datasets <- c("synthetic", "kdd", "iris", "wine", "glass", "breast-cancer-wisconsin", "yeast")
prefix   <- "resbs"
res_bs_all <- c()

get_running_time <- function(res) {
    difftime(res$time_stop, res$time_start, units = "secs")
}


for (ds in datasets) {
    print(ds)
    res_tmp <- get_reslist(classifier_list, dataset = ds, base_path = datadir_in_bootstrap, prefix = prefix, is_list = FALSE)
    res_tmp <-  sapply(res_tmp, function(i) get_running_time(i))
    res_bs_all <- rbind(res_bs_all, res_tmp)
}
rownames(res_bs_all) <- datasets

## Format the result into a table
tmp <- round(res_bs_all, digits = 0)
tmp <- formatC(tmp, digits = 0, format = "f")
