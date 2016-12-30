graphics.off()
rm(list = ls())

library(corecluster)
library(xtable)
library(clue)

## ----------------------------------------------------------------------------------------------------
## Define paths
## ----------------------------------------------------------------------------------------------------

data_base      <- "results/truedist/"
data_base_bs   <- "results/bootstrap/"

## ----------------------------------------------------------------------------------------------------

res_synthetic <- read_result(pattern = "synthetic", data_base)
res_kdd       <- read_result(pattern = "kdd", data_base)

## ----------------------------------------------------------------------------------------------------

special_table("synthetic", res_synthetic)
special_table("KDD", res_kdd)

## ----------------------------------------------------------------------------------------------------

cat ("\n\n\n--- BOOTSTRAP RESULTS --- \n\n\n")
datasets <- c("breast-cancer-wisconsin", "glass", "iris", "synthetic", "wine", "yeast", "kdd")
res_all  <- list()

for (ds in datasets) {
    res <- read_result(pattern = ds, path =  data_base_bs)
    tmp <- res$raw
    rownames(tmp) <- paste0(ds, "_", rownames(tmp))
    res_all[[ds]] <- tmp
    special_table(ds, res)
}

res_all_mat <- do.call(rbind, res_all)
