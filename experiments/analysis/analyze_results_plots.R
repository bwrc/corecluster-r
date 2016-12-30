graphics.off()
rm(list = ls())

library(corecluster)
library(xtable)
library(clue)

## ----------------------------------------------------------------------------------------------------
## Define paths
## ----------------------------------------------------------------------------------------------------

base_path_data_bs     <- "results/bootstrap/"
base_path_data_true   <- "results/truedist/"
fig_path              <- "/tmp/figs/"

## ----------------------------------------------------------------------------------------------------

ds              <- "synthetic"
classifier_list <- c("kmeanspp", "hclust", "tclust", "tkmeans", "svm", "rf")
prefix          <- "resbs"

## resl_bs <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_bs, prefix = prefix, is_list = FALSE)

graphics.off()
## plot_result(resl_bs[[1]])
## plot_reslist(resl_bs, save = FALSE, format = "pdf", legend = TRUE)

## ----------------------------------------------------------------------------------------------------
## Plot and save figures
## ----------------------------------------------------------------------------------------------------
## Get the data for the motivating example
ds              <- "synthetic"
classifier_list <- c("kmeanspp")

prefix          <- "res"
resl_td         <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_true, prefix = prefix, is_list = FALSE)

prefix          <- "resbs"
resl_bs         <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_bs, prefix = prefix, is_list = FALSE)

## plot_result(resl_td[[1]], savename = paste0(fig_path, "fig_ex_core_clusters_td.pdf"), plot_format = "pdf")
plot_result(resl_bs[[1]], savename = paste0(fig_path, "fig_ex_core_clusters_bs.pdf"), plot_format = "pdf")

## ----------------------------------------------------------------------------------------------------
## Example figures
## ----------------------------------------------------------------------------------------------------
prefix            <- "resbs"
## ----------------------------------------------------------------------------------------------------
ds                <- "synthetic"
classifier_list   <- c("kmeanspp", "svm", "hclust", "tclust")
resl_ex_synthetic <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_bs, prefix = prefix, is_list = FALSE)
plot_and_save_reslist(resl_ex_synthetic, basepath = fig_path, plot_format = "pdf")
## ----------------------------------------------------------------------------------------------------
ds                <- "iris"
classifier_list   <- c("kmeanspp", "hclust", "tkmeans")
resl_ex_iris      <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_bs, prefix = prefix, is_list = FALSE)
plot_reslist(resl_ex_iris, legend = TRUE)
plot_and_save_reslist(resl_ex_iris, basepath = fig_path, plot_format = "pdf")
## ----------------------------------------------------------------------------------------------------
ds                <- "breast-cancer-wisconsin"
classifier_list   <- c("kmeanspp", "hclust", "tclust")
resl_ex_bcw       <- get_reslist(classifier_list, dataset = ds, base_path = base_path_data_bs, prefix = prefix, is_list = FALSE)
plot_reslist(resl_ex_bcw, legend = TRUE)
plot_and_save_reslist(resl_ex_bcw, basepath = fig_path, plot_format = "pdf")
## ----------------------------------------------------------------------------------------------------


