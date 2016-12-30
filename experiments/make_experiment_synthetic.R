## -----------------------------------------------------------------------------
## Load libraries
## -----------------------------------------------------------------------------
graphics.off()
rm(list = ls())

library(corecluster)
library(tclust)
library(e1071)
library(randomForest)
library(readMLData)
library(igraph)
library(parallel)

## -----------------------------------------------------------------------------
## The synthetic dataset
## -----------------------------------------------------------------------------
## The size of the dataset
N <- 150

dataset_synthetic    <- get_dataset_synthetic(N = N)
dataset_synthetic_df <- get_dataset_synthetic(N = N, type = "df")

## Define data distributions
sample_synthetic_distribution    <- function() { get_data_synthetic(N-2)$data }
sample_synthetic_distribution_df <- function() {  get_data_synthetic(N-2, type = "df")$data }

## -----------------------------------------------------------------------------
## Clustering functions
## -----------------------------------------------------------------------------

## Clustering functions:
n_cluster <- 3

clusterfunc_kmeanspp <- function(x) { cluster_kmeanspp(x, k = n_cluster) }
clusterfunc_hclust   <- function(x) { cluster_hclust(x, k = n_cluster)   }

clusterfunc_tclust  <- function(x) { cluster_tclust(x, k = n_cluster)  }
clusterfunc_tkmeans <- function(x) { cluster_tkmeans(x, k = n_cluster) }

## -----------------------------------------------------------------------------
## Define helper functions for the experiments
## -----------------------------------------------------------------------------
make_exp_and_save <- function(cf, basepath, savename, using_trim = FALSE) {
    tmp <- make_experiment(dataset         = dataset_synthetic,
                           clustering_func = cf,
                           sampling_func   = sample_synthetic_distribution,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename),
                           using_trim      = using_trim)
}

## -----------------------------------------------------------------------------

make_exp_and_save_df <- function(cf, basepath, savename, using_trim = FALSE) {
    tmp <- make_experiment(dataset         = dataset_synthetic_df,
                           clustering_func = cf,
                           sampling_func   = sample_synthetic_distribution_df,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename),
                           using_trim      = using_trim)
}

## -----------------------------------------------------------------------------
## Perform clustering
## -----------------------------------------------------------------------------
basepath <- "/tmp/res/"
nodename <- Sys.info()[["nodename"]]

cat(" --- ", nodename, " --- \n")

## -----------------------------------------------------------------------------

make_exp_and_save(clusterfunc_hclust, basepath, "res_synthetic_hclust.rds")
make_exp_and_save(clusterfunc_kmeanspp, basepath, "res_synthetic_kmeanspp.rds")

make_exp_and_save(clusterfunc_tclust, basepath, "res_synthetic_tclust.rds", using_trim = TRUE)
make_exp_and_save(clusterfunc_tkmeans, basepath, "res_synthetic_tkmeans.rds", using_trim = TRUE)

make_exp_and_save_df(clusterfunc_rf, basepath, "res_synthetic_rf.rds")
make_exp_and_save_df(clusterfunc_svm, basepath, "res_synthetic_svm.rds")

## -----------------------------------------------------------------------------
## EOF
## -----------------------------------------------------------------------------
