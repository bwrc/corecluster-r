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
## Set number of items per cluster
N <- 150

dataset_synthetic    <- get_dataset_synthetic(N = N)
dataset_synthetic_df <- get_dataset_synthetic(N = N)

## -----------------------------------------------------------------------------
## Clustering functions
## -----------------------------------------------------------------------------

## Clustering functions:
n_cluster <- 3

clusterfunc_kmeanspp <- function(x) { cluster_kmeanspp(x, k = n_cluster) }
clusterfunc_hclust   <- function(x) { cluster_hclust(x, k = n_cluster)   }
clusterfunc_tkmeans  <- function(x) { tclust(x, k = n_cluster, restr = "eigen", equal.weights = TRUE, restr.fact = 1)$cluster }
clusterfunc_tclust   <- function(x) { tclust(x, k = n_cluster, restr = "eigen", equal.weights = FALSE, restr.fact = 50)$cluster }

## -----------------------------------------------------------------------------
## Define helper functions for the experiments
## -----------------------------------------------------------------------------
make_exp_and_save <- function(cf, basepath, savename) {
    tmp <- make_experiment(dataset         = dataset_synthetic,
                           clustering_func = cf,
                           sampling_func   = sample_synthetic_distribution,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename))

}

## -----------------------------------------------------------------------------

make_exp_and_save_df <- function(cf, basepath, savename) {
    tmp <- make_experiment(dataset         = dataset_synthetic_df,
                           clustering_func = cf,
                           sampling_func   = sample_synthetic_distribution_df,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename))

}

## -----------------------------------------------------------------------------

basepath <- "/tmp/res/"

## make_exp_and_save(clusterfunc_hclust, basepath, "res_synth_hclust.rds")
## make_exp_and_save(clusterfunc_kmeanspp, basepath, "res_synth_kmeanspp.rds")

## make_exp_and_save(clusterfunc_tclust, basepath, "res_synth_tclust.rds")
## make_exp_and_save(clusterfunc_tkmeans, basepath, "res_synth_tkmeans.rds")

## make_exp_and_save_df(clusterfunc_svm, basepath, "res_synth_svm.rds")
## make_exp_and_save_df(clusterfunc_rf, basepath, "res_synth_rf.rds")

## -----------------------------------------------------------------------------
## EOF
## -----------------------------------------------------------------------------
