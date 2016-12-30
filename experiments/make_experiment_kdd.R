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
## The KDD dataset
## -----------------------------------------------------------------------------

N <- 200
if (FALSE) {
    ## Define the path to the kdd dataset
    kdd_data_path <- "kddcup.data_10_percent"

    ## Generate the data and save it as RDS files to save time
    dataset_kdd    <- get_data_kdd(kdd_data_path = kdd_data_path, N_samples = N, format = "matrix", mode = "sample")
    dataset_kdd_df <- get_data_kdd(kdd_data_path = kdd_data_path, N_samples = N, format = "df", mode = "sample")

    data_all_kdd    <- get_data_kdd(kdd_data_path = kdd_data_path, format = "matrix")$data
    data_all_kdd_df <- get_data_kdd(kdd_data_path = kdd_data_path, format = "df")$data

    ## select only those 5 columns that were chosen using varselrf
    colinds    <- which(colnames(dataset_kdd_df$data) %in% c("V5", "V23", "V24", "V30", "V36"))
    colinds_df <- which(colnames(dataset_kdd_df$data) %in% c("V5", "V23", "V24", "V30", "V36", "class"))

    dataset_kdd$data      <- dataset_kdd$data[, colinds]
    dataset_kdd_df$data   <- dataset_kdd_df$data[, colinds_df]
    data_all_kdd          <- data_all_kdd[, colinds]
    data_all_kdd_df       <- data_all_kdd_df[, colinds_df]
    
    ## save
    saveRDS(dataset_kdd, "data/dataset_kdd.rds", compress = "xz")
    saveRDS(dataset_kdd_df, "data/dataset_kdd_df.rds", compress = "xz")
    saveRDS(data_all_kdd, "data/data_all_kdd.rds", compress = "xz")
    saveRDS(data_all_kdd_df, "data/data_all_kdd_df.rds", compress = "xz")
}

dataset_kdd     <- readRDS("data/dataset_kdd.rds")
dataset_kdd_df  <- readRDS("data/dataset_kdd_df.rds")
data_all_kdd    <- readRDS("data/data_all_kdd.rds")
data_all_kdd_df <- readRDS("data/data_all_kdd_df.rds")

N_tot <- nrow(data_all_kdd)

## Define data distributions
sample_kdd_distribution    <- function() { data_all_kdd[sample(seq.int(N_tot), N, replace = TRUE),] }
sample_kdd_distribution_df <- function() { data_all_kdd_df[sample(seq.int(N_tot), N, replace = TRUE),] }

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
    tmp <- make_experiment(dataset         = dataset_kdd,
                           clustering_func = cf,
                           sampling_func   = sample_kdd_distribution,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename),
                           using_trim      = using_trim)
}

## -----------------------------------------------------------------------------

make_exp_and_save_df <- function(cf, basepath, savename, using_trim = FALSE) {
    tmp <- make_experiment(dataset         = dataset_kdd_df,
                           clustering_func = cf,
                           sampling_func   = sample_kdd_distribution_df,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "distribution",
                           save            = paste0(basepath, savename),
                           using_trim      = using_trim)
}

## -----------------------------------------------------------------------------
## Perform clustering
## -----------------------------------------------------------------------------
basepath <- "/tmp/cres/"
nodename <- Sys.info()[["nodename"]]

cat(" --- ", nodename, " --- \n")

## -----------------------------------------------------------------------------
 
    make_exp_and_save(clusterfunc_hclust, basepath, "res_kdd_hclust.rds")
    make_exp_and_save(clusterfunc_kmeanspp, basepath, "res_kdd_kmeanspp.rds")

    make_exp_and_save(clusterfunc_tclust, basepath, "res_kdd_tclust.rds", using_trim = TRUE)
    make_exp_and_save(clusterfunc_tkmeans, basepath, "res_kdd_tkmeans.rds", using_trim = TRUE)


    make_exp_and_save_df(clusterfunc_svm, basepath, "res_kdd_svm.rds")
    make_exp_and_save_df(clusterfunc_rf, basepath, "res_kdd_rf.rds")

## -----------------------------------------------------------------------------
## EOF
## -----------------------------------------------------------------------------
