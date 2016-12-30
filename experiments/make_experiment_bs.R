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
## Get the UCI datasets
## -----------------------------------------------------------------------------
if (FALSE) {
    library(readMLData)

    kdd_data_path   <- "data/raw/kddcup.data_10_percent"

    pathData        <- "data/raw/datasets/UCI_ML_DataFolders"
    pathDescription <- "data/raw/datasets/UCI_ML_DataDescription"
    dsList          <- prepareDSList(pathData, pathDescription)

    datasets        <- c("iris", "wine", "glass", "breast-cancer-wisconsin", "yeast")

    ## Read datasets as
    dataset_list    <- sapply(datasets, function(i) read_uci_dataset(dsList, i))
    dataset_list_df <- sapply(datasets, function(i) read_uci_dataset(dsList, i, as_df = TRUE, remove_class = FALSE))

    ## -----------------------------------
    ## Add the synthetic and KDD datasets
    ##------------------------------------
    dataset_list$synthetic    <- get_dataset_synthetic(N = 150)
    dataset_list_df$synthetic <- get_dataset_synthetic(N = 150, type = "df")

    dataset_list$kdd    <- readRDS("data/dataset_kdd.rds")
    dataset_list_df$kdd <- readRDS("data/dataset_kdd_df.rds")

    saveRDS(dataset_list, "data/dataset_list_uci.rds")
    saveRDS(dataset_list_df, "data/dataset_list_uci_df.rds")
}

dataset_list    <- readRDS("data/dataset_list_uci.rds")
dataset_list_df <- readRDS("data/dataset_list_uci_df.rds")

## -----------------------------------------------------------------------------
## Define helper functions for the experiments
## -----------------------------------------------------------------------------
generate_cf <- function(alg_name, n_cluster) {
    if (alg_name == "kmeanspp")
        cf <- function(x) { cluster_kmeanspp(x, k = n_cluster) }
    if (alg_name == "hclust")
        cf   <- function(x) { cluster_hclust(x, k = n_cluster)   }
    if (alg_name == "tkmeans")
        cf  <- function(x) { cluster_tkmeans(x, k = n_cluster) }
    if (alg_name == "tclust")
        cf  <- function(x) { cluster_tclust(x, k = n_cluster)  }
    if (alg_name == "svm")
        cf <- clusterfunc_svm
    if (alg_name == "rf")
        cf <- clusterfunc_rf
    cf
}

make_exp_and_save <- function(dataset, alg_name, basepath, using_trim = FALSE) {
    ## Make clustering functions
    n_cluster <- length(unique(dataset$classes))
    cf        <- generate_cf(alg_name, n_cluster)

    savename <- paste0(basepath, "resbs_", dataset$name, "_", alg_name, ".rds")

    cat(savename, "\n")

    tmp <- make_experiment(dataset         = dataset,
                           clustering_func = cf,
                           sampling_func   = NULL,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "bootstrap",
                           save            = savename,
                           using_trim      = using_trim)

}

## -----------------------------------------------------------------------------
run_experiment_set <- function(ca_list, dataset_list, basepath) {
    i <- 0
    for (ds in names(dataset_list)) {
        cat(i <- i + 1, " / ", length(dataset_list), "\n")
        ## Clustering functions
        for (ca in ca_list) {
            if (ca %in% c("tclust", "tkmeans"))
                using_trim <- TRUE
            else
                using_trim <- FALSE

            make_exp_and_save(dataset_list[[ds]], ca, basepath, using_trim = using_trim)
        }
    }
}

## -----------------------------------------------------------------------------
## Perform clustering
## -----------------------------------------------------------------------------
ca_list    <- c("kmeanspp", "hclust", "tkmeans", "tclust")
ca_list_la <- c("svm", "rf")
basepath   <- "/tmp/cres/"

nodename   <- Sys.info()[["nodename"]]
cat(" --- ", nodename, " --- \n")

## -----------------------------------------------------------------------------

run_experiment_set(c("kmeanspp", "hclust"), dataset_list, basepath)
run_experiment_set(c("tkmeans", "tclust"), dataset_list, basepath)
run_experiment_set(c("rf", "svm"), dataset_list_df, basepath)

## -----------------------------------------------------------------------------
## EOF
## -----------------------------------------------------------------------------
