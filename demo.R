## Load library
library(corecluster)

## Generate some synthetic data
N         <- 150
n_cluster <- 3

set.seed(42)
dataset_synthetic <- get_dataset_synthetic(N = N)

## Define clustering functions
## A clustering function is defined such that it takes
## one argument (the data) and returns a vector of
## cluster IDs.
##
## As an example, to use the k-means algorithm we define
clusterfunc_kmeans   <- function(x) { kmeans(x, centers = n_cluster)$cluster }

## Perform clustering
res <- make_experiment(dataset             = dataset_synthetic,
                           clustering_func = clusterfunc_kmeans,
                           sampling_func   = NULL,
                           alpha           = 0.1,
                           n_iter          = 1000,
                           method          = "bootstrap",
                           save            = NULL)

## Visualise results
plot_result(res, savename = "/tmp/data_synthetic.png", plot_format = "png")
