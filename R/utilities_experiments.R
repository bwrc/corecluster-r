#' Run a core clustering experiment on a dataset.
#'
#' @param dataset A dataset: a list with (at least) the fields
#' \code{data} (the data as a matrix or data frame, depending on the
#' clustering function) and \code{classes} (the true, known classes of
#' the dataset).
#' @param clustering_func A clustering function to use, which given data returns cluster indices.
#' @param sampling_func A sampling function to use. Default is NULL. Only used when method is \code{distribution}.
#' @param alpha The confidenence level. Default 0.1
#' @param n_iter The number of iterations to use. The default is 1000.
#' @param method The method to use. String. Either \code{bootstrap} to
#' use bootstrapping or \code{distribution} to calculate the exact
#' co-occurrence probabilities.
#' @param save The full path to a file (rds-file) where to save the data.
#' @param seed Random seed. Default is NULL to not set the random seed.
#' @param using_trim Boolen. Does the clustering method use trimming?
#'
#' @return A list structure:
#' \describe{
#' \item{data}{The data}
#' \item{alpha}{The confidence level}
#' \item{iterations}{The number of iterations}
#' \item{cluster}{The initial clustering}
#' \item{cluster_core}{The core clustering}
#' \item{mat}{The co-occurrence probability matrix}
#' \item{amat}{The adjacency matrix}
#' \item{using_trim}{Whether or not the clustering method is using trimming}
#' \item{weak_cluster}{The index used to describe the weak clusters}
#' \item{classes_true}{The known true classes of the data items}
#' \item{clustering_function}{The used clustering function}
#' \item{time_start}{The timestamp when the experiment was started}
#' \item{time_stop}{The timestamp when the experiment was ready}
#' }
#' 
#' @export
make_experiment <- function(dataset,
                            clustering_func = NULL,
                            sampling_func   = NULL,
                            alpha           = 0.1,
                            n_iter          = 1000,
                            method          = "bootstrap",
                            save            = NULL,
                            seed            = 42,
                            using_trim      = FALSE) {

    t_start <- Sys.time()

    ## Perform core clustering
    res <- core_clustering(dataset$data, clustering_func = clustering_func, sampling_func = sampling_func, method = method, n_iter = n_iter, seed = seed, using_trim = using_trim)

    t_stop <- Sys.time()

    ## Change type of the data
    if (is.data.frame(res$data)) {
        ind <- which(names(res$data) == "class")
        res$data <- as.matrix(res$data[,-ind])
    }

    out <- c(res, list("classes_true" = dataset$classes,
                       "clustering_function" = clustering_func,
                       "time_start" = t_start,
                       "time_stop" = t_stop))

    if (! is.null(save))
        saveRDS(out, file = save)

    ## Return result
    out
}

