## ------------------------------------------------------------------------------
## The core clustering algorithm
## ------------------------------------------------------------------------------

#' Core clustering.
#'
#' @param data The data. Either a matrix or a data frame, depending on the requirements of the clustering function.
#' @param clustering_func A clustering function to use, which given data returns cluster indices.
#' @param sampling_func A sampling function to use. Default is NULL. Only used when method is \code{distribution}.
#' @param method The method to use. String. Either \code{bootstrap} to
#' use bootstrapping or \code{distribution} to calculate the exact
#' co-occurrence probabilities.
#' @param alpha The confidenence level. Default 0.1
#' @param n_iter The number of iterations to use. The default is 1000.
#' @param weak_cluster The idnex to use for weak points. Default is -1.
#' @param using_trim Boolean. Does the clustering method use trimming?
#' @param seed Random seed. Default is NULL to not set the random seed.
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
#' }
#'
#' @export
#' @useDynLib corecluster
#' @importFrom Rcpp sourceCpp
core_clustering <- function(data,
                            clustering_func = NULL,
                            sampling_func   = NULL,
                            method          = "bootstrap",
                            alpha           = 0.1,
                            n_iter          = 1000,
                            weak_cluster    = -1,
                            using_trim      = FALSE,
                            seed            = NULL) {

    ## Set the random seed
    if (! is.null(seed))
        set.seed(seed)

    ## Check which method is to be used
    if (! method %in% c("bootstrap", "distribution"))
        stop("Unknown method. Cannot continue")

    ## Check if clustering function is given
    if (is.null(clustering_func))
        stop("No clustering function provided. Cannot continue")

    ## Get initial clustering
    clusters_initial <- clustering_func(data)

    ## Check which method that is to be used
    if (method == "bootstrap") {
        pmat <- matrix(1 / nrow(data), nrow = nrow(data), ncol = nrow(data))
        pmat <- co_occurrence_prob_all(data = data, cluster_function = clustering_func, iter = n_iter)
    }


    if (method == "distribution") {
        if (is.null(sampling_func))
            stop("No sampling function provided. Cannot continue")

        pmat <- matrix(0, nrow = nrow(data), ncol = nrow(data))

        NN   <- nrow(data)
        k    <- 0
        kmax <- (NN*NN - N) / 2

        ## Initialize a progress bar
        prog_bar <- txtProgressBar(min = 0, max = kmax, initial = 0, style = 3)

        for (i in seq.int(nrow(data) - 1)) {
            for (j in seq.int(i + 1, nrow(data))) {
                setTxtProgressBar(prog_bar, k)
                k <- (k + 1)
                pmat[i, j] <- co_occurrence_prob_single_parallel(data[i,], data[j,], data_function = sampling_func, cluster_function = clustering_func, iter = n_iter)
            }
        }

        close(prog_bar)

    }


    ## Discretize to get adjacency matrix
    amat          <- 1 * (pmat >= (1 - alpha))
    g             <- graph.adjacency(adjmatrix = amat, mode = "upper", diag = TRUE)
    ## g_plottable   <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)

    ## Determine largest maximal cliques inside each cluster
    tmp           <- find_core_clusters(g, clusters_initial)

    ## If we are using a clustering method that does trimming (e.g. tkmeans), we must remove the
    ## first cluster from the core clusters, corresponding to cluster index zero, i.e., the trimmed
    ## points
    if (using_trim)
        tmp <- tmp[-1]

    clusters_core <- clusters_from_cliques(tmp, nrow(data), weak_cluster = weak_cluster)

    ## Return the result
    list("data"         = data,
         "alpha"        = alpha,
         "iterations"   = n_iter,
         "cluster"      = clusters_initial,
         "cluster_core" = clusters_core,
         "pmat"         = pmat,
         "amat"         = amat,
         "using_trim"   = using_trim,
         "weak_cluster" = weak_cluster)
}


## ------------------------------------------------------------------------------
## Functions for calculating the co-occurrence probabilities
## ------------------------------------------------------------------------------

#' Estimate all co-occurence probabilities Pij using bootstrap and within-bag-samples
#'
#' @param data A matrix with data points.
#' @param cluster_function A clustering function that returns a set of cluster labels.
#' @param iter The number of iterations. Optional. Default is 1000.
#'
#' @return A square matrix of size (nrow(data) x nrow(data))
#' containing the co-occurence probabilities of the items in the data
#' matrix.
#'
#' @export
co_occurrence_prob_all <- function(data, cluster_function, iter = 1000) {
    n     <- nrow(data)
    same  <- matrix((1 / n), nrow = n, ncol = n)
    count <- matrix(1, nrow = n, ncol = n)

    ## Initialize a progress bar
    prog_bar <- txtProgressBar(min = 0, max = iter, initial = 0, style = 3)

    for (k in seq.int(iter)) {
        setTxtProgressBar(prog_bar, k)

        ## loop until a clustering result is achieved
        ## -----------------------
        bp <- 0
        cl <- NULL

        while ("success" != tryCatch({
            idx <- sample.int(n, size = n, replace = TRUE)
            cl  <- cluster_function(data[idx, ])
            "success"
        }, error = function(e) {
            print(e)
            "failure"
        }, finally = {
            bp <- bp + 1
        })) {
            if (bp >= 5) {
                print("break in co_occurrence_prob_all")
                break
            }
        }

        ## --------------------------------------------------
        ## call C++ code
        ## --------------------------------------------------
        if (! is.null(cl)) {
            res   <- co_occurrence_mat(idx, cl, same, count)
            same  <- res$same
            count <- res$count
        }
        ## --------------------------------------------------
    }
    close(prog_bar)

    diag(same) <- diag(count)
    same / count
}


#' Estimate co-occurence probability Pij of two data items x1 and x2. The function is
#' parallellized and utilizes (number-of-cores - 1) parallel computations.
#'
#' @param x1 Data item i.
#' @param x2 Data item j.
#' @param data_function A function, such that data_function() returns n-2 resampled data points.
#' @param cluster_function A clustering function that returns a set of cluster labels.
#' @param iter The number of iterations. Optional. Default is 1000.
#'
#' @return The co-occurence probability of x1 and x2; i.e., the
#' probability that data items x1 and x2 occur in the same cluster.
#'
#' @export
co_occurrence_prob_single_parallel <- function(x1, x2, data_function, cluster_function, iter = 1000) {
    res <- mclapply(1:iter, function(i) co_occurrence(x1, x2, data_function, cluster_function), mc.cores = (detectCores() - 1))
    res <- unlist(res)
    sum(res, na.rm = TRUE) / sum(! is.na(res))
}


#' A helper function for that determines if two data items x1 and x2 occur in one
#' clustering of the data
#'
#' @param x1 Data item i.
#' @param x2 Data item j.
#' @param data_function A function, such that data_function() returns n-2 resampled data points.
#' @param cluster_function A clustering function that returns a set of cluster labels.
#'
#' @return 1 if x1 and x2 are in the same clustering, 0 if not. NULL if the clustering failed.
#'
#' @export
co_occurrence <- function(x1, x2, data_function, cluster_function) {
    clu <- NULL
    bp  <- 0

    while ("success" != tryCatch({
        clu <- cluster_function(rbind(x1, x2, data_function()))
        "success"
    }, error = function(e) {
        print(e)
        "failure"
    }, finally = {
        bp <- bp + 1
    })) {
        if (bp >= 5) {
            print("break in co_occurrence_prob_all")
            break
        }
    }

    ## check if the clustering failed and returned a NULL
    if (is.null(clu))
        return(NA)

    ## otherwise
    if (clu[1] == clu[2]) { 1 }  else { 0 }

}

## ------------------------------------------------------------------------------
## Functions for finding the core clusters
## ------------------------------------------------------------------------------

#' Determine the core clusters
#'
#' @param g A graph
#' @param clusters An initial clustering
#'
#' @return The core clusters
#'
#' @export
find_core_clusters <- function(g, clusters) {
    out <- vector(mode = "list", length = length(unique(clusters)))
    j <- 1

    for (i in sort(unique(clusters))) {
        vtmp     <- sort(which(clusters == i))
        g2       <- induced.subgraph(g, v = vtmp)
        cl       <- find_largest_clique(g2)
        out[[j]] <- vtmp[cl]
        j        <- j + 1
    }

    out
}


#' Find the largest clique in the graph g
#'
#' @param g A graph
#'
#' @return The indices of the items in the largest clique in g.
#'
#' @export
find_largest_clique <- function(g) {
    mcl <- maximal.cliques(g)
    mcl[[which.max(sapply(mcl, length))]]
}


#' Get the clusters from the cliques
#'
#' @param cliques The cliques
#' @param N The number of data items.
#' @param weak_cluster The id of the weak points. The default is 0.
#'
#' @return The clusters
#'
#' @export
clusters_from_cliques <- function(cliques, N, weak_cluster = 0) {
    clusters <- rep(weak_cluster, N)
    for (i in seq.int(length(cliques)))
        clusters[unlist(cliques[[i]])] <- i
    clusters
}

## ------------------------------------------------------------------------------
