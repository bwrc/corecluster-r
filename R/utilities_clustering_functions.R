## ------------------------------------------------------------------------------
## Clustering functions
## ------------------------------------------------------------------------------

#' K-means++ algorithm.
#'
#' @param x Data matrix with rows corresponding to data items.
#' @param k Number of clusters.
#' @param iter The maximum number of iterations. Optional. Default is 1000.
#' @param algorithm Clustering algorithm to use. Optional. Default is
#' \code{Lloyd}. See documentation for \code{kmeans} for details.
#'
#' @return See documentation for kmeans for details.
#'
#' @references Arthur & Vassilivitskii (2007) k-means++: the
#' advantages of careful seeding. In Proc SODA '07, 1027-1035.
#'
#' @export
kmeanspp <- function(x, k, iter.max = 1000, algorithm = "Lloyd") {
    kmeans(x,
           centers   = kmeansppcenters(x,k),
           iter.max  = iter.max,
           algorithm = algorithm)
}


#' Run the k-means++ algorithm n times and return the solution
#' with the smallest loss.
#'
#' @param x Data matrix with rows corresponding to data items.
#' @param k Number of clusters.
#' @param n Number of repeats to run. Optional. Default is 10.
#' @param iter The maximum number of iterations. Optional. Default is 1000.
#' @param algorithm Clustering algorithm to use. Optional. Default is
#' \code{Lloyd}. See documentation for \code{kmeans} for details.
#'
#' @return See documentation for kmeans for details.
#'
#' @references Arthur & Vassilivitskii (2007) k-means++: the
#' advantages of careful seeding. In Proc SODA '07, 1027-1035.
#'
#' @export
kmeansppn <- function(x, k, n = 10, iter.max = 1000, algorithm = "Lloyd") {
    best <- NULL

    for(i in seq.int(n)) {
        res <- kmeanspp(x, k, iter.max = iter.max, algorithm = algorithm)

        if (is.null(best) || res$tot.withinss < best$tot.withinss) {
            best <- res
        }
    }

    best
}


#' Return cluster indices using kmeans++.
#'
#' @param x Data matrix
#' @param n Number of clusters
#'
#' @return Vector with cluster labels.
#'
#' @export
cluster_kmeanspp <- function(x, k, iteration = NULL) {
    kmeansppn(x, k = k)$cluster
}


#' Find initial kmeans++ centroids.
#'
#' @param x Data matrix
#' @param k Number of clusters.
#'
#' @return A matrix containing the cluster centroids.
#'
#' @references Arthur & Vassilivitskii (2007) k-means++: the
#' advantages of careful seeding. In Proc SODA '07, 1027-1035.
#'
#' @export
kmeansppcenters <- function(x, k) {
    x       <- as.matrix(x)
    n       <- nrow(x)
    centers <- matrix(NA,k, ncol(x))
    p       <- rep(1 / n, n)
    d       <- rep(NA, n)

    for (i in seq.int(k)) {
        centers[i,] <- x[sample.int(n, size = 1, prob = p),]
        dd          <- rowSums((x - (rep(1, n) %o% centers[i, ]))^2)
        d           <- if (i > 1) pmin(d, dd) else dd
        if (max(d) > 0) { p <- d / sum(d) }
    }

    centers
}


#' Return cluster indices using hierarchical clustering.
#'
#' @param x Data matrix
#' @param n Number of clusters.
#'
#' @return Vector with cluster labels.
#'
#' @export
cluster_hclust <- function(x, k) {
    cutree(hclust(dist(x)), k = k)
}


#' Cluster a dataset x into k clusters using tclust
#'
#' @param x The data
#' @param k The number of clusters
#' 
#' @return The cluster indices of the data.
#'
#' @export
cluster_tclust <- function(x, k) {
    bp <- 0
    cl <- NULL

    while ("success" != tryCatch({
        cl <- tclust(x, k = k, restr = "eigen", equal.weights = FALSE, restr.fact = 50)
        "success"
    }, error = function(e) {
        print(e)
        "failure"
    }, finally = {
        ## cat(paste("iter : ", bp, "\n", sep = ''))
        bp <- bp + 1
    })) {
        if (bp >= 5) {
            print("break in cluster_tclust")
            break
        }
    }

    cl$cluster
}


#' Cluster a dataset x into k clusters using tkmeans
#'
#' @param x The data
#' @param k The number of clusters
#' 
#' @return The cluster indices of the data.
#'
#' @export
cluster_tkmeans <- function(x, k) {
    bp <- 0
    cl <- NULL

    while ("success" != tryCatch({
        cl <- tclust(x, k = k, restr = "eigen", equal.weights = TRUE, restr.fact = 1)
        "success"
    }, error = function(e) {
        print(e)
        "failure"
    }, finally = {
        bp <- bp + 1
    })) {
        if (bp >= 5) {
            print("break in cluster_tkmeans")
            break
        }
    }

    cl$cluster
}


#' Cluster (classify) a dataset x into its constituent clusters (classes)
#' using an SVM.
#'
#' @param x The data
#' 
#' @return The cluster indices of the data.
#'
#' @export
clusterfunc_svm <- function(x) {
    cl <- svm(class~., data = x)
    as.numeric(predict(cl, newdata = x))
}


#' Cluster (classify) a dataset x into its constituent clusters (classes)
#' using an SVM.
#'
#' @param x The data
#' 
#' @return The cluster indices of the data.
#'
#' @export
clusterfunc_rf <- function(x) {
    cl <- randomForest(class~., data = x)
    as.numeric(predict(cl, newdata = x))
}


## ------------------------------------------------------------------------------
