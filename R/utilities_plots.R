## ------------------------------------------------------------------------------
## Functions for plotting results from core clustering>
## ------------------------------------------------------------------------------

#' Plot the data
#'
#' @param data The data
#' @param clusters The clusters
#' @param clusters_core The core clusters
#' @param main The title of the plot
#'
#' @return Nothing
#'
#' @export
plot_data <- function(data, clusters = NULL, clusters_core = NULL, main = NULL) {
    if (is.null(clusters))
        clusters <- rep(1, nrow(data))

    if (! is.null(clusters_core)) {
        clusters[clusters_core < 0] <- -clusters[clusters_core < 0]
    }


    ## Create fill vector
    fill_cluster <- c("red", "green", "blue", "deeppink", "darkorange", "aquamarine")
    fill_weak    <- "grey"

    n_cluster  <- length(unique(abs(clusters)))

    if (n_cluster < 6)
        fill_weak <- "white"
    else
        fill_weak <- "grey"

    ## Has a clustering method that does trimming been used?
    if (0 %in% unique(clusters)) {
        ind_pruned <- which(clusters == 0)
        clusters[ind_pruned] <- n_cluster
        symbol_list <- get_symbol_vector(abs(clusters), c(21, 22, 24, 23, 25))
        symbol_list[ind_pruned] <- 8
    } else {
        symbol_list <- get_symbol_vector(abs(clusters), c(21, 22, 24, 23, 25))
    }

    tmp               <- clusters
    tmp[clusters < 0] <- (1 + n_cluster)
    fill_vector       <- c(fill_cluster[1:n_cluster], fill_weak)[tmp]

    ## Plot the clusters
    if (n_cluster < 6)
        plot(data[, 1], data[, 2], col = "black", bg = fill_vector, pch = symbol_list, asp = 1, main = main, cex = 2, xaxt = "n", yaxt = "n", ann = FALSE)
    else
        plot(data[, 1], data[, 2], col = fill_vector, pch = symbol_list, asp = 1, main = main, cex = 2, xaxt = "n", yaxt = "n", ann = FALSE)
}


#' Plot (and optionally save) a result structure
#'
#' @param res A result structure from a core clustering experiment
#' @param savename The filename where to save the figure
#' @param main The title of the plot
#' @param plot_format The format of the plot. Default is \code{pdf}.
#'
#' @return Nothing
#'
#' @export
plot_result <- function(res, savename = NULL, main = NULL, plot_format = "pdf") {

    print(plot_format)
    
    ## Make sure the data is numeric
    if (! is.numeric(res$data)) {
        print("ERROR")
        class(res$data) <- "numeric"
    }

    ## Plot the dataset
    if (ncol(res$data) > 2) {
        set.seed(42)
        ## res$data <- princomp(res$data)$scores[,1:2]
        res$data <- prcomp(res$data)$x[,1:2]
    }

    ## ------------------------------
    if (! is.null(savename)) {
        sn_tmp <- paste(plot_format, "(\"", savename, "\")", sep = "")
        eval(parse(text = sn_tmp))
        #if (plot_format == "pdf")
        #    pdf(savename)
    }
    ## ------------------------------

    plot_data(res$data, clusters = res$cluster, clusters_core = res$cluster_core, main = main)

    ## ------------------------------
    if (! is.null(savename))
        dev.off()
    ## ------------------------------

}


#' Plot (and optionally save) a list of result structures, multiple plots in one figure.
#'
#' @param resl A list of result structures from core clustering experiments
#' @param save The filename where to save the figure
#' @param plot_format The format of the plot. Default is \code{pdf}.
#' @param legend Add the names of the used clustering functions to the plot
#'
#' @return Nothing
#'
#' @export
plot_reslist <- function(resl, save = FALSE, plot_format = "pdf", legend = FALSE) {
    np <- length(resl)

    if (legend)
        par(mfrow = c(2, np))
    else
        par(mfrow = c(1, np))

    for (i in names(resl)) {
        res <- resl[[i]]
        cn <- res$properties$classifier_name
        dn <- res$properties$dataset_name
        savename <- paste("figures_", plot_format, "/", dn, "/", "fig_", i, "_", dn, ".", plot_format, sep = "")
        if (save) {
            plot_result(resl[[i]], savename = savename, main = i, plot_format = plot_format)
        } else {
            plot_result(resl[[i]], main = i)
        }
    }
    if (legend) {
        for (i in names(resl)) {
            plot.new()
            text(0.5, 0.5, labels = i, cex = 3)
        }
    }

}

#' Plot (and optionally save) a list of result structures, one plot per figure.
#'
#' @param resl A list of result structures from core clustering experiments
#' @param basepath The base path where to save the results.
#' @param plot_format The format of the plot. Default is \code{pdf}.
#'
#' @return Nothing
#'
#' @export
plot_and_save_reslist <- function(resl, basepath = NULL, plot_format = "pdf") {
    if (is.null(basepath))
        stop("No base path given.")

    for (i in resl) {
        savename <- paste0(basepath, "fig_", i$properties$classifier_name, "_", i$properties$dataset_name, ".", plot_format)
        print(savename)
        plot_result(i, savename = savename, plot_format = plot_format)
    }
}

## ------------------------------------------------------------------------------
## Helper functions needed for the plotting
## ------------------------------------------------------------------------------


#' Get a list of symbols given the clusters.
#'
#' @param clusters The clusters
#' @param symbol_list A list of plot symbols.
#'
#' @return A list of symbols
#'
#' @export
get_symbol_vector <- function(clusters, symbol_list) {
    symbols <- clusters
    cl      <- sort(unique(clusters))

    if (length(cl) < 6) {
        for (i in seq.int(length(cl)))
            symbols[symbols == cl[i]] <- symbol_list[i]
    } else {
        symbols <- LETTERS[clusters]
    }

    symbols

}


#' Match two clusterings using the Hungarian algorithm.
#'
#' @param n Number of items. Cluster indices shluld be in the range [1,n].
#' @param cluster1 First vector of cluster indices.
#' @param cluster2 Second vector of cluster indices.
#'
#' @return A vector of cluster indices for cluster1, so that they are best
#' matched to cluster2.
#'
#' @export
match_clusters <- function(n, cluster1, cluster2) {
    aux <- matrix(0, nrow = n, ncol = n)

    for (i in seq.int(length(cluster1))) {
        aux[cluster1[i],cluster2[i]] <- aux[cluster1[i], cluster2[i]] + 1
    }

    c(solve_LSAP(aux, maximum = TRUE))[cluster1]
}

## ------------------------------------------------------------------------------
