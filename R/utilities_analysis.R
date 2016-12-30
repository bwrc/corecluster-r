## ------------------------------------------------------------------------------
## Utilities used for the analysis of core clustering results
## ------------------------------------------------------------------------------

#' Match clusterings given a template using the Hungarian algorithm
#'
#' @param res_template A result structure to be used as a template
#' @param res_other Another result structure, which is to be modified
#'
#' @return res_other with the clusters matched to res_template
#'
#' @export
fix_clusters <- function(res_template, res_other) {
    cl_t <- as.numeric(res_template$cluster)
    cl_o <- as.numeric(res_other$cluster)

    if (0 %in% cl_o) {
                               
        ind_trim <- which(cl_o == 0)
        ind      <- seq.int(length(cl_o))[-ind_trim]
        cl_t     <- cl_t[-ind_trim]
        cl_o     <- cl_o[-ind_trim]

        n_classes <- max(length(unique(cl_t)), length(unique(cl_o)))
        
        ## res_other$cluster[ind] <- match_clusters(length(unique(cl_o)), cl_o, cl_t)
        res_other$cluster[ind] <- match_clusters(n_classes, cl_o, cl_t)
    } else {
        n_classes <- max(length(unique(cl_t)), length(unique(cl_o)))
        
        ## res_other$cluster <- match_clusters(length(unique(cl_o)), cl_o, cl_t)
        res_other$cluster <- match_clusters(n_classes, cl_o, cl_t)
    }

    res_other
}


#' Read results from experiments and return them as a list
#'
#' @param classifier_list A list of classifier names
#' @param dataset The name of the dataset to read
#' @param base_path The base path to the results
#' @param is_list Is the result structure a list
#' @param prefix A prefix used in the filenames of the results
#'
#' @return A list of result structures from core clustering
#'
#' @export
get_reslist <- function(classifier_list, dataset, base_path = "results", is_list = TRUE, prefix = "res") {
    reslist <- sapply(classifier_list, function(i) read_res(i, dataset, base_path = base_path, is_list = is_list, prefix = prefix), simplify = FALSE, USE.NAMES = TRUE)
    ind_null <- which(sapply(reslist, is.null))

    if (length(ind_null) > 0)
        reslist <- reslist[-ind_null]

    if (length(reslist) > 1) {
        tmp <- sapply(reslist[-1], function(i) fix_clusters(reslist[[1]], i), simplify = FALSE, USE.NAMES = TRUE)
        reslist <- c(reslist[1], tmp)
    }

    reslist
}


#' Read a result structure from core clustering
#'
#' @param classifier A classifier names
#' @param dataset The name of the dataset to read
#' @param base_path The base path to the results
#' @param is_list Is the result structure a list
#' @param prefix A prefix used in the filenames of the results
#'
#' @return A result structure from core clustering
#'
#' @export
read_res <- function(classifier, dataset, base_path, is_list = TRUE, prefix = "res") {
    fname <- paste0(base_path, "/", prefix, "_", dataset, "_", classifier, ".rds")

    if (! file.exists(fname))
        return(NULL)

    res <- readRDS(fname)

    properties                 <- list()
    properties$classifier_name <- classifier
    properties$dataset_name    <- dataset

    out <- NULL

    if (is_list) {
        if (dataset %in% names(res) ) {
            out            <- res[[dataset]]
            out$properties <- properties
        }
    } else {
        out <- res
        out$properties <- properties

    }

    out
}

## ------------------------------------------------------------------------------
## Utilities for calculating running times
## ------------------------------------------------------------------------------

#' Get the running time of a core clustering experiment
#'
#' @param res A result structure from a core clustering experiment.
#'
#' @return The running time for the experiment
#'
#' @export
get_running_time <- function(res) {
    difftime(res$time_stop, res$time_start, units = "secs")
}


## ------------------------------------------------------------------------------
## Utilities for determining the agreement between two core clustering results
## ------------------------------------------------------------------------------

#' Get the running time of a core clustering experiment
#'
#' @param dir_1 A directory with results
#' @param dir_2 A directory with results to compare to dir_1
#' @param ds_name The name of the dataset
#' @param alg_name The name of the clustering algorithm
#'
#' @return A list with confusion matrices for the agreement between
#' the results
#'
#' @export
get_agreement <- function(dir_1, dir_2, ds_name, alg_name) {
    pattern = paste0(".", ds_name, ".*", alg_name)
    fname_1 <- list.files(dir_1, pattern = pattern, full.names = TRUE)
    fname_2 <- list.files(dir_2, pattern = pattern, full.names = TRUE)

    print(fname_1)
    print(fname_2)

    if (length(fname_1) == 0 | length(fname_2) == 0)
        stop("Files not found.")

    res_1 <- readRDS(fname_1)
    res_2 <- readRDS(fname_2)

    clusters <- match_clusters_core(res_1, res_2)
    res      <- get_confusion_matrix(clusters$cl_1, clusters$cl_2)

    attr(res, "ds_name") <- ds_name
    attr(res, "alg_name") <- alg_name

    res
}


#' Make a confusion matrix of the core clustering agreement
#' for clusters cl_1 and cl_2
#'
#' @param cl_1 A vector with cluster indices
#' @param cl_2 A vector with cluster indices
#'
#' @return A confusion matrix
#'
#' @export
get_confusion_matrix <- function(cl_1, cl_2) {
    ## cores
    ic_1 <- which(cl_1 > 0)
    ic_2 <- which(cl_2 > 0)
    ## weaks
    iw_1 <- which(cl_1 < 0)
    iw_2 <- which(cl_2 < 0)

    out <- matrix(NA, nrow = 2, ncol = 2)
    out[1, 1] <- length(intersect(ic_1, ic_2))
    out[1, 2] <- length(intersect(ic_1, iw_2))
    out[2, 1] <- length(intersect(iw_1, ic_2))
    out[2, 2] <- length(intersect(iw_1, iw_2))
    ## out <- out / sum(out)
    colnames(out) <- c("core", "weak")
    rownames(out) <- c("core", "weak")

    out
}


#' Match core clusterings using the Hungarian algorithm.
#'
#' @param res_1 A result structure
#' @param res_2 A result structure
#'
#' @return A list with the core clusters of res_1 and res_2 matched.
#'
#' @export
match_clusters_core <- function(res_1, res_2) {
    cl_1 <- res_1$cluster_core
    cl_2 <- res_2$cluster_core

    if (res_1$using_trim) {
        ind_trimmed <- which(res_1$cluster == 0)
        if (length(ind_trimmed) > 0) {
            cl_1 <- cl_1[-ind_trimmed]
            cl_2 <- cl_2[-ind_trimmed]
        }
    }

    cl_2_tmp <- cl_2
    ind_neg  <- which(cl_2_tmp < 0)
    cl_2     <- match_clusters(n = length(cl_1), abs(cl_2_tmp), abs(cl_1))
    cl_2[ind_neg] <- (-1) * cl_2[ind_neg]
    list("cl_1" = cl_1, "cl_2" = cl_2)
}


#' Helper function to generate a string for a results table
#'
#' @param mat A matrix
#'
#' @return A string
#'
#' @keywords internal
make_cfmat_string <- function(mat) {
    paste0("\\cfmat", paste(sapply(as.vector(t(mat)), function(i) paste0("{", i, "}")), collapse = ""))
}


#' Fix the names of clustering algorithms
#'
#' @param x The name of a clustering algorithm as a string
#'
#' @return A string
#'
#' @keywords internal
fix_alg_name <- function(x) {
    out <- x
    if (x == "svm")
        out <- "SVM"
    if (x == "rf")
        out <- "random forest"
    if (x == "kmeanspp")
        out <- "k-means++"
    out

}


#' Given a list of algorithm names and two lists with results,
#' create a latex table.
#'
#' @param alg_list A list with the names of clustering algorithms used in the experiments
#' @param list_left The contents for the left side of the list
#' @param list_right The contents for the rigth side of the list
#'
#' @return Outputs a latex table
#'
#' @export
confusion_matrix_to_latex <- function(alg_list, list_left, list_right) {
    ## The LaTeX definition of cfmat
    ##
    ## \newcommand{\cfmat}[4]{
    ## $ \begin{array}{c|c}
    ## #1 & #2 \\
    ## \hline
    ## #3 & #4
    ## \end{array} $
    ## }

    for (i in alg_list) {
        cf_1 <- list_left[[i]]
        cf_2 <- list_right[[i]]

        alg_name <- fix_alg_name(i)

        res <- paste0(alg_name, " & ", make_cfmat_string(cf_1), " & ", make_cfmat_string(cf_2),  "\\\\")

        cat(res, "\n")
        cat("\\midrule", "\n")
    }

}

## ------------------------------------------------------------------------------
## Functions used to generate results tables
## ------------------------------------------------------------------------------

#' Create a matrix of results given a result structure.
#'
#' @param res A results structure
#' @param alg_name The name of the clustering algorithm
#'
#' @return A matrix
#'
#' @export
res_to_mat <- function(res, alg_name) {
    res$classes_true <- as.numeric(res$classes_true)
    tmp              <- calculate_stats_light(res)
    mat              <- matrix(tmp, nrow = 1, ncol = 3)
    colnames(mat)    <- names(tmp)
    rownames(mat)    <- alg_name
    mat
}


#' Read results from a filename and return the results as a matrix
#'
#' @param f Full path to the file containing the results (an rds file).
#'
#' @return The results as a matrix.
#'
#' @export
read_result_set <- function(f) {
    res      <- readRDS(f)
    alg_name <- rev(strsplit(strsplit(rev(unlist(strsplit(f, "/")))[[1]], "\\.")[[1]][[1]], "_")[[1]])[1]
    res_to_mat(res, alg_name)
}


#' Read results from core clustering experiments stored in rds files.
#'
#' @param pattern A pattern to match
#' @param path The path where the files are found
#'
#' @return A list with the fields \code{raw} containing the results as a matrix and \code{latex} containing the results as a latex table.
#'
#' @export
read_result <- function(pattern, path) {
    ## print(path)
    
    ## get all matching files
    flist <- list.files(path = path, pattern = pattern, full.names = TRUE)

    ## print(flist)
    
    res <- lapply(flist, read_result_set)
    tmp <- do.call(rbind, res)
    ind <- order(rownames(tmp))
    tmp <- tmp[ind, ]

    colnames(tmp)        <- c("po", "pc", "w")

    resmat_latex         <- xtable(tmp)
    digits(resmat_latex) <- c(0, rep(2, ncol(tmp)))
    list("raw" = tmp, "latex" = resmat_latex)
}


#' Create a results table in latex format given the name of a datset and a results structure.
#'
#' @param dataset_name The name of the dataset
#' @param res A results structure.
#'
#' @return Nothing.
#'
#' @export
special_table <- function(dataset_name = "dataset_name", res) {
    tmp <- res$raw
    out <- vector(mode = "list", length = (nrow(tmp) + 1))

    ## format the digits
    ## tmp <- format(tmp, digits = 2)
    tmp <- round(tmp, digits = 2)
    tmp <- formatC(tmp, digits = 2, format = "f")
    out[[1]] <- paste0("\\multirow{6}{*}{", dataset_name, "}")


    for (i in seq.int(nrow(tmp))) {
        alg_name <- fix_alg_name(rownames(tmp)[i])

        out[[i + 1]] <- paste0("   & ",alg_name, " & ", paste(tmp[i,], collapse = " & "), "\\\\")
    }

    for (i in out)
        cat(i, "\n")

    cat("\\midrule\n")
}


#' Calculate statistics for a core clustering experiment
#'
#' @param res A results structure.
#' @param trimmed_core Boolean indicating whether the clustering algorithm used trimming.
#'
#' @return A matrix with statistics.
#'
#' @export
calculate_stats_light <- function(res, trimmed_core = FALSE) {
    classes_true = res$classes_true

    if (is.null(classes_true))
        stop("True classes are NULL, cannot continue.")


    if (res$using_trim) {
        purity_o <- purity(classes_true, res$cluster, ignore_class = c(0, -1))
    }
    else {
        purity_o <- purity(classes_true, res$cluster)
    }

    purity_c <- purity(classes_true, res$cluster_core, ignore_class = c(0, -1))

    ## percentage of discarded points
    w          <- sum(res$cluster_core < 0) / length(res$cluster_core)
    out        <- c(purity_o, purity_c, w)
    names(out) <- c("po", "pc", "w")

    out

}


#' Calculate the purity of a clustering
#'
#' @param cl_true The true classes of a dataset
#' @param cl_other A clustering results
#' @param ignore_class A class index to ignore, e.g., if trimming was used.
#'
#' @return A matrix with statistics.
#'
#' @export
purity <- function(cl_true, cl_other, ignore_class = NULL) {
    if (! is.null(ignore_class)) {
        ind      <- which(cl_other %in% ignore_class)

        if (length(ind) > 0) {
            cl_other <- cl_other[-ind]
            cl_true  <- cl_true[-ind]
        }
    }

    N        <- length(cl_true)
    cl_other <- match_clusters(N, cl_other, cl_true)
    sum(apply(table(cl_true, cl_other), 2, max)) / length(cl_true)
}


#' Get the specifications of a dataset
#'
#' @param dataset A dataset
#' 
#' @return A list with the specifications of the dataset
#'
#' @export
dataset_specs <- function(dataset) {
    n_items   <- nrow(dataset$data)
    n_classes <- length(unique(dataset$classes))
    n_attributes <- ncol(dataset$data)
    major_class  <- max(table(dataset$classes)) / n_items

    list("n_items" = n_items,
         "n_classes" = n_classes,
         "n_attr"    = n_attributes,
         "major_class" = major_class )
}
## ------------------------------------------------------------------------------
