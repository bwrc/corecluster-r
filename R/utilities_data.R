## ------------------------------------------------------------------------------
## Functions used for generating and reading data used for the experiments
## ------------------------------------------------------------------------------

#' Get synthetic data
#'
#' @param N The number of data points. Default is 150.
#' @param data.mean Data mean.
#' @param data.sd Data standard deviation.
#' @param type Type of data. String: 'matrix' or 'df'.
#'
#' @return A list with the synthetic data points and their corresponding classes (clusters).
#'
#' @export
get_data_synthetic <- function(N = 150, data.mean = 0, data.sd = 1, type = "matrix") {
    a        <- data.sd * 1.5
    classes  <- sample.int(3, size = N, replace = TRUE)
    centres  <- matrix(c(c(0, a), c(a * sqrt(3)/2, -0.5*a), c(-a * sqrt(3)/2, -0.5*a)), ncol = 2, nrow = 3, byrow = TRUE)
    data     <- matrix(rnorm(2 * N, mean = data.mean, sd = data.sd), nrow = N, ncol = 2) + centres[classes,]

    if (type == "df") {
        data       <- as.data.frame(data)
        data$class <- as.factor(classes)
    }


    list("data" = data, "classes" = classes, "name" = "synthetic")
}


#' Get the synthetic dataset
#'
#' @param N The number of data points. Default is 150.
#' @param data.mean Data mean.
#' @param data.sd Data standard deviation.
#' @param seed Random seed. Default is 42.
#' @param type Type of data. String: 'matrix' or 'df'.
#'
#' @return A list with the synthetic data points and their corresponding classes (clusters).
#'
#' @export
get_dataset_synthetic <- function(N = 150, data.mean = 0, data.sd = 1, seed = 42, type = "matrix") {
    set.seed(seed)
    get_data_synthetic(N = N, data.mean = data.mean, data.sd = data.sd, type = type)
}


#' Read UCI datasets
#'
#' @param dslist A list of available datasets obtained using the function \code{prepareDSList} from the package \code{readMLData}.
#' @param dataset_name The name of the dataset to read
#' @param remove_class Should the class be removed from the data. Default is TRUE.
#' @param as_df Should the data be returned as a data frame. Default is FALSE, which returns a matrix.
#'
#' @return A list with the fields:
#' \describe{
#' \item{data}{The data in the dataset}
#' \item{classes}{The classes in the dataset}
#' \item{name}{The name of the dataset}
#' }
#' 
#' @export
read_uci_dataset <- function(dslist, dataset_name, remove_class = TRUE, as_df = FALSE) {
    dataset             <- dsRead(dslist, dataset_name)
    ind                 <- which(sapply(dataset, class) == "factor")
    dataset[, ind]      <- as.numeric(dataset[, ind])
    names(dataset)[ind] <- "class"

    ## Remove rows with missing values
    ind_na <- which(is.na(rowSums(as.matrix(dataset[,-ind]))))
    if (length(ind_na) > 0) {
        data <- as.matrix(dataset)[-ind_na, ]
        cl   <- dataset[-ind_na, ind]
    } else {
        data <- as.matrix(dataset)
        cl   <- dataset[, ind]
    }

    if (remove_class)
        out <- data[, -ind]
    else
        out <- data


    if (as_df) {
        out <- as.data.frame(out)
        if (! remove_class)
            out$class <- as.factor(out$class)
    }

    list(list("data" = out, "classes" = cl, "name" = dataset_name))
}


#' Load the KDD Cup 1999 dataset
#'
#' @param datapath The path to the data
#'
#' @return The data
#'
#' @export
load_kdd_cup_1999_data <- function(datapath) {
    data <- read.table(datapath, sep = ",")

    ## The last column is the class-column
    names(data)[length(names(data))] <- "class"

    data$class <- gsub("\\.", "", data$class)
    data$class <- as.factor(data$class)

    ## Remove non-numeric columns
    data <- data[,-c(2, 3, 4)]

    data
}


#' Load the KDD Cup 1999 dataset
#'
#' @param kdd_data_path The path to the data
#' @param N_samples The number of samples to obtain. Default is 200.
#' @param format String. Either \code{df} to return a data frame or \code{matrix} to return a matrix.
#' @param mode either \code{sample} to return N_samples of data items or \code{full} (default) to return the entire dataset
#' @param seed Random seed used when mode is \code{sample}. Default is 42.
#'
#' @return The data
#'
#' @export
get_data_kdd <- function(kdd_data_path, N_samples = 200, format = "df", mode = "full", seed = 42) {
    data_kdd      <- load_kdd_cup_1999_data(kdd_data_path)
    data_kdd      <- droplevels(subset(data_kdd, class %in% c("normal", "smurf", "neptune")))

    if (mode == "sample") {
        set.seed(seed)
        data_kdd <- data_kdd[sample(seq.int(nrow(data_kdd)), N_samples),]
    }

    if (format == "df") {
        out <-  list("data" = data_kdd, "classes" = as.numeric(data_kdd$class), name = "kdd")
    }
    if (format == "matrix") {
        classes <- as.numeric(data_kdd$class)
        ind_c <- which(names(data_kdd) == "class")
        data_kdd <- as.matrix(data_kdd)[,-ind_c]
        class(data_kdd) <- "numeric"
        out <-  list("data" = data_kdd, "classes" = classes, name = "kdd")
    }

    out
}

## ------------------------------------------------------------------------------
