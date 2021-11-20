#' @inheritParams missing
missing_vector <- function(x, na_prob){
  if(!is.null(dim(x))){
    n_vector <- nrow(x)
    x[sample.int(n_vector, round(na_prob * n_vector)),] <- NA
  } else {
    n_vector <- length(x)
    x[sample(seq_along(x), round(na_prob * n_vector))] <- NA
  }

  return(x)
}

#' @inheritParams missing
missing_symm <- function(x, na_prob = na_prob, cols = seq_along(x)){
  # add if length(na_prob) > 1
  if(is.null(dim(x))){
    x <- missing_vector(x, na_prob = na_prob)
  } else {
    x[c(cols)] <- lapply(x[c(cols)], missing_vector, na_prob = na_prob)
  }
  return(x)
}

#' @inheritParams missing
missing_asymm <- function(x, na_prob = na_prob, cols = seq_along(x)){
  x_na <- x[cols]
  n_vect <- nrow(x_na) * ncol(x_na)
  na_vector <- c(rep(1, n_vect))
  na_vector[sample(1:length(na_vector), size = na_prob * n_vect)] <- NA
  na_matrix <- is.na(matrix(na_vector, nrow = nrow(x_na), ncol = ncol(x_na)))
  x_na[na_matrix] <- NA
  x[cols] <- x_na
  return(x)
}

# Some functionality from 'wakefield' package
#' Inserts NAs throughout a data frame at random
#'
#' @details Function is a modified version of 'r_na' and r_na_vector' from the
#' 'wakefield' package. It adds the functionality to add NAs asymmetrically to
#' a data frame, meaning the number of NAs per column will vary.
#' @param x Data frame or vector.
#' @param na_prob the probability of missing values per variable in the data frame.
#' Default is 0.1, i.e. each variable will have 10% NAs.
#' @param cols numeric. In which columns to insert NAs. Default is all columns.
#' To leave out the first column, use cols = -1.
#' @param symm logical. whether the missing values should be inserted symmetrically,
#' i.e., whether each column should contain the same number of NAs.
#' @return Returns the original data frame with NAs inserted. The default settings
#' returns a data frame with data missing completely at random.
#' @seealso \code{\link[wakefield]{r_na}}
missing <- function(x, na_prob = 0.1, cols = seq_along(x), symm = F){
  if(is.matrix(x)) {
    x <- as.data.frame(x)
    warning("missing() does not currently support matrices. Input was converted to a data frame")
  }
  if(na_prob > 1){
    na_prob <- na_prob / 100
    warning(paste("'na_prob' must be a probability between 0 and 1 \n
              Input was automatically replaced by", na_prob))
  }
  if(symm == TRUE){

    x <- missing_symm(x, na_prob, cols)

  } else if(symm == FALSE) {

    if(length(na_prob) > 1){
      if(length(na_prob) != length(cols)){
        stop(paste("na_prob must be a vector of length 1 or", length(cols),
                   "to match the selected columns:", paste(cols, collapse = ", ")))
      }
      for(i in seq_along(cols)){
        x[,i] <- missing_vector(x[,i], na_prob[i])
      }
      apply(x[c(cols)], 2, missing_vector, na_prob)
    }
    x <- missing_asymm(x, na_prob, cols)
  }
  return(x)
}

#' The Infinite Improbability Drive
#'
#' Generates a random sample of individuals.
#'
#' @param n number of individuals to generate.
#' @param race which races to include in the generated sample. Default is all.
#' @param na_prob Numeric. Proportion of values that should be missing at random.
#' Must be a number between 0 and 1.
#' @param ... can be used to pass 'stats' through to race-specific functions. see example.
#' @param quiet if FALSE, will print a statement about the odds of a random event.
#' Default is TRUE.
#' @param symm logical. whether the missing values should be inserted symmetrically,
#' i.e., whether each column should contain the same number of NAs.
#' @importFrom dplyr as_tibble
#' @export
infinite_improbability_drive <- function(n, race = race_index(), na_prob = NULL,
                                         quiet = T, symm = T, ...){
  race <- match.arg(race, race_index(), T)
  race <- as.list(race)
  args <- list(n, ...)
  x <- lapply(race, do.call, args)
  x <- as.data.frame(data.table::rbindlist(x, use.names = T))
  h2g2_sample <- x[sample(1:nrow(x), size = n, replace = F), ]
  # create randomly missing variables if input is used
  if(!is.null(na_prob)){
    if(symm == FALSE){
      h2g2_sample <- missing(h2g2_sample, na_prob = na_prob,
                             cols = seq_along(h2g2_sample), symm = F)
    } else {
      h2g2_sample <- missing(h2g2_sample, na_prob = na_prob,
                           cols = seq_along(h2g2_sample))
    }
  }
  h2g2_sample <- dplyr::as_tibble(h2g2_sample)
  if(quiet == FALSE){
    print(drive_output())
  }
  return(h2g2_sample)
}

#' Same functionality as the 'infinite_improbability_drive' function but infinitely
#' easier to write!
#' @inheritParams infinite_improbability_drive
#' @rdname iidr
iidr = infinite_improbability_drive

race_index <- function(){
  race <- c("betegeusians", "dentrassi", "dolphins", "golgafrinchans",
            "haggunenons", "humans", "jatravartids", "krikkits",
            "magratheans", "mice", "vogons")
  current <- c("humans", "vogons", "golgafrinchans", "dentrassi")
  return(current)
}

stats_index <- function(){
  stats <- c("race", "sex", "age", "dob", "height", "weight", "IQ", "occupation",
             "ses", "income")
  current <- c("race", "sex", "age", "height", "weight", "IQ", "occupation")
  return(current)
}
