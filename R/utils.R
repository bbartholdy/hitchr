#' @inheritParams missing
missing_vector <- function(x, na_prob){
  x[sample(seq_along(x), round(na_prob * length(x)))] <- NA
  return(x)
}

# From 'wakefield' package
#' Inserts NAs throughout a data frame at random
#'
#' @details Function is a modified version of 'r_na' and r_na_vector' from the
#' 'wakefield' package.
#' @param x Data frame or vector.
#' @param na_prob the probability of missing values per variable in the data frame.
#' Default is 0.1, i.e. each variable will have 10% NAs.
#' @param cols numeric. In which columns to insert NAs. Default is all columns.
#' To leave out the first column, use cols = -1.
#' @seealso \code{\link[wakefield]{r_na}}
missing <- function(x, na_prob = 0.1, cols = seq_along(x)){
  if(na_prob > 1){
    na_prob <- na_prob / 100
    warning(paste("'missing_prob' must be a probability between 0 and 1 \n
              Input was automatically replaced by", na_prob))
  }
  if(is.null(dim(x))){
    x <- missing_vector(x, na_prob = na_prob)
  } else {
  x[c(cols)] <- lapply(x[c(cols)], missing_vector, na_prob = na_prob)
  }
  return(x)
}

#' Create a random sample
#'
#' @param n number of individuals to generate.
#' @param race which races to include in the generated sample. Default is all.
#' @param missing_prob Numeric. Proportion of values that should be missing at random.
#' Must be a number between 0 and 1.
#' @param ... can be used to pass 'stats' through to race-specific functions. see example.
#' @importFrom dplyr as_tibble
#'
#' @export
h2g2 <- function(n, race = race_index(), na_prob = NULL, ...){

   race <- match.arg(race, race_index(), T)
   race <- as.list(race)
   args <- list(n, ...)
   x <- lapply(race, do.call, args)
   x <- as.data.frame(data.table::rbindlist(x, use.names = T))
   h2g2_sample <- x[sample(1:nrow(x), size = n, replace = F), ]
  # create randomly missing variables if input is used
  if(!is.null(na_prob)){
    h2g2_sample <- missing(h2g2_sample, na_prob = na_prob, cols = seq_along(h2g2_sample))
  }
  h2g2_sample <- dplyr::as_tibble(h2g2_sample)
  return(h2g2_sample)
}

#' The Infinite Improbability Drive
#'
#' Generates a random sample of individuals.
#'
#' @inheritParams h2g2
#' @param quiet if FALSE, will print a statement about the odds of a random event.
#' Default is TRUE.
#' @export
infinite_improbability_drive <- function(n, race = race_index(), na_prob = NULL,
                                         quiet = T, ...){

  race <- match.arg(race, race_index(), T)
  race <- as.list(race)
  args <- list(n, ...)
  x <- lapply(race, do.call, args)
  x <- as.data.frame(data.table::rbindlist(x, use.names = T))
  h2g2_sample <- x[sample(1:nrow(x), size = n, replace = F), ]
  # create randomly missing variables if input is used
  if(!is.null(na_prob)){
    h2g2_sample <- missing(h2g2_sample, na_prob = na_prob, cols = seq_along(h2g2_sample))
  }
  h2g2_sample <- dplyr::as_tibble(h2g2_sample)
  if(quiet == FALSE){
    iidr()
  }
  return(h2g2_sample)
}

race_index <- function(){
  race <- c("betegeusians", "dentrassi", "dolphins", "golgafrinchans",
            "haggunenons", "humans", "jatravartids", "krikkits",
            "magratheans", "mice", "vogons")
  current <- c("humans", "vogons", "golgafrinchans", "dentrassi")
  return(current)
}

#' @inheritParams infinite_improbability_drive
inf_improb_dr <- function(...){
  infinite_improbability_drive(...)
}

stats_index <- function(){
  stats <- c("race", "sex", "age", "height", "weight", "IQ", "occupation")
  return(stats)
}
