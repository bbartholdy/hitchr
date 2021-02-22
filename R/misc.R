#' This function creates randomly generated Vogon poetry, thought to be the second
#' worst poetry in the universe.
#'
#' @export
vogon_poetry <- function(){
  # really really bad poetry
}

#' Generates a random statement indicating the odds of an event
#'
#' @export
inf_improb_dr <- function(){
  prob <- runif(1, max = 10^9)
  prob <- format(round(prob), big.mark = ",")
  paste(prob, "to 1 against")
}

#' Generates a random statement indicating the odds of an event
#'
#' @export
infinite_improbability_drive <- function(){
  prob <- runif(1, max = 10^9)
  prob <- format(round(prob), big.mark = ",")
  paste(prob, "to 1 against")
}

#' The Guide
#'
#' @export

the_guide <- function(){
  "under construction"
  # function to retrieve guide entries from https://www.h2g2.com/ or
    # https://hitchhikers.fandom.com/wiki/Main_Page
}
