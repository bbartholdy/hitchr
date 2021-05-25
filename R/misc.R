#' This function creates randomly generated Vogon poetry, thought to be the third
#' worst poetry in the universe, only surpassed by that of the Azgoths of Kria,
#' and Paula Nancy Millstone Jennings of Sussex.
#'
#' @export
vogon_poetry <- function(){
  "This function is currently under construction. It cannot proceed until orders
  have been signed in triplicates."
}

drive_output <- function(){
  prob <- runif(1, max = 10^9)
  prob <- format(round(prob), big.mark = ",")
  paste(prob, "to 1 against")
}

#' The Guide
#'
#' @export

the_guide <- function(){
  "This function is currently under construction. It cannot proceed until orders
  have been signed in triplicates."
}

# calculate the proportion of missing values within a variable or row of a data frame.
#' This function is about the most massively useful thing in this package.
towel <- function(x, rowwise = F, colwise = T, cols = NULL){
  if(is.null(dim(x))){
    prop_na <- mean(is.na(x))
    return(prop_na)
  } else {
    prop_na_cols <- colMeans(is.na(x))
    prop_na_rows <- rowMeans(is.na(x))
      if(!is.null(cols)){
        prop_na_cols <- prop_na_cols[,cols]
      }
    }
  if(rowwise == F) {
    return(prop_na_cols)
  } else if(colwise == F){
    return(prop_na_rows)
  } else {
    prop_na <- list("cols" = prop_na_cols, "rows" = prop_na_rows)
  }
  return(prop_na)
}
