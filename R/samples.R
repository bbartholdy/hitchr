#' Function to create a random sample of Betelgeusians
#'
#' @inheritParams humans
#' @export
betelgeusians <- function(n, stats = stats_index(),
                          ...){
  race <- rep("betelgeusian", n)
  sigma <- matrix(nrow = 2, ncol = 2)
  sigma[1,1] <- rnorm(1, 70, 1)
  sigma[1,2] <- rnorm(1, 90, 1)
  sigma[2,1] <- sigma[1,2]
  sigma[2,2] <- rnorm(1, 70, 1)
  betel_hw <- as.data.frame(
    MASS::mvrnorm(n, c(168.5736, 73.2282), sigma, empirical = T)
  )
  IQ <- rnorm(n, 130)
  age <- runif(n, 0, 320)
}

#' @inheritParams humans
dentrassi <- function(...){
  # and so on...
}

#' @inheritParams humans
dolphins <- function(...){

}

#' Function to create a random sample of Golgafrinchans
#'
#' @inheritParams humans
#' @export
golgafrinchans <- function(n,
                           stats = stats_index(),
                           ...){
  race <- rep("golgafrinchan", n)
  n_male <- floor(n * 0.5107)
  n_female <- n - n_male
  male_cov <- matrix(c(52.89566, 56.31551, 40, 56.31551, 80.5074, 10, 40, 10, 400),3,3)
  female_cov <- matrix(c(46.90279, 50.20551, 40, 50.20551, 74.45026, 10, 40, 10, 100),3,3)
  c_names <- c("height", "weight", "IQ")
  males <- as.data.frame(
    MASS::mvrnorm(n_male,
                  c(175.3269, 84.83123, 100),
                  male_cov))
  colnames(males) <- c_names
  males$sex <- rep("male", n_male)
  females <- as.data.frame(
    MASS::mvrnorm(n_female,
                  c(161.8203, 61.62517, 101),
                  female_cov)
  )
  colnames(females) <- c_names
  females$sex <- rep("female", n_female)
  age <- round(runif(n, 0, 120))
  occupation <- sample(c("Telephone Sanitiser", "Lawyer", "Hairdresser"),
                       size = n, replace = T, ...)
  golgafrinchan_sample <- rbind(males, females)
  golgafrinchan_sample$IQ <- round(golgafrinchan_sample$IQ)
  golgafrinchan_sample <- data.frame("race" = race, golgafrinchan_sample, "age" = age,
                                     "occupation" = occupation)
  golgafrinchan_sample <- golgafrinchan_sample[, stats]
  return(golgafrinchan_sample)
}

#' Random sample of humans
#'
#' The entry in The Guide for Earth: "Mostly harmless."
#' @details Function to create a random sample of humans
#' @param n numeric. Number of individuals to create.
#' @param stats which stats to include in the sample. See 'stats_index()' for a
#' list of available stats.
#' @param ... currently serves no function.
#' @export
humans <- function(n, stats = stats_index(), ...){
  race <- rep("human", n)
    # sex ratios
  n_male <- floor(n * 0.5107)
  n_female <- n - n_male
  #n_female <- floor(n * 0.4863)
  #n_inter <- n - n_male - n_female
  #  Male  51.07
  #  Female  48.63
  #  Intersex  0.30

  # height and weight stats from ?
    # covariance matrices for height, weight, and IQ
  male_cov <- matrix(c(52.89566, 56.31551, 40, 56.31551, 80.5074, 10, 40, 10, 400),3,3)
  female_cov <- matrix(c(46.90279, 50.20551, 40, 50.20551, 74.45026, 10, 40, 10, 100),3,3)
    # random multivariate normal data frame with pre-defined means
  c_names <- c("height", "weight", "IQ")
  males <- as.data.frame(
    MASS::mvrnorm(n_male,
                  c(175.3269, 84.83123, 100),
                  male_cov))
  colnames(males) <- c_names
  males$sex <- rep("male", n_male)
  females <- as.data.frame(
    MASS::mvrnorm(n_female,
                  c(161.8203, 61.62517, 101),
                  female_cov)
    )
  colnames(females) <- c_names
  females$sex <- rep("female", n_female)
  age <- round(runif(n, 0, 120))
  occupation <- hitchr::human_occupations[sample(1:nrow(hitchr::human_occupations), size = n, replace = T), ]
  names(occupation) <- "occupation"
  occupation <- as.factor(occupation)
  human_sample <- rbind(males, females)
  human_sample$sex <- as.factor(human_sample$sex)
  human_sample$IQ <- round(human_sample$IQ)
  human_sample <- data.frame("race" = race , human_sample, "age" = age, "occupation" = occupation)
  human_sample <- human_sample[, stats]
  return(human_sample)
}

#' Function to create a random sample of Vogons
#'
#' @inheritParams humans
#' @export
vogons <- function(n, stats = stats_index(), ...){
  vogon_cov <- matrix(nrow = 2, ncol = 2)
  vogon_cov[1,1] <- rnorm(1, 60)
  vogon_cov[1,2] <- rnorm(1, 80)
  vogon_cov[2,1] <- vogon_cov[1,2]
  vogon_cov[2,2] <- rnorm(1, 150)
  hw_data <- MASS::mvrnorm(n, c(230, 350), vogon_cov, empirical = T)
  hw_data <- as.data.frame(hw_data)
  colnames(hw_data) <- c("height", "weight")
  age <- round(runif(n, min = 30, max = 180)) #born/appear at age 30 fully educated in some administrative duty
  race <- as.factor(rep("vogon", n))
  sex <- rep("no thanks", n)
  iq <- round(rnorm(n, 120, 5))
  occupation <- hitchr::vogon_occupations[sample(1:nrow(hitchr::vogon_occupations), size = n, replace = T), ]
  names(occupation) <- "occupation"
  vogon_sample <- data.frame("race" = race, "sex" = sex, "age" = age, hw_data, "IQ" = iq,
                             "occupation" = occupation)
  vogon_sample <- vogon_sample[, stats]
  return(vogon_sample)
}
