#' Function to create a random sample of Dentrassi
#'
#' @details The Dentrassi are described as 'an unruly tribe of
#' gourmands, a wild but pleasant bunch whom the Vogons had recently taken to employ
#' as catering staff on their long-haul fleets (...).
#'
#' -- *Douglas Adams, Hitchiker's Guide to the Galaxy*
#' @inheritParams humans
#' @export
dentrassi <- function(n, stats = stats_index(), ...){
  if(n < 5) stop("'n' must be greater than or equal to 5")
  race <- rep("dentrassi", n)
  n_male <- round(n * rnorm(1, 0.5, 0.05))
  n_female <- n - n_male
  male_cov <- matrix(nrow = 2, ncol = 2)
  repeat{
  male_cov[1,1] <- rnorm(1, 80, 1)
  male_cov[1,2] <- rnorm(1, 70, 1)
  male_cov[2,1] <- male_cov[1,2]
  male_cov[2,2] <- rnorm(1, 100, 1)
  if(det(male_cov) > 0) break
  }
  repeat{
  female_cov <- matrix(nrow = 2, ncol = 2)
  female_cov[1,1] <- rnorm(1, 160, 1)
  female_cov[1,2] <- rnorm(1, 140, 1)
  female_cov[2,1] <- female_cov[1,2]
  female_cov[2,2] <- rnorm(1, 200, 1)
  if(det(female_cov) > 0) break
  }
  c_names <- c("height", "weight")
  males <- as.data.frame(
    MASS::mvrnorm(n_male,
                  c(rnorm(1, 200, 10), rnorm(1, 160, 10)),
                  male_cov))
  colnames(males) <- c_names
  males$sex <- rep("male", n_male)
  females <- as.data.frame(
    MASS::mvrnorm(n_female,
                  c(rnorm(1, 240, 10), rnorm(1, 190, 10)),
                  female_cov)
  )
  colnames(females) <- c_names
  females$sex <- rep("female", n_female)
  occupation <- hitchr::dentrassi_occupations[sample(1:nrow(hitchr::dentrassi_occupations),
                                                 size = n, replace = T), ]
  dentrassi_sample <- rbind(males, females)
  dentrassi_sample$IQ <- round(rnorm(n, 110, 15))
  dentrassi_sample$age <- round(runif(n, 10, 80))
  dentrassi_sample <- data.frame("race" = race, dentrassi_sample,
                                     "occupation" = occupation)
  dentrassi_sample <- dentrassi_sample[, stats]
  return(dentrassi_sample)
}

#' Function to create a random sample of Golgafrinchans
#'
#' @inheritParams humans
#' @export
golgafrinchans <- function(n, stats = stats_index(), ...){
  if(n < 5) stop("'n' must be greater than or equal to 5")
  race <- rep("golgafrinchan", n)
  n_male <- floor(n * 0.5107)
  n_female <- n - n_male
  male_cov <- matrix(nrow = 3, ncol = 3)
  repeat {
    male_cov[1,] <- c(52.89566, 56.31551, rnorm(1, 40, 1))
    male_cov[2,] <- c(56.31551, 80.5074, rnorm(1, 10, 1))
    male_cov[3,] <- c(male_cov[1,3], male_cov[2,3] ,rnorm(1, 100, 1))
    if(det(male_cov) > 0) break
  }
  repeat {
    female_cov <- matrix(nrow = 3, ncol = 3)
    female_cov[1,] <- c(46.90279, 50.20551, rnorm(1, 40, 1))
    female_cov[2,] <- c(50.20551, 74.45026, rnorm(1, 10, 1))
    female_cov[3,] <- c(female_cov[1,3], female_cov[2,3], rnorm(1,100, 1))
    if(det(female_cov) > 0) break
  }
  c_names <- c("height", "weight", "IQ")
  males <- as.data.frame(MASS::mvrnorm(n_male,
                                       c(175.3269, 84.83123, 100), male_cov))
  colnames(males) <- c_names
  males$sex <- rep("male", n_male)

  females <- as.data.frame(MASS::mvrnorm(n_female,
                                         c(161.8203, 61.62517, 101), female_cov))
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
  if(n < 5) stop("'n' must be greater than or equal to 5")
  race <- rep("human", n)
    # sex ratios
  n_male <- round(n * rnorm(1, 0.5107, 0.01))
  n_inter <- round(n * rnorm(1, 0.003, 0.001)) # need better method
  n_female <- n - n_male
    #  Male  51.07
  #  Female  48.63
  #  Intersex  0.30

  # height and weight stats from ?
    # covariance matrices for height, weight, and IQ
  male_cov <- matrix(nrow = 3, ncol = 3)
  repeat {
  male_cov[1,] <- c(52.89566, 56.31551, rnorm(1, 40, 1))
  male_cov[2,] <- c(56.31551, 80.5074, rnorm(1, 10, 1))
  male_cov[3,] <- c(male_cov[1,3], male_cov[2,3] ,rnorm(1, 100, 1))
  if(det(male_cov) > 0) break
  }
  repeat {
  female_cov <- matrix(nrow = 3, ncol = 3)
  female_cov[1,] <- c(46.90279, 50.20551, rnorm(1, 40, 1))
  female_cov[2,] <- c(50.20551, 74.45026, rnorm(1, 10, 1))
  female_cov[3,] <- c(female_cov[1,3], female_cov[2,3], rnorm(1,100, 1))
  if(det(female_cov) > 0) break
  }
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
  occupation <- hitchr::human_occupations[sample(1:nrow(hitchr::human_occupations), size = n, replace = T), ]
  names(occupation) <- "occupation"
  human_sample <- rbind(males, females)
  human_sample$race <- race
  human_sample$sex[sample(1:nrow(human_sample), size = n_inter)] <- "intersex"
  human_sample$sex <- as.factor(human_sample$sex)
  human_sample$age <- round(runif(n, 18, 120)) # only "adults"
  human_sample$IQ <- round(human_sample$IQ)
  human_sample$occupation[human_sample$age > 70] <- paste(human_sample$occupation, "(retired)")
  human_sample$occupation <- as.factor(occupation)
  human_sample <- human_sample[, stats]
  return(human_sample)
}

#' Function to create a random sample of Vogons
#'
#' @details The Guide on Vogons: "They are one of the most unpleasant races in the
#' Galaxy -- not actually evil, but bad-tempered, bureaucratic, officious and callous.
#' They wouldn't even lift a finger to save their own grandmothers from the
#' Ravenous Bugblatter Beast of Traal without orders signed in triplicate, sent in,
#' sent back, queried, lost, found, subjected to public inquiry, lost again, and
#' finally buried in soft peat for three months and recycled as firelighters."
#'
#' "The best way to get a drink out of a Vogon is to stick your finger down his
#' throat, and the best way to irritate him is to feed his grandmother to the
#' Ravenous Bugblatter Beast of Traal."
#'
#' "On no account allow a Vogon to read poetry at you."
#'
#' -- *Douglas Adams, Hitchiker's Guide to the Galaxy*
#'
#' Most notable about Vogons is that the majority of occupations are
#' administrative in nature; they are born/appear fully educated for said occupation;
#' and the males and females of the species are completely indistinguishable.
#' @inheritParams humans
#' @export
vogons <- function(n, stats = stats_index(), ...){
  if(n < 5) stop("'n' must be greater than or equal to 5")
  repeat {
  vogon_cov <- matrix(nrow = 2, ncol = 2)
  vogon_cov[1,1] <- rnorm(1, 60, 5)
  vogon_cov[1,2] <- rnorm(1, 80)
  vogon_cov[2,1] <- vogon_cov[1,2]
  vogon_cov[2,2] <- rnorm(1, 150, 10)
  if(det(vogon_cov) > 0) break
  }
  vogon_sample <- as.data.frame(
    MASS::mvrnorm(n, c(230, 350), vogon_cov, empirical = T))
  colnames(vogon_sample) <- c("height", "weight")
  vogon_sample$age <- round(runif(n, min = 30, max = 180)) #born/appear at age 30 fully educated in some administrative duty
  vogon_sample$race <- as.factor(rep("vogon", n))
  sex_prob <- c(rnorm(2, 0.4, 0.01))
    sex_prob <- c(sex_prob, 1 - sum(sex_prob))
  vogon_sample$sex <- sample(c("male", "female", "other"), n, replace = T, prob = sex_prob)
  vogon_sample$IQ <- round(rnorm(n, 120, 5))
  occupation <- hitchr::vogon_occupations[sample(1:nrow(hitchr::vogon_occupations), size = n, replace = T), ]
  names(occupation) <- "occupation"
  vogon_sample <- data.frame(vogon_sample,
                             "occupation" = occupation)
  vogon_sample <- vogon_sample[, stats]
  return(vogon_sample)
}
