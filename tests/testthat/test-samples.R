n <- floor(runif(1, 5, 100))
stats <- c("race", "sex", "occupation")
race <- c("human", "vogon")
hum_sample <- humans(n)
vog_sample <- vogons(n)
vog_sample_stats <- vogons(n, stats = stats)
hum_sample_stats <- humans(n, stats = stats)
custom_sample <- h2g2(n, race = race, stats = stats)

# test various sample sizes
hitchr_sample <- list()
for(i in 5:100){
  hitchr_sample[[i-4]] <- infinite_improbability_drive(i, quiet = T)
}

# make sure there are no sporadic errors
hitchr_sample2 <- replicate(1000, infinite_improbability_drive(n))

test_that("Test output characteristics of sample generators", {
  expect_equal(dim(hum_sample)[1], n)
  #expect_type(hum_sample$occupation, "factor")
  expect_equal(dim(hum_sample_stats)[2], length(stats))
  expect_equal(dim(vog_sample)[1], n)
  #expect_type(vog_sample$occupation, "factor")
  expect_equal(dim(vog_sample_stats)[2], length(stats))
  expect_equal(levels(custom_sample$race), race)
  expect_equal(dim(custom_sample)[2], length(stats))
  expect_silent(hitchr_sample)
  expect_silent(hitchr_sample2)
  })
