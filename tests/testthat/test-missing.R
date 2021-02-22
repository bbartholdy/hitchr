hum_samp <- humans(100)
na_prob <- 0.1
hum_samp_na <- missing(hum_samp, na_prob = na_prob)
hum_vect_na <- hum_samp_na[,2]

test_that("Test missing value creator function", {
  expect_equal(dim(hum_samp), dim(hum_samp_na))
  expect_equal(as.numeric(colMeans(is.na(hum_samp_na))), rep(na_prob, dim(hum_samp_na)[2]))
  expect_null(dim(hum_vect_na))
})
