hum_samp <- humans(100)
na_prob <- 0.1
hum_samp_na <- missing(hum_samp, na_prob = na_prob, symm = T)
hum_vect_na <- hum_samp_na[,2]
hum_asymm_na <- is.na(missing(hum_samp, na_prob = na_prob))
is_miss_cols <- towel(hum_samp_na)
is_miss_rows <- towel(hum_samp_na, rowwise = T, colwise = F)

test_that("Test missing value creator function", {
  expect_equal(dim(hum_samp), dim(hum_samp_na))
  expect_equal(as.numeric(colMeans(is.na(hum_samp_na))), rep(na_prob, dim(hum_samp_na)[2]))
  expect_null(dim(hum_vect_na))
  expect_equal(mean(colMeans(hum_asymm_na)), na_prob)
})

hum_asymm_select <- is.na(missing(hum_samp, na_prob = na_prob, cols = 2:5, symm = F))

test_that("Test missing values in select columns", {
  expect_equal(mean(colMeans(hum_asymm_select[,2:5])), na_prob)
  expect_length(is_miss_cols, ncol(hum_samp_na))
  expect_length(is_miss_rows, nrow(hum_samp_na))
})

miss_prop_cols <- towel(hum_samp_na)
names(miss_prop_cols) <- NULL
na_cols_expected <- rep(na_prob, length(miss_prop_cols))

test_that("Test towel function", {
  expect_equal(miss_prop_cols, na_cols_expected)
})
