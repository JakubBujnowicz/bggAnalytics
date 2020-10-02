test_that("empty", {
  expect_equal(squeeze(numeric(0)), "")
})

test_that("length one", {
    expect_equal(squeeze(1), "1")
})

vec <- c(0, 1, NA, 25:39, 7, 9, 2)
vec_nona <- sort(vec[!is.na(vec)])
sq_vec <- "0-2, 7, 9, 25-39"

test_that("example", {
    expect_equal(squeeze(vec), sq_vec)
})

# Errors
test_that("error on non-numeric", {
    expect_error(squeeze(letters))
})

test_that("error on non-integer", {
    expect_error(squeeze(pi))
})

# Unsqueeze
test_that("unsqueeze(squeeze(x))", {
    expect_equal(vec_nona, unsqueeze(squeeze(vec)))
})

test_that("squeeze(unsqueeze(x))", {
  expect_equal(sq_vec, squeeze(unsqueeze(sq_vec)))
})
