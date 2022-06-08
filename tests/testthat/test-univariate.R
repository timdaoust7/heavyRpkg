
test_that("correct result for univariate gaussian", {
  expect_equal(mvnpdf(x=matrix(1.96), Log=FALSE)$y, dnorm(1.96))
  expect_equal(mvnpdf(x=matrix(c(1.96, -0.5), ncol = 2), Log=FALSE)$y,
               dnorm(c(1.96, -0.5)))
})
# Compare to dnorm that computes the normal distribution value
# expect_equal --> true or false returned
