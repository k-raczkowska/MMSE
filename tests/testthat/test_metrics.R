testthat::test_that('longest chain test', {
  testthat::expect_equal(scfa(1763098), 1)
  testthat::expect_equal(scpa(1763098), 2)
  testthat::expect_equal(lcpa(1763098), 55)
  testthat::expect_equal(lcfa(1763098), 8)
})
