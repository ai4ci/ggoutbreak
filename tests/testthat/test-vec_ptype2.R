testthat::test_that("S3 column with date attribute can be joined", {
  # I wasn't sure what to expect here however
  # integer based dates are comparable with double based dates in vec_ptype2
  lhs2 = dplyr::tibble(
    time = as.time_period(as.Date("2020-01-01") + 0:9, anchor = "start"),
    col1 = "test_lhs"
  )

  rhs2 = dplyr::tibble(
    time = as.time_period(0:1, unit = "1 week"),
    col2 = "test_rhs"
  )

  # PASSES AS IS:
  testthat::expect_no_error({
    dplyr::left_join(lhs2, rhs2, by = c("time"))
  })
})
