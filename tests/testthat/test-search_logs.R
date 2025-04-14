test_that("log_search fails with invalid user_id or query", {
  expect_error(
    log_search(user_id = "one", query = "valid query"),
    regexp = "^`user_id` must be a single integer\\.$"
  )

  expect_error(
    log_search(user_id = 1, query = 1234),
    regexp = "^`query` must be a single character string\\.$"
  )
})
