test_that("get_companies returns a data frame with expected columns", {
  result <- get_companies(limit = 10)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("company_id", "name", "sector") %in% colnames(result)))
  expect_lte(nrow(result), 10)
})

test_that("get_company_details returns company and its vacancies", {
  # Use a valid company_id from your database
  result <- get_company_details(42)
  skip_if(is.null(result), "Company ID not found in DB, skipping test")

  expect_type(result, "list")
  expect_true(all(c("company", "vacancies") %in% names(result)))

  expect_s3_class(result$company, "data.frame")
  expect_true(nrow(result$company) == 1)

  expect_s3_class(result$vacancies, "data.frame")
})
