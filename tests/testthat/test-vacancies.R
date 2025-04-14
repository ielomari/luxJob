test_that("get_vacancies returns a data frame with expected columns", {
  result <- get_vacancies(limit = 10)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("vacancy_id", "company_id", "occupation", "canton", "year", "month") %in% colnames(result)))
  expect_lte(nrow(result), 10)
})

test_that("get_vacancies with skill, company, and canton filters returns results", {
  result <- get_vacancies(skill = "skill_python", company = 42, canton = "Luxembourg", limit = 5)
  expect_s3_class(result, "data.frame")
})

test_that("get_vacancy_by_id returns correct structure for valid ID", {
  result <- get_vacancy_by_id(123456)  # Replace with real vacancy ID
  skip_if(is.null(result), "Vacancy ID not found, skipping test")

  expect_type(result, "list")
  expect_true(all(c("vacancy", "skills") %in% names(result)))
  expect_s3_class(result$vacancy, "data.frame")
  expect_s3_class(result$skills, "data.frame")
})
