test_that("get_skills returns a data frame with skill_id and skill_label", {
  result <- get_skills(limit = 5)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("skill_id", "skill_label") %in% colnames(result)))
  expect_lte(nrow(result), 5)
})

test_that("get_skill_by_id returns one row for a valid skill_id", {
  result <- get_skill_by_id("http://data.europa.eu/esco/skill/00735755-adc6-4ea0-b034-b8caff339c9f")
  skip_if(is.null(result), "skill_id not found in DB, skipping test")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("skill_id", "skill_label") %in% colnames(result)))
})
