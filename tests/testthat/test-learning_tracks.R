test_that("get_learning_tracks returns a data frame of all tracks", {
  result <- get_learning_tracks()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("track_id", "title", "description", "url") %in% colnames(result)))
})

test_that("get_learning_tracks filters by skill_id correctly", {
  result <- get_learning_tracks(skill_id = "skill_r")
  expect_s3_class(result, "data.frame")
  # You can optionally check column names or result content
})

test_that("get_learning_track_by_id returns a track and its skills", {
  result <- get_learning_track_by_id(71)  # Remplace par un vrai track_id si possible
  skip_if(is.null(result), "Track ID not found, skipping test")

  expect_type(result, "list")
  expect_true(all(c("track", "skills") %in% names(result)))

  expect_s3_class(result$track, "data.frame")
  expect_equal(nrow(result$track), 1)

  expect_s3_class(result$skills, "data.frame")
})
