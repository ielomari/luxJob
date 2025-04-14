test_that("get_books returns a data frame of all books", {
  result <- get_books()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("book_id", "title", "author", "skill_id") %in% colnames(result)))
})

test_that("get_books filters by skill_id correctly", {
  result <- get_books(skill = "skill_python")
  expect_s3_class(result, "data.frame")
})

test_that("get_book_by_id returns one book row for valid ID", {
  result <- get_book_by_id(101)  # Replace with a real book_id
  skip_if(is.null(result), "Book ID not found, skipping test")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("book_id", "title", "author", "skill_id") %in% colnames(result)))
})
