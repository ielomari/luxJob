test_that("connect_db returns a valid connection", {
  con <- connect_db()

  expect_s4_class(con, "PqConnection")

  DBI::dbDisconnect(con)
})
