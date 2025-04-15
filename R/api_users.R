#' Create a new API user
#'
#' @param username The new username
#' @param token The associated API token
#' @param quota Number of requests allowed (default = 100)
#' @return TRUE if the user was created, FALSE otherwise
#' @export
create_user <- function(username, token, quota = 100) {
  if (!is.character(username) || length(username) != 1) {
    stop("`username` must be a single character string.")
  }
  if (!is.character(token) || length(token) != 1) {
    stop("`token` must be a single character string.")
  }
  if (!is.numeric(quota) || length(quota) != 1) {
    stop("`quota` must be a single numeric value.")
  }

  con <- connect_db()
  success <- tryCatch({
    DBI::dbExecute(con, glue::glue_sql("
      INSERT INTO student_ibtissam.api_users (username, token, quota)
      VALUES ({username}, {token}, {quota})
    ", .con = con))
    TRUE
  }, error = function(e) {
    warning("Failed to create user: ", conditionMessage(e))
    FALSE
  })
  DBI::dbDisconnect(con)
  return(success)
}
