#' Log a search query from an API user
#'
#' @param user_id Integer ID of the user
#' @param query Search query string
#' @return TRUE if the log was inserted, FALSE otherwise
#' @export
log_search <- function(user_id, query) {
  if (!is.numeric(user_id) || length(user_id) != 1) {
    stop("`user_id` must be a single integer.")
  }
  if (!is.character(query) || length(query) != 1) {
    stop("`query` must be a single character string.")
  }

  con <- connect_db()
  success <- tryCatch({
    DBI::dbExecute(con, glue::glue_sql("
      INSERT INTO student_ibtissam.search_logs (user_id, query, query_time)
      VALUES ({user_id}, {query}, NOW())
    ", .con = con))
    TRUE
  }, error = function(e) {
    warning("Failed to log search: ", conditionMessage(e))
    FALSE
  })
  DBI::dbDisconnect(con)
  return(success)
}
