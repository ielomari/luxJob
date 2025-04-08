#' Get all skills
#'
#' Returns a list of skills from the `adem.skills` table, limited by `limit`.
#'
#' @param limit Integer. Maximum number of skills to return. Default is 100.
#'
#' @return A data frame with columns: `skill_id`, `skill_label`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_skills(limit = 20)
#' }
get_skills <- function(limit = 100) {
  if (!is.numeric(limit) || limit <= 0) {
    stop("`limit` must be a positive integer.")
  }

  con <- connect_db()

  query <- glue::glue_sql("
    SELECT skill_id, skill_label
    FROM adem.skills
    LIMIT {limit}
  ", .con = con)

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}

#' Get skill by ID
#'
#' Returns a skill from the `adem.skills` table based on the given `skill_id`.
#'
#' @param skill_id Character. The ID of the skill to retrieve.
#'
#' @return A data frame with one row and columns: `skill_id`, `skill_label`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_skill_by_id("skill_python")
#' }
get_skill_by_id <- function(skill_id) {
  if (!is.character(skill_id) || length(skill_id) != 1) {
    stop("`skill_id` must be a single character string.")
  }

  con <- connect_db()

  query <- glue::glue_sql("
    SELECT skill_id, skill_label
    FROM adem.skills
    WHERE skill_id = {skill_id}
    LIMIT 1
  ", .con = con)

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}
