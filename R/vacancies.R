#' Get job vacancies
#'
#' Returns job vacancies with optional filters for skill, company, and canton.
#'
#' @param skill Character. Optional. Filter by skill ID.
#' @param company Integer. Optional. Filter by company ID.
#' @param canton Character. Optional. Filter by canton name.
#' @param limit Integer. Maximum number of vacancies to return. Default is 100.
#'
#' @return A data frame with columns: `vacancy_id`, `company_id`, `occupation`, `canton`, `year`, `month`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_vacancies(skill = "skill_python", canton = "Luxembourg")
#'   get_vacancies(company = 42)
#' }
get_vacancies <- function(skill = NULL, company = NULL, canton = NULL, limit = 100) {
  if (!is.numeric(limit) || limit <= 0) {
    stop("`limit` must be a positive integer.")
  }

  con <- connect_db()

  # Initialize condition string
  where_clauses <- c()

  # Manually construct condition for skill subquery
  if (!is.null(skill)) {
    if (!is.character(skill)) stop("`skill` must be a character string.")
    where_clauses <- c(where_clauses,
                       paste0("vacancy_id IN (SELECT vacancy_id FROM adem.vacancy_skills WHERE skill_id = '", skill, "')"))
  }

  if (!is.null(company)) {
    if (!is.numeric(company)) stop("`company` must be an integer.")
    where_clauses <- c(where_clauses, paste0("company_id = ", company))
  }

  if (!is.null(canton)) {
    if (!is.character(canton)) stop("`canton` must be a character string.")
    where_clauses <- c(where_clauses, paste0("canton = '", canton, "'"))
  }

  # Build final WHERE clause
  where_sql <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }

  # Final query
  query <- paste0("
    SELECT vacancy_id, company_id, occupation, canton, year, month
    FROM adem.vacancies
    ", where_sql, "
    LIMIT ", limit
  )

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}

#' Get vacancy by ID
#'
#' Returns a vacancy and its required skills based on the given `vacancy_id`.
#'
#' @param vacancy_id Integer. The ID of the vacancy.
#'
#' @return A list with two elements: `vacancy` (data frame with one row) and `skills` (data frame with 0 or more rows).
#' Returns `NULL` if the vacancy is not found.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_vacancy_by_id(123456)
#' }
get_vacancy_by_id <- function(vacancy_id) {
  if (!is.numeric(vacancy_id) || length(vacancy_id) != 1) {
    stop("`vacancy_id` must be a single integer.")
  }

  con <- connect_db()

  # Get main vacancy info
  query_vacancy <- glue::glue_sql("
    SELECT vacancy_id, company_id, occupation, canton, year, month
    FROM adem.vacancies
    WHERE vacancy_id = {vacancy_id}
    LIMIT 1
  ", .con = con)

  vacancy <- DBI::dbGetQuery(con, query_vacancy)

  if (nrow(vacancy) == 0) {
    DBI::dbDisconnect(con)
    return(NULL)
  }

  # Get associated skills
  query_skills <- glue::glue_sql("
    SELECT vs.skill_id, s.skill_label
    FROM adem.vacancy_skills vs
    JOIN adem.skills s ON vs.skill_id = s.skill_id
    WHERE vs.vacancy_id = {vacancy_id}
  ", .con = con)

  skills <- DBI::dbGetQuery(con, query_skills)

  DBI::dbDisconnect(con)

  return(list(
    vacancy = vacancy,
    skills = skills
  ))
}


