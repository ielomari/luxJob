#' Get companies
#'
#' Returns a list of companies from the `adem.companies` table, limited by `limit`.
#'
#' @param limit Integer. Maximum number of companies to return. Default is 100.
#'
#' @return A data frame with columns: `company_id`, `name`, `sector`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_companies(limit = 50)
#' }
get_companies <- function(limit = 100) {
  if (!is.numeric(limit) || limit <= 0) {
    stop("`limit` must be a positive integer.")
  }

  con <- connect_db()

  query <- glue::glue_sql("
    SELECT company_id, name, sector
    FROM adem.companies
    LIMIT {limit}
  ", .con = con)

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}


#' Get company details
#'
#' Returns company information and all its vacancies.
#'
#' @param company_id Integer. The ID of the company.
#'
#' @return A list with two elements: `company` (data frame with one row) and `vacancies` (data frame with 0 or more rows).
#' Returns `NULL` if the company is not found.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_company_details(42)
#' }
get_company_details <- function(company_id) {
  if (!is.numeric(company_id) || length(company_id) != 1) {
    stop("`company_id` must be a single integer.")
  }

  con <- connect_db()

  # Get company info
  query_company <- glue::glue_sql("
    SELECT company_id, name, sector
    FROM adem.companies
    WHERE company_id = {company_id}
    LIMIT 1
  ", .con = con)

  company_info <- DBI::dbGetQuery(con, query_company)

  if (nrow(company_info) == 0) {
    DBI::dbDisconnect(con)
    return(NULL)
  }

  # Get vacancies
  query_vacancies <- glue::glue_sql("
    SELECT vacancy_id, occupation, canton, year, month
    FROM adem.vacancies
    WHERE company_id = {company_id}
  ", .con = con)

  vacancies <- DBI::dbGetQuery(con, query_vacancies)

  DBI::dbDisconnect(con)

  return(list(
    company = company_info,
    vacancies = vacancies
  ))
}
