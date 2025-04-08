#' Get books
#'
#' Returns recommended books. You can optionally filter by a skill.
#'
#' @param skill Character. Optional. Filter books by skill ID.
#'
#' @return A data frame with columns: `book_id`, `title`, `author`, `skill_id`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_books()
#'   get_books(skill = "skill_python")
#' }
get_books <- function(skill = NULL) {
  con <- connect_db()

  if (is.null(skill)) {
    query <- "
      SELECT book_id, title, author, skill_id
      FROM adem.book_recommendations
    "
  } else {
    if (!is.character(skill)) stop("`skill` must be a character string.")
    query <- paste0("
      SELECT book_id, title, author, skill_id
      FROM adem.book_recommendations
      WHERE skill_id = '", skill, "'")
  }

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}


#' Get book by ID
#'
#' Returns one book recommendation based on the given `book_id`.
#'
#' @param book_id Integer. The ID of the book to retrieve.
#'
#' @return A data frame with one row and columns: `book_id`, `title`, `author`, `skill_id`.
#' Returns `NULL` if not found.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_book_by_id(101)
#' }
get_book_by_id <- function(book_id) {
  if (!is.numeric(book_id) || length(book_id) != 1) {
    stop("`book_id` must be a single integer.")
  }

  con <- connect_db()

  query <- glue::glue_sql("
    SELECT book_id, title, author, skill_id
    FROM adem.book_recommendations
    WHERE book_id = {book_id}
    LIMIT 1
  ", .con = con)

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  if (nrow(result) == 0) return(NULL)

  return(result)
}

