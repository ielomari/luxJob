#' Get learning tracks
#'
#' Returns learning tracks. If a skill ID is provided, only returns tracks teaching that skill.
#'
#' @param skill_id Character. Optional. Skill ID to filter by.
#'
#' @return A data frame with columns: `track_id`, `title`, `description`, `url`.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_learning_tracks()
#'   get_learning_tracks(skill_id = "skill_r")
#' }
get_learning_tracks <- function(skill_id = NULL) {
  con <- connect_db()

  if (is.null(skill_id)) {
    query <- "
      SELECT track_id, title, description, url
      FROM adem.learning_tracks
    "
  } else {
    if (!is.character(skill_id)) stop("`skill_id` must be a character string.")
    query <- paste0("
      SELECT lt.track_id, lt.title, lt.description, lt.url
      FROM adem.learning_tracks lt
      JOIN adem.track_skills ts ON lt.track_id = ts.track_id
      WHERE ts.skill_id = '", skill_id, "'")
  }

  result <- DBI::dbGetQuery(con, query)

  DBI::dbDisconnect(con)

  return(result)
}


#' Get learning track by ID
#'
#' Returns one learning track and all its linked skills.
#'
#' @param track_id Integer. The ID of the learning track.
#'
#' @return A list with two elements: `track` (data frame with one row), and `skills` (data frame with 0 or more rows).
#' Returns `NULL` if the track is not found.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_learning_track_by_id(71)
#' }
get_learning_track_by_id <- function(track_id) {
  if (!is.numeric(track_id) || length(track_id) != 1) {
    stop("`track_id` must be a single integer.")
  }

  con <- connect_db()

  # Get main track
  query_track <- glue::glue_sql("
    SELECT track_id, title, description, url
    FROM adem.learning_tracks
    WHERE track_id = {track_id}
    LIMIT 1
  ", .con = con)

  track <- DBI::dbGetQuery(con, query_track)

  if (nrow(track) == 0) {
    DBI::dbDisconnect(con)
    return(NULL)
  }

  # Get skills linked to the track
  query_skills <- glue::glue_sql("
    SELECT s.skill_id, s.skill_label
    FROM adem.track_skills ts
    JOIN adem.skills s ON ts.skill_id = s.skill_id
    WHERE ts.track_id = {track_id}
  ", .con = con)

  skills <- DBI::dbGetQuery(con, query_skills)

  DBI::dbDisconnect(con)

  return(list(
    track = track,
    skills = skills
  ))
}


