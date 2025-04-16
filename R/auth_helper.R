library(plumber)

auth_helper <- function(res, req, FUN, ...) {
  auth_header <- if (!is.null(req$HTTP_AUTHORIZATION)) req$HTTP_AUTHORIZATION else ""
  token <- sub("^Bearer ", "", auth_header)
  valid_token <- Sys.getenv("BEARER_TOKEN")

  if (nchar(valid_token) <= 1 || token != valid_token) {
    res$status <- 401
    return(list(error = "Unauthorized: Invalid or missing token"))
  }

  FUN(...)
}
