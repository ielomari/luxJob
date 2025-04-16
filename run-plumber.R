# run_plumber.R

library(plumber)

# ------------------------------------------------------------------------------
# Define Bearer Token Security for Swagger UI
# ------------------------------------------------------------------------------

add_bearer_auth <- function(api, paths = NULL) {
  # Declare the Bearer token scheme in OpenAPI
  api$components <- list(
    securitySchemes = list(
      BearerAuth = list(
        type = "http",
        scheme = "bearer",
        description = "Enter a Bearer token like: Bearer 123456"
      )
    )
  )

  # Apply the security scheme to all paths if not explicitly passed
  if (is.null(paths)) paths <- names(api$paths)

  for (path in paths) {
    methods <- names(api$paths[[path]])
    for (method in intersect(methods, c("get", "post", "put", "delete", "head"))) {
      api$paths[[path]][[method]] <- c(
        api$paths[[path]][[method]],
        list(security = list(list(BearerAuth = list())))
      )
    }
  }

  return(api)
}

# ------------------------------------------------------------------------------
# Start API
# ------------------------------------------------------------------------------

# Load your token checker helper
source("R/auth_helper.R")

# Load the plumber file
pr("plumber.R") |>
  pr_set_api_spec(add_bearer_auth) |>
  pr_run(port = 8000, host = "0.0.0.0")
