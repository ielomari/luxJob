#* @apiTitle luxJob API
#* @apiDescription Access jobs, skills, companies, and learning content securely
#* @securityDefinitions apiKey BearerToken
#* @security BearerToken
#* @apiKeyName Authorization
#* @apiKeyIn header

#* @filter bearer_auth
function(req, res) {
  auth_header <- req$HTTP_AUTHORIZATION
  print(paste("Header received:", auth_header))  # DEBUG

  valid_token <- Sys.getenv("API_TOKEN")
  print(paste("Expected token:", valid_token))  # DEBUG

  if (is.null(auth_header) || !grepl("^Bearer ", auth_header)) {
    res$status <- 401
    return(list(error = "Unauthorized - missing or invalid token format"))
  }

  token <- sub("^Bearer ", "", auth_header)

  if (token != valid_token) {
    res$status <- 401
    return(list(error = "Unauthorized - invalid token"))
  }

  plumber::forward()
}




#* @apiTitle luxJob API
#* @apiDescription Access skills, jobs, companies, learning resources and books from the luxJob R package

# Load luxJob package
library(luxJob)

### --- SKILLS ---

#* Get all skills
#* @security BearerToken
#* @param limit:int Max number of skills to return
#* @get /skills
function(limit = 100) {
  get_skills(limit)
}

#* Get a skill by ID
#* @security BearerToken
#* @param id Skill ID
#* @get /skills/<id>
function(id) {
  get_skill_by_id(id)
}


### --- COMPANIES ---

#* Get all companies
#* @security BearerToken
#* @param limit:int Max number of companies to return
#* @get /companies
function(limit = 100) {
  get_companies(limit)
}

#* Get details for a specific company
#* @security BearerToken
#* @param id:int Company ID
#* @get /companies/<id>
function(id) {
  get_company_details(as.integer(id))
}


### --- VACANCIES ---

#* Get job vacancies (filterable)
#* @security BearerToken
#* @param skill Skill ID
#* @param company:int Company ID
#* @param canton Canton name
#* @param limit:int Max number of results
#* @get /vacancies
function(skill = NULL, company = NULL, canton = NULL, limit = 100) {
  if (!is.null(company)) company <- as.integer(company)
  get_vacancies(skill, company, canton, limit)
}

#* Get vacancy by ID
#* @security BearerToken
#* @param id:int Vacancy ID
#* @get /vacancies/<id>
function(id) {
  get_vacancy_by_id(as.integer(id))
}


### --- LEARNING TRACKS ---

#* Get learning tracks (optionally filter by skill)
#* @security BearerToken
#* @param skill_id Skill ID
#* @get /tracks
function(skill_id = NULL) {
  get_learning_tracks(skill_id)
}

#* Get learning track by ID
#* @security BearerToken
#* @param id:int Track ID
#* @get /tracks/<id>
function(id) {
  get_learning_track_by_id(as.integer(id))
}


### --- BOOKS ---

#* Get books (optionally filter by skill)
#* @security BearerToken
#* @param skill Skill ID
#* @get /books
function(skill = NULL) {
  get_books(skill)
}

#* Get book by ID
#* @security BearerToken
#* @param id:int Book ID
#* @get /books/<id>
function(id) {
  get_book_by_id(as.integer(id))
}


### --- SEARCH LOGGING ---

#* Log a user search query
#* @security BearerToken
#* @param user_id:int User ID
#* @param query Search query string
#* @post /search
function(user_id, query) {
  log_search(as.integer(user_id), query)
}

#* @post /create_user
#* @security BearerToken
#* @param username
#* @param token
#* @param quota
function(username, token, quota = 100) {
  create_user(username, token, as.integer(quota))
}




