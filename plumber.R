# plumber.R

#* @apiTitle luxJob API
#* @apiDescription Access jobs, skills, companies, and learning content securely
#* @apiVersion 1.0.0

#* @securityDefinitions apiKey BearerAuth
#* @apiKeyName Authorization
#* @apiKeyIn header

# Load your package
library(luxJob)
library(DBI)
library(glue)


### --- SKILLS ---

#* Get all skills
#* @security BearerAuth
#* @param limit:int Max number of skills to return
#* @get /skills
function(req, res, limit = 100) {
  auth_helper(res, req, function(limit) {
    get_skills(limit)
  }, limit = limit)
}

#* Get a skill by ID
#* @security BearerAuth
#* @param id Skill ID
#* @get /skills/<id>
function(req, res, id) {
  auth_helper(res, req, function(id) {
    get_skill_by_id(id)
  }, id = id)
}

### --- COMPANIES ---

#* Get all companies
#* @security BearerAuth
#* @param limit:int Max number of companies to return
#* @get /companies
function(req, res, limit = 100) {
  auth_helper(res, req, function(limit) {
    get_companies(limit)
  }, limit = limit)
}

#* Get details for a specific company
#* @security BearerAuth
#* @param id:int Company ID
#* @get /companies/<id>
function(req, res, id) {
  auth_helper(res, req, function(id) {
    get_company_details(as.integer(id))
  }, id = id)
}

### --- VACANCIES ---

#* Get job vacancies
#* @security BearerAuth
#* @param skill Skill ID
#* @param company:int Company ID
#* @param canton Canton name
#* @param limit:int Max number of results
#* @get /vacancies
function(req, res, skill = NULL, company = NULL, canton = NULL, limit = 100) {
  auth_helper(res, req, function(skill, company, canton, limit) {
    if (!is.null(company)) company <- as.integer(company)
    get_vacancies(skill, company, canton, limit)
  }, skill = skill, company = company, canton = canton, limit = limit)
}

#* Get vacancy by ID
#* @security BearerAuth
#* @param id:int Vacancy ID
#* @get /vacancies/<id>
function(req, res, id) {
  auth_helper(res, req, function(id) {
    get_vacancy_by_id(as.integer(id))
  }, id = id)
}

### --- LEARNING TRACKS ---

#* Get learning tracks (optionally filter by skill)
#* @security BearerAuth
#* @param skill_id Skill ID
#* @get /tracks
function(req, res, skill_id = NULL) {
  auth_helper(res, req, function(skill_id) {
    get_learning_tracks(skill_id)
  }, skill_id = skill_id)
}

#* Get learning track by ID
#* @security BearerAuth
#* @param id:int Track ID
#* @get /tracks/<id>
function(req, res, id) {
  auth_helper(res, req, function(id) {
    get_learning_track_by_id(as.integer(id))
  }, id = id)
}

### --- BOOKS ---

#* Get books
#* @security BearerAuth
#* @param skill Skill ID
#* @get /books
function(req, res, skill = NULL) {
  auth_helper(res, req, function(skill) {
    get_books(skill)
  }, skill = skill)
}

#* Get book by ID
#* @security BearerAuth
#* @param id:int Book ID
#* @get /books/<id>
function(req, res, id) {
  auth_helper(res, req, function(id) {
    get_book_by_id(as.integer(id))
  }, id = id)
}

### --- SEARCH LOGGING ---

#* Log a user search query
#* @security BearerAuth
#* @param user_id:int User ID
#* @param query Search query string
#* @post /search
function(req, res, user_id, query) {
  auth_helper(res, req, function(user_id, query) {
    log_search(as.integer(user_id), query)
  }, user_id = user_id, query = query)
}

### --- CREATE USER ---

#* Create a new API user
#* @security BearerAuth
#* @param username
#* @param token
#* @param quota
#* @post /create_user
function(req, res, username, token, quota = 100) {
  auth_helper(res, req, function(username, token, quota) {
    create_user(username, token, as.integer(quota))
  }, username = username, token = token, quota = quota)
}
