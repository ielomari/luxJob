% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skills.R
\name{get_skill_by_id}
\alias{get_skill_by_id}
\title{Get skill by ID}
\usage{
get_skill_by_id(skill_id)
}
\arguments{
\item{skill_id}{Character. The ID of the skill to retrieve.}
}
\value{
A data frame with one row and columns: \code{skill_id}, \code{skill_label}.
}
\description{
Returns a skill from the \code{adem.skills} table based on the given \code{skill_id}.
}
\examples{
\dontrun{
  get_skill_by_id("skill_python")
}
}
