% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/susor_post_interviewers.R
\name{susor_post_interviewers}
\alias{susor_post_interviewers}
\title{Uploads interviewers to the server}
\usage{
susor_post_interviewers(susor_server, interviewers_db)
}
\arguments{
\item{interviewers_db}{a tibble with the following variables:
The list of interviewers should not exist in the server yet
"Role":
"UserName":,
"FullName":,
"PhoneNumber",
"Email": ,
"Password":,
"Supervisor"}
}
\description{
Uploads interviewers to the server
}
