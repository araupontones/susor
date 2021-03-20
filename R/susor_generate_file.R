#' Start file creation in Survey Solution's server
#'
#' As for any other function of \code{susor}, \code{susor_generate_file()} requires
#' that you first define your credentials with \code{susor_login()}.
#' \code{susor_generate_file()} will tell the server to start the creation of the file.
#' @param susor_quid A string. Questionnaire Identity. You can find it in
#' the tibble \code{susor_questionnaires} that is created by \code{susor_login()}
#' See ?\code{susor_login}
#' @param susor_format A string to define the format of the file c("STATA", "Tabular").
#' Default = "STATA"
#' @param susor_interview_status A string. To define the status of interviews to download
#' c( "All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor",
#'  "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters").
#'  Default = "ALL"
#'


susor_generate_file <- function(susor_quid,
                                susor_interview_status = "ALL",
                                susor_format = "STATA",
                                ...){

  #check that credentials have been defined in susor_login
  check_credentials()

  #'define query to start the creation process in the server
  apiGenerate <- sprintf("%s/api/v2/export/", susor_server)

  #'create .json file temporarly to store the server's response
  Json_POST <- tempfile(fileext = ".json")

  #create file in server
  response <-  POST(apiGenerate,
                   authenticate(susor_user, susor_password),
                   body = list(ExportType= susor_format,
                               QuestionnaireId	 = susor_quid,
                               InterviewStatus = susor_interview_status
                   ),
                   encode = "json",
                   write_disk(Json_POST, overwrite = T))



  response_info <- jsonlite::fromJSON(Json_POST)

  #checks and messages to use
  status <- response$status_code
  jobID <- response_info$JobId
  check_response(status)


  #return important information of the export process to be used in the export
  return(
    list(details = jsonlite::fromJSON(Json_POST),
         response = response,
         jobID = jobID,
         apiGenerate = apiGenerate)
  )


  #check status of the query to let the user know about any issue
  if (status == 201){

    message(paste("Export has started, its",
                  "JobID is:", jobID)
    )
  } else if (status == 400){

    stop(paste("Status:", status,
               "susor_quid is malformed")
    )
  } else if (status == 404) {

    stop(paste("Status:", status,
               "susor_quid was not found")
    )

  }


  #status of the export process
  file_status <- susor_get_status_creation(apiGenerate, jobID)

  #wait until the status is Completed so we can start exporting
  while (file_status != "Completed") {
    Sys.sleep(1)
    message("Creating file in Server")
    file_status <- susor_get_status_creation(apiGenerate, jobID)

  }

  #let the user know that everything is OK :)
  message("File creation has been completed!")


}
