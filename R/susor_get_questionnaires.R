
#' Get details from questionnaires imported in the server
#'
#' Before running \code{susor_get_questionnaires()}, provide your server credentials
#' by running \code{susor_login()}. \code{susor_get_questionnaires()} gets
#' the list of imported questionnaires in the server and transforms
#' this list into a tibble that is saved in your global environment as
#' \code{susor_questionnaires}.

#' @param susor_limit A number. Limit number of returned rows. Max allowed
#' value is 40. Default=40
#' @param susor_offset Page number starting from 1. Default = 1
#' @return A tibble named \code{susor_questionnaires} saved in your global
#' environment
#' @examples
#' susor_get_questionnaires(susor_limit = 20)




susor_get_questionnaires = function(susor_limit = 40,
                                    susor_offset = 1,
                                    ...){

  #check if needed packages are loaded
  pks <- c("httr", "dplyr", "lubridate")

  #stop if any of the needed packages is not loaded
  if (!all(pks %in% (.packages()))) {

    stop(paste("Error: package(s):",pks[!pks %in% (.packages())], "required!"))

  }


  #check if credentials have been defined
  credentials <- c("susor_server", "susor_password", "susor_user")

  if (!all(credentials %in% objects(envir = globalenv()))) {

    stop(paste(paste(credentials[!credentials %in% objects()],collapse = ", "),
               "have not been defined! See ?susor_login"))

  }



  ## define url to contact the server
  api_questoinnaires <- sprintf("%s/api/v1/questionnaires", susor_server)

  ## define a temporary json file
  my_json <- tempfile(fileext = ".json")

  ## get all questionnaires from the server
  qn_details <- GET(api_questoinnaires,
                    authenticate(susor_user, susor_password),
                    query = list(limit = susor_limit,
                                 offset = susor_offset),
                    write_disk(my_json, overwrite = T)
  )

  ## check status of the response
  status <- qn_details$status_code
  check_response(status)

  ## convert response into a tibble
  response_json <- jsonlite::fromJSON(my_json)


  tibble_qndetails <-tibble(response_json$Questionnaires) %>%
    mutate(LastEntryDate = ymd_hms(str_replace(LastEntryDate, "T", " ")))

  #save questionnaire details in the global environment
  assign("susor_questionnaires", tibble_qndetails, .GlobalEnv)

  message(paste("Your credentials are correct! A tibble called",
                "susor_questionnaires has been also saved in the global environment.",
                "It cointains all the information of the questionaires imported in",
                susor_server))




}
