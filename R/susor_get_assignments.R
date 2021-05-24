#'Downloads assignments from the server
#'@param susor_qn_variable A string with the variable name of the questionnaire
#'@param susor_qn_version A number with the version of the assignment
#'@return A tibbble with the information of the assignment



susor_get_assignments <- function(susor_qn_version,
                                  susor_qn_variable,
                                  susor_limit =40){
  check_credentials()

  api_assignments <- sprintf("%s/api/v1/assignments", susor_server)

  #fetch questionnaire id
  questionnaire_id <- susor_get_qnID(susor_qn_variable, susor_qn_version)
  #message(questionnaire_id)

  ## define a temporary json file
  my_json <- tempfile(fileext = ".json")

  ## get all assignments from the server
  assignments_response <- GET(api_assignments,
                              authenticate(susor_user, susor_password),
                              query = list(limit = susor_limit,
                                           QuestionnaireId = questionnaire_id),
                              write_disk(my_json, overwrite = T)
  )

  ## check status of the response
  status <- assignments_response$status_code
  check_response(status)

  ## convert response into a tibble
  response_json <- jsonlite::fromJSON(my_json)


  tibble_assignments <-tibble(response_json$Assignments)

  return(tibble_assignments)

}


