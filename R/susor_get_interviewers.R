#'Downloads the list of interviewers for a given Supervisor
#' @param supervisor Name of supervisor
#' @result a tibble with the interviewers of the supervisor




susor_get_interviewers <- function(susor_server,
                                   susor_user,
                                   susor_password,
                                   supervisor = "Supervisor"){

  #Get the ID of the supervisor ----------------------------------------------
  api_supervisor <- sprintf("%s/api/v1/supervisors", susor_server)

  #temp file
  my_json <- tempfile(fileext = ".json")


  r_supervisor <- GET(api_supervisor,
                      authenticate(susor_user, susor_password),
                      write_disk(my_json)
  )

  #Check response
  check_response(r_supervisor$status_code)


  response_json <- jsonlite::fromJSON(my_json)
  #get the table with supervisors information
  supervisors <- response_json$Users
  supervisor_id <- supervisors$UserId[supervisors$UserName == supervisor]

  #let user know if this supervisor does not exist
  if(length(supervisor_id) == 0){


    stop(paste("supervisor",supervisor, "does not exist in", susor_server))

    #download interviewers team of this supervisor ---------------------------------------------------
  } else {

    cli::cli_alert_success(paste("ID of", supervisor, "has been created. Downloading its interviewers"))

    api_interviewers <- glue::glue("{api_supervisor}/{supervisor_id}/interviewers")

    my_json_ints <- tempfile(fileext = ".json")


    r_interviewers <- GET(api_interviewers,
                          authenticate(susor_user, susor_password),
                          write_disk(my_json_ints, overwrite = T)
    )

    #check status
    check_response(r_interviewers$status_code)

    #continue if not error
    response_json_ints <- jsonlite::fromJSON(my_json_ints)
    interviewers <- response_json_ints$Users

    cli::cli_alert_success(paste("Interviewers' team of", supervisor, "has been downloaded"))

    return(interviewers)


  }


}

