
#' Approve interviews as headquarters
#'
#' @param  susor_interview_key A string. interview__key of the interview to be a
#' approved in the server. It can be a vector containing more than one interview__key
#' @param susor_data_reference A tibble. A tibble that includes the interviews to
#' be approved.
#' @param susor_data_comment A string. Approve comment
#' @return Change status of interviews to ApprovedByHeadquarters
#' @examples
#' #define interview__key of interviews to approve by HQ
#'to_approve <- c("00-06-27-10","00-78-23-02" )
#'#approve interviews using 'susor_approve_hq'
#'susor_approve_hq(susor_interview_key = to_approve,
#'                 by = "interview__key",
#'                 susor_data_reference = raw
#'                 )



susor_approve_hq = function(susor_interview_key,
                            susor_approve_comment = NULL,
                            susor_data_reference,
                            ...

) {

  #confirm that key arguments have been passed --------------------------------
  if (missing(susor_data_reference)) {

    stop("susor_data_reference is missing in 'susor_approve_hq'")

  }

  if (missing(susor_interview_key)) {

    stop("susor_interview_key is missing in 'susor_approve_hq'")

  }


  #confirm that the user has run susor_login()---------------------------------
  check_credentials()


  #repeat the approve process to all the interview__keys passed to the function
  for (id in susor_interview_key){

    #fetch interview__id from reference data ------------------------------------

    interview__id = susor_data_reference %>%
      dplyr::filter(interview__key == id) %>%
      pull(interview__id)


    #define APIs ------------------------------------------------------------------
    api_interviews = sprintf("%s/api/v1/interviews/%s/", susor_server, interview__id)
    api_interview_status = paste0(api_interviews, "stats")
    api_approve_hq <- paste0(api_interviews, "hqapprove")


    #check current status of interview -------------------------------------------
    response_status = GET(url = api_interview_status,
                          authenticate(susor_user, susor_password)
    )


    interview_current_status = content(response_status)$Status

    message(paste("Current status of interview", id, ":", interview_current_status))


    #Approve interviews in the server -------------------------------------------
    if (interview_current_status != "ApprovedByHeadquarters") {

      response_approved_hq <- PATCH(url = api_approve_hq,
                                    authenticate(susor_user, susor_password),
                                    body = list(comment = susor_approve_comment)
      )

      #let the user now about the status of the interview
      status_response_approved = response_approved_hq$status_code

      if (status_response_approved == 404) {

        message(paste("Warning. Interview", id, "Not found in", susor_server))

      } else if (status_response_approved == 404) {

        warning(paste("Warning. Interview", id, "is in a status not ready to be approved"))

      } else if(status_response_approved == 200){

        message(paste("Interview", id, "switched from", interview_current_status,
                      "to ApprovedByHeadquarters"))
      }

    }




  }


}









