#alert the user with any non sucesfull response from the server

#status is defined in every query

check_response <- function(status){


  if (status == 204) { #no content

    stop(paste("Status:", status, "The request has been successfully processed, but is not returning any content"))

  } else if (status == 401) { #bad credentials (client error)

    stop(paste("Status:", status, "The request was a legal request, but the server",
               "is refusing to respond to it.",
               "Check susor_user and susor_password in susor_login()"))


  } else if (status == 404) { #bad server (client error)

    stop(paste("Status:", status, "The requested page could not be found but may be available again in the future",
               "Check the definition of susor_server in susor_login()"))

  } else if (status >= 500 & status < 511) { #server side error


    stop(paste("Status:", status, "Server error, contact Survey Solutions support"))

  }

}
