#' Uploads interviewers to the server
#' @param interviewers_db a tibble with the following variables:
#' The list of interviewers should not exist in the server yet
#' "Role":
#'"UserName":,
#'"FullName":,
#'"PhoneNumber",
#'"Email": ,
#'"Password":,
#'"Supervisor"



susor_post_interviewers <- function(susor_server,
                                    interviewers_db){

  #split the list of interviewers and uploads one by one
  post <- lapply(1:nrow(interviewers_db), function(x){

    row <- interviewers_db[x,]
    inst_list <- as.list(row)

    api_post_users <- glue::glue('{susor_server}/api/v1/users')

    r <- POST(api_post_users,
              body = inst_list,
              authenticate(susor_user,susor_password),
              encode = "json"
    )



  })

  return(post)

}
