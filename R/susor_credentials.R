
#library(httr)



#'Define Server credentials
#'
#'\code{susor_credentials} saves all your API credentials in the global
#'environment. This function must be run before any other function of this
#'package \code{susor}.
#' @param susor_server A string. URL of your Survey Solution's server.
#' @param  susor_user A string. It should be the API user defined in
#' Survey Solutions Server.
#' @param susor_password A string. Password of the API user.
#' @param  susor_dir_downloads A string. Path to the directory where the
#' data exported from the server is to be stored in your local drive.
#' Default = 'downloads'.
#' @param susor_dir_raw A string. Path to the directory where the raw data will
#' be stored in your local drive.Default = 'raw.
#' @return  All the parameters provided to the function (\code{susor_server},
#' \code{susor_user}, \code{susor_password}, \code{susor_dir_downloads}, and
#' \code{susor_dir_raw} will be saved in your global environment so can be used
#' in your workflow). Additionally, A tibble named \code{susor_questionnaires}
#' will be saved in your global environment. View this tibble to get: Questionnaire ID,
#' etc.
#' @examples
#' susor_credentials(susor_server = "http://my.muvasurveys.com",
#'                   susor_user = "Diego_Maradona",
#'                   susor_password = "Pelusa123",
#'                   susor_dir_downloads = file.path("Dropbox", "downloads"),
#'                   susor_dir_raw = file.path("Dropbox", "raw")
#')

#'
#'
#'

susor_credentials = function(susor_server,
                             susor_user,
                             susor_password,
                             susor_dir_downloads = 'downloads',
                             susor_dir_raw = 'raw',
                             ...){

  names <- c("susor_server", "susor_user", "susor_password", "susor_dir_downloads", "susor_dir_raw")
  values <-  c(susor_server,susor_user, susor_password, susor_dir_downloads, susor_dir_raw)

  #save parameters in global directory
  invisible(
  lapply(seq_along(1:length(names)),
         function(x){

           message(paste(names[[x]], "stored in global environment"))
           assign(names[[x]], value = values[[x]], envir = .GlobalEnv)
         })
  )


#get questionnaires
 susor_get_questionnaires()


}









