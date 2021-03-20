#check if credentials have been defined
check_credentials <- function(){

  credentials <- c("susor_server", "susor_password", "susor_user", "susor_dir_downloads")

if (!all(credentials %in% objects(envir = globalenv()))) {

  stop(paste(paste(credentials[!credentials %in% objects()],collapse = ", "),
             "have not been defined! See ?susor_login"))

}


  #create directory to export data --------------------------------------------
  if (!dir.exists(susor_dir_downloads)) {

    dir.create(susor_dir_downloads)

  }

}
