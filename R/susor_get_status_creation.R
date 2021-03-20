#' Gets the status of the export process
#'
#' @param apiGenerate A string. susor_server//api/v2/export
#' @return The status of export process



#Function to get status of export in server
susor_get_status_creation = function(apiGenerate, jobID){

  #check export status
  Json_GET = tempfile(fileext = ".json")

  response_file_status =  GET(apiGenerate,
                              authenticate(susor_user, susor_password),
                              encode = "json",
                              write_disk(Json_GET, overwrite = T)
                              )

  file_status_data = jsonlite::fromJSON(Json_GET)
  file_status = file_status_data[file_status_data$JobId == jobID,]$ExportStatus

  return(file_status)

}

