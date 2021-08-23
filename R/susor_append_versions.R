
#' Appends all versions of a questionnaire and saves appended data in \code{susor_dir_raw}
#'
#' First define \code{susor_dir_downloads} and \code{susor_dir_raw} in \code{susor_login()} for
#' \code{susor_append_versions()} to work. Pass the questionnaire variable (\code{susor_qn_variable})
#' to \code{susor_append_versions()} to append all the versions of that questionnaire and save the
#' appended data in \code{susor_dir_raw}.
#'
#' @param  susor_qn_variable A string. The variable name of questionnaire which versions
#' are to be appended
#' @param susor_format A string. Format of the downloaded data c("STATA", "Tabular").
#' Default = "STATA"
#' @param get_diagnostics A boolean. If True, the appended version will include the
#' variables from the interview__diagnostics file and the last action reported in interview__actions
#' @return A directory with the appended file(s) with all the versions of \code{susor_qn_variable} saved
#' in \code{susor_dir_raw}.




susor_append_versions <- function (susor_qn_variable,
                                   susor_format = "STATA",
                                   get_diagostics = T
) {

  #check that rio is loaded
  if (!"rio" %in% (.packages())) {

    stop("Error: load package rio before running susor_append_versions()")

  }

  #define extension based on susor_format
  if (susor_format == "STATA"){
    ext <- ".dta"

  } else if (format == "Tabular") {

    ext <- ".tab"

  }

  #create directory for raw data

  exdir <- file.path(susor_dir_raw, susor_qn_variable)

  if (!dir.exists(susor_dir_raw)) {

    dir.create(susor_dir_raw)
    #create raw directory of the questionnaire

    #dir.create(exdir)

  }

  if (!dir.exists(exdir)) {

    #create raw directory of the questionnaire

    dir.create(exdir)

  }


  # List of all files that have been downloaded
  downloaded_files <- list.files(susor_dir_downloads, pattern = ext, recursive = T, full.names = T)

  # list of downloaded files of this questionnaire
  downloaded_files_questionarie <- downloaded_files[str_detect(downloaded_files,
                                                               file.path(susor_dir_downloads,susor_qn_variable))]


  #read all files stored for susor_qn_variable
  list_files <- lapply(downloaded_files_questionarie, function(x)({

    #'do not return if file is empty
    questionnaire <- import(x)

    if(nrow(questionnaire)>0){

      return(questionnaire)
    }

  }))

  names(list_files) <- downloaded_files_questionarie

  # questionnaires with at least one row
  list_files <- list_files[lengths(list_files) != 0]
  questionnaires  <-  unique(str_extract(names(list_files), "([^\\/]+$)")) #in the name, keep only the file name


  # append versions
  for(q in questionnaires){

    exdir_q <- file.path(exdir,q)
    message(paste("Creating", exdir_q))
    # list with versions of these questoinnaire
    append = list_files[str_detect(names(list_files), q)]

    # append and export
    do.call(plyr::rbind.fill, append) %>%
      export(exdir_q)
  }


  if (get_diagostics) {

    questionnaire_with_diagnostics = susor_get_diagostics(data_directory =  susor_dir_raw,
                                                          susor_qn_variable = susor_qn_variable)

    exfile = file.path(exdir, paste0(susor_qn_variable,".dta"))

    #export file to raw folder
    export(questionnaire_with_diagnostics, exfile)


  }

}

