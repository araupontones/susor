#' Gets interview diagnostics and actions into the main questionnaire
#'
#' @param data_directory A directory. The directory where the data is stored. This
#' directory must include the following files: interview__diagnostics.dta,
#' interview__actions.dta and the questionnaire for which the indicators will be attached
#' @param susor_qn_variable The variable name of questionnaire which versions
#' are to be appended
#' @return A tibble of the questionnaire (susor_qn_variable) with the variables
#' from interview__diagnostics, the last action of the interview, and the url
#' link to the interview

susor_get_diagostics <- function(data_directory ,
                                 susor_qn_variable
){

  #define target directories --------------------------------------------------
  target_dir = file.path(data_directory, susor_qn_variable)
  target_file = paste0(susor_qn_variable, ".dta")


  #import questionnare main questionaire --------------------------------------
  questionnaire = import(file.path(target_dir, target_file))


  #import diagonistics --------------------------------------------------------
  diagnostics = import(file.path(target_dir,"interview__diagnostics.dta" ))

  #import actions and keep only the last action -----------------------------
  actions = import(file.path(target_dir, "interview__actions.dta")) %>%
    #get labels from stata
    mutate(across(all_of("action"), susor_get_stata_labels),
           date = ymd(date)) %>%
    dplyr::filter(action != "Paused") %>%
    arrange(interview__key, date, time) %>%
    group_by(interview__key) %>%
    dplyr::filter(row_number() == n()) %>% #keep interview last action
    rename(last_action = action) %>%
    select(interview__key, interview__id, date, time, last_action)


  #join questionnaire with actions and diagnostics -----------------------------
  output_file = questionnaire %>%
    select(-interview__status) %>%
    left_join(diagnostics, by= c("interview__key", "interview__id")) %>% #get interview diagnostics
    mutate(link = glue::glue("{susor_server}/Interview/Review/{interview__id}"),
           interview__duration = hms(str_remove(interview__duration, "[0-9]{1,}\\.")),
           interview__duration = round(period_to_seconds(interview__duration) / 60, digits = 0)
    ) %>%
    left_join(actions, by=c("interview__key", "interview__id")) %>%#get interview last action
  #this variable is giving errors. check why!
    select(-assignment__id)


  return(output_file)

}

