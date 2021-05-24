
#' FUNCTION TO GET QN ID

susor_get_qnID = function(susor_qn_variable,
                          susor_qn_version,
                       ...){

      quid = susor_questionnaires %>% dplyr::filter(Variable == susor_qn_variable & Version == susor_qn_version) %>% .$QuestionnaireIdentity

      return(quid)


    }


