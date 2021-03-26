# susor
`susor` is work in process and does not intend to substitute any official documentation, software, or other packages.
The main reason of its existence is that after having worked on several household surveys using
[Survey Solutions](https://mysurvey.solutions/en//api/survey-solutions-api/) I realized that I was always copy and pasting old code to repeat similar or identical processes. 

The main motivation is to develop a package that facilitates the work of the *future me*. So, the *future me* can continue improving and growing the code. Nevertheless, it would be great if others feel that `susor` is useful for their needs. Please feel free to use it **but be aware that I am not responsible for any bug or for its incorrect use.** 

Also, feel free to collaborate on making `susor` easier and more efficient to use.

## Thus far `susor` does the following:

* Logs in the user on the server with `susor_login()`,
* Gets the list of imported questionnaires with `susor_login()` or with `susor_get_questionnaires()`,
* Generates and downloads files from the server to your local drive with `susor_export_file()`, and
* Appends different versions of the same questionnaire with `susor_append_versions()`
* Approves interviews as headquarters with `susor_approve_hq()`


Each of the above functions is documented. 


## The expected workflow of `susor` is like this

1. ALWAYS START BY DEFINING YOUR API CREDENTIALS WITH `susor_login()`. All the other functions won't work without this. You must call `susor_login()` once you start R or everytime you empty your global environment. You must specify all the paramenters within `susor_login()`, including `susor_dir_downloads` and `susor_dir_raw`. You'll use them later. 

2. Look at the tibble `susor_questionnaires` that was generated by `susor_login()`.In this tibble you'll find all the parameters to generate and download files with `susor_export_file()`.

3. Pass the questionnaire variable and version `susor_qn_variable` and `susor_qn_version` to `susor_export_file()`, the result of this will be a folder with the unzipped version of that file in the directory (`susor_dir_download`) that you defined in step 1.

4. Pass `susor_qn_variable` to `susor_append_versions()` to append all the versions of the same questionnaire.
A directory with the appended file(s) will be saved in `susor_dir_raw`.

5. After having checked the quality of each interview, pass a vector `susor_interview_key` to `susor_approve_hq` to approve these interviews in the server.

## Basic example


```{r eval=FALSE}
#install and load susor
devtools::install_github("araupontones/susor")
library(susor)

library(httr)
library(jsonlite)
library(lubridate) 
library(rio) 
library(tidyverse)


#define path to surveys folder

survey_dir = "C:/Users/MARADONA/Dropbox/survey/Data Management"

# define my credentials, check that they are ok, and retrieve susor_questionnaires 
# a tibble with the details of the questionnaires imported in Survey Solutions

susor_login(susor_server = "http://my.golsurveys.com",
            susor_user = "maradona",
            susor_password = "pelusa123",
            susor_dir_downloads = file.path(survey_dir, "downloads"),
            susor_dir_raw = file.path(survey_dir, "raw")
                  
)

#generate and export file to susor_dir_download which is defined in susor_login()

susor_export_file(susor_qn_variable = "best_goals",
                  susor_qn_version = 2)


#generate and export file of another version of the same questionnaire

susor_export_file(susor_qn_variable = "best_goals",
                  susor_qn_version = 3)
                  

#append versions and save in susor_dir_raw. Remember that susor_dir_raw is defined
#by susor_login()

susor_append_versions(susor_qn_variable = "best_goals",
                      susor_format = "STATA"
                      )

#Import raw data --------------------------------------------------------------

df_raw = import(file.path(susor_dir_raw, "best_goals.dta")

#Approve interviews as headquarters

#define interview__key of interviews to approve by HQ
to_approve <- c("00-06-27-10","00-78-23-02" )

#approve interviews using 'susor_approve_hq'
susor_approve_hq(susor_interview_key = to_approve,
                 by = "interview__key",
                 susor_data_reference = raw
                 )


```


## What I am working on (in next versions)

I am working on functions to ease the use of interview__diagnostics and interview__actions so they can be included in the regular checks during a data collection

The link and chart below were used to develop the code that interacts with the server via the API. Both were obtained from the support site of Survey Silutions:
https://demo.mysurvey.solutions/apidocs/index.html

![](https://raw.githubusercontent.com/araupontones/Survey_Solutions_download/main/API_flow.png)

