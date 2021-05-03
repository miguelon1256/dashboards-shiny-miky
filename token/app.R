
library("httr")
library("jsonlite")
library("dplyr")
library("readr")

##This code downloads data from kf.nexion-dev.tk
##Note: there is the api for the kobo server - where you have the forms and such: https://kf.nexion-dev.tk. 
##And there is the old_API: https://kc.nexion-dev.tk

##Original source: https://github.com/nzfarhad/koboAPI/blob/master/R/koboAPI.R

##You can get info on forms from API but data only from old API, the IDs across those of forms are different. 
##so the process is: get forms first from new API - retreive new ID and old ID
##Match with old ID and then load the data. 


##Define url where our data is 
api <- "https://kf.nexion-dev.tk"
##the old API is needed for the actual data. 
old_api <- "https://kc.nexion-dev.tk"

##get form ID form website - this is the new form_id
form_id <- "aG43m9VaJENFVng7zgeqJR"
formid <- "aG43m9VaJENFVng7zgeqJR"

url_form <- paste0(api, "/assets/", form_id, "/")
kobo_token <- Sys.getenv('DASHBOARDS_KOBO_TOKEN')

##Full URL for the new API
url_form <- paste0(api, "/assets/", form_id, "/")

##-------------------------------------------------------------------------------------------------------------------
##Get the forms - as in the questionnaires
raw_form <- GET(url_form, add_headers(Authorization = paste("Token ", kobo_token)), progress())

##check code
print(paste0("Status Code: ",raw_form$status_code))

##Format forms
raw_form_text <- content(raw_form, "text", encoding = "UTF-8")
raw_form_text_json <- fromJSON(raw_form_text)
languages <- as.vector(raw_form_text_json$content$translations)
languages_labels <- paste0("label::", languages)

##survey <- as.data.frame(raw_form_text_json$content$survey)%>%
##  purrr::modify_depth(2, replace_x, replacement = c(rep("NA", length(languages_labels))))%>%
##  dplyr::mutate(label = purrr::map(label, setNames, languages_labels))%>%
##  unnest_wider(label)

##Download all available forms from new api
download_forms_all <- GET(paste0(api, "/assets/"), add_headers(Authorization = paste("Token ", kobo_token)), progress())
download_forms_all <- download_forms_all%>%
  content("text", encoding = "UTF-8")%>%
  fromJSON()

##The below gives a list of the questionnaires in the dataset
download_forms_all_df <-  as.data.frame(download_forms_all$results)%>%
  filter(has_deployment == TRUE)%>%
  select(name, uid, date_created, date_modified, deployment__submission_count)


##The below gets the list of forms from the old api
url_old_form <- paste0(old_api, "/api/v1/data")
download_forms_all_old <- GET(url_old_form, add_headers(Authorization = paste("Token ", kobo_token)), progress())
download_forms_all_old <- content(download_forms_all_old, "text", encoding = "UTF-8")
download_forms_all_old <- fromJSON(download_forms_all_old)
download_forms_all_old <- as.data.frame(download_forms_all_old)%>%
  mutate(old_id = id)%>%
  select(old_id, id_string)

##merge forms across old and new api
download_forms_all_df <- left_join(download_forms_all_df, download_forms_all_old, by = c("uid" = "id_string"))

##this gets the old ID from old API
old_id <- as.character(download_forms_all_df%>%filter(uid == form_id)%>%select(old_id))

##using the old ID get data from old API
url_data <- paste0(old_api, "/api/v1/data/",old_id, ".csv")

raw_data <- GET(url_data, add_headers(Authorization = paste("Token ", kobo_token)), progress())

print(raw_data)

##bit of data formatting
raw_data <- content(raw_data, "raw", encoding = "UTF-8")
raw_data <- read_csv(raw_data, na = c("", "NA", "n/a"))
##raw_data <- remove_GroupeHeaders(raw_data, formid, pw, u, api)
##raw_data <- addStartCols_sm(raw_data, form )














