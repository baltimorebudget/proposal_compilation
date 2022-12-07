library(bbmR)
library(httr)
library(rio)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(knitr)
library(kableExtra)

analysts <- import("G:/Analyst Folders/Lillian/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  mutate(`Agency ID` = as.character(`Agency ID`)) %>%
  select(-(Operational:Notes))

line_item <- import("G:/Fiscal Years/Fiscal 2021/Planning Year/3. Proposal/1. Line Item Reports/line_items_2020-02-03.xlsx") %>%
  mutate_at(vars(ends_with("ID")), as.character) %>%
  rename(`Service ID` = `Program ID`, `Service Name` = `Program Name`) %>%
  filter(`Fund Name` %in% c("General", "Internal Service", "Parking Management"),
         !`Service ID` %in% c(741, 605)) %>%
  left_join(analysts) 

reduction_targets <- line_item %>%
  group_by(`Agency Name`,`Agency Name - Cleaned`, `Fund Name`) %>%
  summarize_at(vars("FY21 CLS"), sum, na.rm = TRUE) %>%
  mutate(`5% Reduction` = round(`FY21 CLS` * .05),
         ) %>%
  mutate_if(is.numeric, scales::comma) %>%
  ungroup()

api <- list(
  key = Sys.getenv("API_KEY"),
  endpoint = oauth_endpoint(
    authorize = "https://www.formstack.com/api/v2/oauth2/authorize?client_id=21697&redirect_uri=http%3A%2F%2Fbbmr.baltimorecity.gov&response_type=code",
    access = "https://www.formstack.com/api/v2/oauth2/token"),
  app = oauth_app(
    "R",
    key = Sys.getenv("OAUTH_KEY"),
    secret = Sys.getenv("OAUTH_SECRET"),
    redirect_uri = Sys.getenv("OAUTH_REDIRECT"))
)

forms <- GET("https://www.formstack.com/api/v2/form.json",
     add_headers(Authorization = paste("Bearer", api$key, sep = " ")),
     encode = "json") %>% 
  content() %>% 
  toJSON() %>%
  fromJSON() %>%
  extract2("forms") %>%
  select(form = id, agency = name, submissions) %>%
  filter(grepl("Fiscal 2021 Budget Proposal", agency),
         submissions > 0) %>%
  mutate(form = as.character(form))

submissions <- map(forms$form, function(x) {
  GET(paste("https://www.formstack.com/api/v2/form/", x, "/submission.json"),
             add_headers(Authorization = paste("Bearer", api$key, sep = " ")),
             body = list(id = x),
             encode = "json") %>% 
    content() %>% 
    toJSON() %>%
    fromJSON() %>%
    extract2("submissions") %>%
    select(submission = id)
  }
) %>%
  set_names(forms$form) %>%
  bind_rows(.id = "form") %>%
  mutate(submission = as.character(submission))


data <- map(submissions$submission, function(x) {
  
  raw <- GET(paste0("https://www.formstack.com/api/v2/submission/", x, ".json"),
                  add_headers(Authorization = paste("Bearer", api$key, sep = " ")),
                  body = list(id = x),
                  encode = "json") %>% 
      content() %>% 
      toJSON() %>%
      fromJSON()
  
  df <- raw %>%
    extract2("data") %>%
    mutate(date = raw %>% extract2("timestamp"))
  
  return(df)
  }
) %>%
  set_names(submissions$submission) %>%
  bind_rows(.id = "submission")

proposals <- forms %>%
  mutate(`Agency Name` = gsub("Fiscal 2021 Budget Proposal - Agency Head - ", "", agency),
         `Agency Name` = ifelse(`Agency Name` == "M-R: Office of Homeless Services",
                                "M-R: Office of Human Services", `Agency Name`)) %>%
  select(-submissions, -agency) %>%
  left_join(submissions) %>%
  left_join(data) %>%
  group_by(`Agency Name`, submission) %>%
  mutate(field = row_number(),
         field = case_when(field == 1 ~ "Priority ranking",
                           field == 2 ~ "Ranking explanation",
                           field == 3~ "2-Year budget plan"),
         value = as.character(value)) %>%
  left_join(analysts %>%
              mutate(`Agency Name` = 
                       ifelse(`Agency Name` == 
                                "M-R: Office of Information & Technology", 
                              "M-R: Office of Information and Technology",
                              `Agency Name`))) %>%
  group_by(`Agency Name`, add = FALSE) %>%
  filter(date == max(date, na.rm = TRUE))

ranking <- proposals %>%
  filter(field == "Priority ranking") %>%
  select(-field) %>%
  extract2("value") %>%
  str_split("\n", simplify = TRUE) %>%
  as_tibble() %>%
  gather() %>%
  separate(value, c("service", "Rank"), sep = " = ") %>%
  separate(service, c("Service ID", "Service Name"), sep = ": ") %>%
  mutate(`Service ID` = str_trim(gsub("Program", "", `Service ID`))) %>%
  distinct() %>%
  left_join(line_item %>%
              distinct(`Service ID`, `Agency Name - Cleaned`)) %>%
  mutate(`Agency Name - Cleaned` = ifelse(
    `Service ID` == 893, "MR Office of Human Services", `Agency Name - Cleaned`))
  
activities <- ranking %>%
  group_by(`Service ID`, `Agency Name - Cleaned`) %>%
  count() %>%
  filter(n > 1) 

service_ranking <- ranking %>%
  filter(!`Agency Name - Cleaned` %in% activities$`Agency Name - Cleaned`) %>%
  select(-key, -`Service Name`)

activity_ranking <- ranking %>%
  filter(`Agency Name - Cleaned` %in% activities$`Agency Name - Cleaned`) %>%
  mutate(`Service Name` = ifelse(
    !grepl("-", `Service Name`), 
    paste(`Service Name`, " - ", `Service Name`), 
    `Service Name`)) %>%
  separate(`Service Name`, c("Service Name", "Activity Name"), sep = " - ") %>%
  filter(!is.na(`Service ID`)) %>%
  select(-key, -`Service Name`) %>%
  distinct() %>%
  left_join(line_item %>%
              distinct(`Service ID`, `Activity Name`, `Activity ID`)) %>%
  mutate(`Activity ID` = ifelse(is.na(`Activity ID`), 1, `Activity ID`))

service_ranking <- line_item %>%
  group_by(`Agency Name`, `Agency Name - Cleaned`, `Service ID`, `Service Name`) %>%
  summarize_at(vars("FY20 Adopted", "FY21 CLS"), sum, na.rm = TRUE) %>%
  ungroup() %>%
  # this service was moved from DPW to DGS; manual fix
  mutate_at(vars(starts_with("Agency Name")),
            funs(ifelse(`Service ID` == 730, "General Services", .))) %>%
  left_join(service_ranking %>% select(-`Agency Name - Cleaned`), by = "Service ID") %>%
  distinct() %>%
  arrange(as.numeric(Rank)) %>%
  mutate_if(is.numeric, scales::comma)

activity_ranking <- line_item %>%
  group_by(`Agency Name`, `Agency Name - Cleaned`, `Service ID`, `Service Name`, 
           `Activity ID`, `Activity Name`) %>%
  summarize_at(vars("FY20 Adopted", "FY21 CLS"), sum, na.rm = TRUE) %>%
  filter(`Service ID` %in% activity_ranking$`Service ID`) %>%
  left_join(activity_ranking %>% select(-`Agency Name - Cleaned`, -`Activity Name`),
            by = c("Service ID", "Activity ID")) %>%
  distinct() %>%
  arrange(as.numeric(Rank)) %>%
  ungroup() %>%
  mutate_if(is.numeric, scales::comma)

rm(ranking)

sc_pms <- readRDS("G:/Analyst Folders/Lillian/budget_book_2_agency_detail/outputs/Scorecard PMs.Rds")

sc_pms %<>%
  select(-`Actual 2020`, -ends_with("2015"), -ends_with("2016"))

sc_questions <- readRDS("G:/Analyst Folders/Lillian/budget_book_2_agency_detail/outputs/Scorecard Questions.Rds") %>%
  mutate_at(vars(starts_with("Question")), funs(gsub("â€™s", "'", ., fixed = TRUE)))

sc_service_desc <- readRDS("G:/Analyst Folders/Lillian/budget_book_2_agency_detail/outputs/Scorecard Service Descriptions.Rds")

make_proposal_pdf <- function(x, rmd, file_name_end, dir) {
  
  tryCatch({
    
    df <- proposals %>%
      filter(`Agency Name - Cleaned` == x) %>%
      select(-form, -submission)

    rmarkdown::render(rmd,
                      output_file = paste0(x, file_name_end),
                      output_dir = dir)
  },
  
  error = function(cond) {
    
    warning("File could not be generated for ", x, ". ", cond)
    
  })
}

map(x = unique(proposals$`Agency Name - Cleaned`),
    rmd =  'r/budget_proposal.Rmd',
    file_name = "FY21 Budget Proposal.pdf",
    dir = paste0('outputs/'),
    make_proposal_pdf)

map(unique(proposals$`Agency Name - Cleaned`),
    make_proposal_pdf,
    rmd = 'r/how_we_impact.Rmd',
    file_name_end = " FY21 How We Impact.pdf",
    dir = paste0('outputs/How We Impact'))