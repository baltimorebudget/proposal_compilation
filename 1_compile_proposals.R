# Combine BPFS proposal data with agency answers from Scorecard to get a 
# comprehensive proposal PDF per service

params <- list(
  fy = 24
)

.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(httr)
library(rio)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(knitr)
library(kableExtra)
library(scales)
library(stringr)
library(janitor)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")

source("r/functions.R")
source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/formatting.R")
source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/latex.R")
source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/tables.R")
source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/totals.R")
source("G:/Budget Publications/automation/1_prelim_exec_sota/bookPrelimExecSOTA/R/format_functions.R")

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  mutate(`Agency ID` = as.character(`Agency ID`)) %>%
  select(-(Operational:Notes))

path <- list(cls = paste0("G:/Budget Publications/automation/0_data_prep/outputs/fy",
                params$fy, "_cls/"),
             prop = paste0("G:/Budget Publications/automation/0_data_prep/outputs/fy",
                           params$fy, "_prop/"))

cols <- readRDS(paste0(path$prop, "cols.Rds"))
cols$dollars_cls <- paste0("FY", params$fy, " Dollars - CLS")
cols$dollars_prop <- paste0("FY", params$fy, " Dollars - PROP")

line_item <- list(
  cls = readRDS(paste0(path$cls, "expenditure.Rds")),
  prop = readRDS(paste0(path$prop, "expenditure.Rds")))  %>%
  map(group_by_at, vars(starts_with("Agency Name"), starts_with("Service"), starts_with("Fund"))) %>%
  map(summarize_at, vars(starts_with("FY")), sum, na.rm = TRUE) %>%
  map(ungroup)

line_item$cls <- line_item$cls %>%
  select(-ends_with("Name"), -ends_with("Actual"), -!!paste0("FY", params$fy - 1, " Budget")) %>%
  rename(!!cols$dollars_cls := !!paste0("FY", params$fy, " Budget"))

line_item$prop <- line_item$prop %>%
  select(`Agency Name`:`Fund Name`, !!cols$expend$prior, !!cols$expend$projection, 
         !!cols$dollars_prop := !!paste0("FY", params$fy, " Budget")) %>%
  set_colnames(gsub("Budget|Actual", "Dollars", names(.)))

line_item <- line_item$prop %>%
  left_join(line_item$cls) %>%
  relocate(!!cols$dollars_prop, .after = last_col())

positions <- readRDS(paste0(path$prop, "positions.Rds"))

positions$cls <- readRDS(paste0(path$cls, "positions.Rds")) %>%
    extract2("planning")

positions <- positions %>%
  map(group_by, `Service ID`, `Fund Name`) %>%
  map(count)

positions <- positions$planning %>%
  rename(`FY24 Positions - PROP` = n) %>%
  left_join(positions$cls %>%
              rename(`FY24 Positions - CLS` = n)) %>%
  left_join(positions$projection %>%
              rename(`FY23 Positions` = n)) %>%
  left_join(positions$prior %>%
              rename(`FY22 Positions` = n))

summary <- line_item %>%
  left_join(positions) %>%
  mutate(`Fund` = case_when(`Fund ID` == 1001 ~ "General Fund",
                            TRUE ~ "All Other Funds")) %>%
  relocate(Fund, .after = `Fund Name`) %>%
  select(-`Fund ID`, -`Fund Name`) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  select(`Agency Name`:`Fund`, sort(names(.))) %>%
  select(-`FY24 Prop`) %>%
  relocate(`FY24 Positions - CLS`, .after = !!cols$dollars_cls) %>%
  filter(!!sym(cols$dollars_prop) != 0) %>%
  group_by(`Agency Name`, `Agency Name - Cleaned`, `Service ID`, `Service Name`, Fund) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) 

##get Proposal budget from Scorecard eventually ==================


##scorecard PM data (if needed) =================
sc_pms <- readRDS(paste0(path$prop, "scorecard_pms.Rds"))

sc_pms %<>%
  select(-`Extra PM Table`) %>%
  select(`Agency Name - Cleaned`, `Service ID`:Measure,
         ends_with(paste0("20", params$fy - 3)), 
         ends_with(paste0("20", params$fy - 2)),
         ends_with(paste0("20", params$fy - 1)), 
         ends_with(paste0("20", params$fy)))


##proposal questions and answer ==============
# sc_questions <- readRDS(paste0(path$prop, "scorecard_questions.Rds"))
  # mutate_at(vars(starts_with("Question")), funs(gsub("â€™s", "'", ., fixed = TRUE)))

sc_questions <- import("inputs/FY24_ServiceNotes.xlsx") %>%
  filter(!grepl("*(MOSS)|*(Copy)|*FY23|*Protocol|*Action Plans|*FY 23", Service)) %>%
  mutate(
  # `Service ID` = str_extract(Service, "[0-9]{3}"),
         NoteText = str_trim(gsub("For your reference, the Fiscal 2022 Actual and Fiscal 2023 Adopted expenditure and position information have been populated below.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Instructions: This information has been pre-loaded; if any edits are needed to the table or description, discuss with your budget analyst.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Service Delivery & EquityBaltimore City's Equity Assessment Program defines equity as closing the gaps in policy, practice, and allocation of City resources so that race, gender, religion, sexual orientations, and income do not predict one’s success, while also improving outcomes for all. Equality provides the same resources and opportunities to all, whereas equity recognizes there are institutional and structural barriers and provides everyone with what they need to thrive.               ", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Service Delivery & ResiliencyBaltimore City's Sustainability Office defines resiliency as the ability of our community to anticipate, accommodate, and positively adapt to or thrive amidst changing climate conditions or hazard events and enhance quality of life, reliable systems, economic vitality, and conservation of resources for present and future generations.               ", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = gsub("([A-Za-z])\\s\\s+([A-Za-z])", "\\1\\2", NoteText)) %>%
  pivot_wider(id_cols = c(Service), names_from = NoteType, values_from = NoteText) %>%
  mutate(`Service ID` = str_extract(`Identifying Information`, "([0-9]{3}[a-d]{1})|([0-9]{3})"),
         Description = str_trim(str_extract(`Identifying Information`, "(?<=escription).+(?=Service Contact)"), side = "right"),
         Pillar = str_extract(`Identifying Information`, "(?<=Pillar).+(?=Lead Agency)"),
         Agency = str_extract(`Identifying Information`, "(?<=Lead Agency).+((?=Service Description)|(?=Elongated))"),
         `Service Name` = str_extract(Service, "(?<=: ).+"))

##scorecard service descriptions ===========
# sc_service_desc <- readRDS(paste0(path$prop, "service_desc.Rds")) 

# sc_story <- readRDS(paste0(path$prop, "scorecard_pm_notes.Rds")) %>%
#   filter(`PM Note Type` == "Story Behind the Curve")

services = c("708")

for (i in services) {
  
  info <- sc_questions %>%
    filter(`Service ID` == i)
  
  service = info$`Service Name`
  id = i
  agency = info$Agency
  description = info$Description
  pillar = info$Pillar
  fy24_dollars = sum(summary$`FY24 Dollars - PROP`[summary$`Service ID` == i], na.rm = TRUE)
  fy24_positions = sum(summary$`FY24 Positions - PROP`[summary$`Service ID` == i], na.rm = TRUE)
  
  rmarkdown::render("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/r/service_pages.Rmd",
                    output_file = paste0("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/outputs/fy", params$fy, "/", i, " ", service, " Budget Proposal.pdf"))
  
}

##must label proposals as Health1 and Health2 for big agencies================
sc_enhancements <- import("inputs/FY24_EnhancementNotes.xlsx") %>%
  filter(Enhancement == "FY24 Enhancement") %>%
  separate(col = Enhancement, into = c("Year", "Subservice"), sep = " - ") %>%
  mutate(Version = as.character(Version),
         Agency = case_when(!is.na(Subservice) ~ paste(gsub("&#39;", "'", Scorecard), Subservice),
                             TRUE ~ paste(gsub("&#39;", "'", Scorecard), Version)),
         Agency = gsub("NA", "", Agency),
         NoteText = str_trim(gsub("Please complete this section ONLY if you are submitting an enhancement request that will advance and accelerate a specific action in the Mayor's Action Plan.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Instructions:  This section should reflect the total dollar amount and number of positions being requested.  Any request for positions must accurately reflect the fully-loaded costs of the positions(s), including all OPCs.  For more information you can review the personnel costs calculation under \"Part 3: BPFS Instructions\" in the Instructions document.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Equity  Baltimore City's Equity Assessment Program defines equity as closing the gaps in policy, practice, and allocation of City resources so that race, gender, religion, sexual orientations, and income do not predict one’s success, while also improving outcomes for all. Equality provides the same resources and opportunities to all, whereas equity recognizes there are institutional and structural barriers and provides everyone with what they need to thrive.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Resiliency  Baltimore City's Sustainability Office defines resiliency as the ability of our community to anticipate, accommodate, and positively adapt to or thrive amidst changing climate conditions or hazard events and enhance quality of life, reliable systems, economic vitality, and conservation of resources for present and future generations.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = str_trim(gsub("Please complete this section ONLY if you are submitting an enhancement request that will address workload and service delivery demands.", 
                                  "", NoteText, fixed = TRUE), side = "left"),
         NoteText = gsub("([A-Za-z])\\s\\s+([A-Za-z])", "\\1\\2", NoteText)) %>%
  pivot_wider(id_cols = Agency, names_from = NoteType, values_from = NoteText)

agencies <- sc_enhancements$Agency

for (a in agencies) {
  
  agency = str_trim(gsub('[[:digit:]]+', '', a), side = "right")
  #quasis need to be added here / names aren't the same as official names, MOIT/BCIT
  # agency = unique(analysts$`Agency Name - Cleaned`[analysts$`Agency Name`==a])
  
  rmarkdown::render("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/r/enhance_pages.qmd",
                    output_file = paste0("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/outputs/fy", params$fy, "/", str_trim(a, side = "right"), " Budget Enhancements.pdf"))
  
}