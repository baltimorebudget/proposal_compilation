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
  # select(-`FY24 Dollars - PROP`) %>%
  relocate(`FY24 Positions - CLS`, .after = !!cols$dollars_cls) %>%
  filter(!!sym(cols$dollars_prop) != 0) %>%
  group_by(`Agency Name`, `Agency Name - Cleaned`, `Service ID`, `Service Name`, Fund) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) 

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

sc_questions <- import("inputs/OutcomeStat_20221224.xlsx", which = "Services") %>%
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
         `Service Name` = str_extract(Service, "(?<=: ).+")) %>%
  filter(!is.na(`Service ID`))

Encoding(sc_questions$`Mayor's  Action Plan`) <- "UTF-8"
Encoding(sc_questions$Resiliency) <- "UTF-8"
Encoding(sc_questions$Equity) <- "UTF-8"

##scorecard service descriptions ===========
# sc_service_desc <- readRDS(paste0(path$prop, "service_desc.Rds")) 

##story behind the curve =====

# sc_story <- readRDS(paste0(path$prop, "scorecard_pm_notes.Rds")) %>%
#   filter(`PM Note Type` == "Story Behind the Curve")

sc_story <- import("inputs/Story Behind the Curve.xlsx", skip = 4) %>%
  select(-`...6`, -`...7`, -`...8`, -`...9`, -`...10`, -`...11`, -`...12`, -`...13`, -`...14`, -`...15`, -`...16`) %>%
  filter(!is.na(`Note Text`)) %>%
  fill(Name, .direction = "down") %>%
  fill(Title, .direction = "down") %>%
  fill(Title2, .direction = "down") %>%
  fill(Type, .direction = "down") %>%
  fill(`Modify Date`, .direction = "down") %>%
  mutate(`Service ID` = str_extract(Title, "([0-9]{3}[a-d]{1})|([0-9]{3})")) %>%
  rename(Measure = Title2, `Service Name` = Title, Tag = Name) %>%
  filter(!is.na(`Service ID`))

Encoding(sc_story$`Note Text`) <- "UTF-8"

##rendering ======
#save electeds for later
# elected_services = sc_questions %>% 
#   filter((`Agency` %in% c("City Council", "Council Services", "Comptroller", "State's Attorney", "Sheriff"))) %>%
#   select(`Service ID`) %>%
#   distinct()

services <- sc_questions$`Service ID`

for (i in services) {
  
  if (i %in% c("100", "103", "115", "130", "131", "132", "133", "136", "781", "786", "881", "882", "883", "884", "889",
               "100",  "103",  "106",  "107",  "109",  "110",  "115",  "117",  "125",  "128",  "130",  "131",  "132",
               "133",  "136",  "148",  "150",  "152",  "154", "155",  "168",  "185",  "189",  "303",  "305",  "307",
               "308",  "310",  "311",  "315",  "316",  "356",  "385a", "385b", "446",  "493a", "493b", "493c",
               "493d", "500",  "548",  "590b", "590c", "593",  "600",  "602",  "604",  "605",  "608",  "609",  "610",
               "611",  "612",  "613",  "614",  "615",  "617", "618",  "619",  "621",  "622",  "623",  "624",
               "625",  "626",  "627",  "628",  "632",  "634",  "635",  "637",  "638",  "640",  "642",  "644",  "645", 
               "646",  "647",  "648",  "649",  "650",  "651",  "652",  "653",  "654", "656",  "660",  "661",  "662",
               "663",  "664",  "670",  "671",  "672",  "673", "674",  "675",  "676",  "681",  "682",  "683",  "684",
               "685",  "687",  "688",  "689",  "690",  "691",  "692",  "693",  "694",  "695",  "696",  "697", 
               "698",  "699", "700",  "701",  "702",  "703",  "704",  "705",  "707",  "708",  "710",  "711",  "715",
               "716",  "717",  "718",  "720",  "721",  "722", "723",  "724", "725",  "726",  "727",  "730",  "731",
               "734",  "737",  "738",  "740",  "741",  "742",  "745",  "747",  "748",  "749",  "750",  "751", 
               "752",  "754",  "757",  "758",  "761",  "762",  "763",  "765",  "768",  "770",  "771",  "772",  "773",
               "781",  "786",  "788",  "791",  "792",  "793", "794", "795",  "796",  "797",  "798",  "800",  "802",
               "803",  "804",  "805",  "806",  "807",  "809")) {
    
  } else {
  
  info <- sc_questions %>%
    filter(`Service ID` == i) %>%
    mutate(`Service Name` = gsub(":", "", `Service Name`),
           `Service Name` = gsub("/", "-", `Service Name`))
  
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
  }

##must label proposals as Health1 and Health2 for big agencies================
# sc_enhancements <- import("inputs/Service Note Export_12-16-2022.xlsx", which = "Enhancement") %>%
#   # filter(Enhancement == "FY24 Enhancement") %>%
#   separate(col = Enhancement, into = c("Year", "Subservice"), sep = " - ") %>%
#   ##remember to manually add version to the data
#   mutate(Version = as.character(Version),
#          Agency = case_when(!is.na(Subservice) ~ paste(gsub("&#39;", "'", Scorecard), Subservice, Version),
#                              TRUE ~ paste(gsub("&#39;", "'", Scorecard), Version)),
#          Agency = gsub("NA", "", Agency),
#          NoteText = str_trim(gsub("Please complete this section ONLY if you are submitting an enhancement request that will advance and accelerate a specific action in the Mayor's Action Plan.", 
#                                   "", NoteText, fixed = TRUE), side = "left"),
#          NoteText = str_trim(gsub("Instructions:  This section should reflect the total dollar amount and number of positions being requested.  Any request for positions must accurately reflect the fully-loaded costs of the positions(s), including all OPCs.  For more information you can review the personnel costs calculation under \"Part 3: BPFS Instructions\" in the Instructions document.", 
#                                   "", NoteText, fixed = TRUE), side = "left"),
#          NoteText = str_trim(gsub("Equity  Baltimore City's Equity Assessment Program defines equity as closing the gaps in policy, practice, and allocation of City resources so that race, gender, religion, sexual orientations, and income do not predict one’s success, while also improving outcomes for all. Equality provides the same resources and opportunities to all, whereas equity recognizes there are institutional and structural barriers and provides everyone with what they need to thrive.", 
#                                   "", NoteText, fixed = TRUE), side = "left"),
#          NoteText = str_trim(gsub("Resiliency  Baltimore City's Sustainability Office defines resiliency as the ability of our community to anticipate, accommodate, and positively adapt to or thrive amidst changing climate conditions or hazard events and enhance quality of life, reliable systems, economic vitality, and conservation of resources for present and future generations.", 
#                                   "", NoteText, fixed = TRUE), side = "left"),
#          NoteText = str_trim(gsub("Please complete this section ONLY if you are submitting an enhancement request that will address workload and service delivery demands.", 
#                                   "", NoteText, fixed = TRUE), side = "left"),
#          NoteText = gsub("([A-Za-z])\\s\\s+([A-Za-z])", "\\1\\2", NoteText)) %>%
#   pivot_wider(id_cols = Agency, names_from = NoteType, values_from = NoteText) %>%
#   mutate(Service = str_extract(`Identifying Information`, "(?<=Service:).+(?=Contact Name:)"),
#          `Enhancement Budget` = gsub("4. Budget DetailNote: If you have no expenditures for a section, please mark the amount as $0.  If you have more than one line item for an expenditure type, feel free to include additional rows.               ",
#                                      "", `Enhancement Budget`, fixed = TRUE),
#          Budget = str_trim(str_extract(`Enhancement Budget`, "(?<=Cost explanation).+"), side = "both"),
#          `Total Cost` = str_trim(str_extract(`Enhancement Budget`, "(?<=TOTAL EXPENDITURES).+(?=)"), side = "both"),
#          `Total Positions` = str_trim(str_extract(`Enhancement Budget`, "(?<=Positions Requested).+(?=Expenditure TypeCost explanation)"), side = "both"))
# 
# export_excel(sc_enhancements, "FY24 Enhancement Proposals", "outputs/FY24 Enhancement Proposals.xlsx")
# 
# agencies <- sc_enhancements$Agency
# 
# for (a in agencies) {
#   
#   agency = str_trim(gsub('[[:digit:]]+', '', a), side = "right")
#   service = sc_enhancements$Service[sc_enhancements$Agency == a]
#   #quasis need to be added here / names aren't the same as official names, MOIT/BCIT
#   # agency = unique(analysts$`Agency Name - Cleaned`[analysts$`Agency Name`==a])
#   
#   rmarkdown::render("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/r/enhance_pages.qmd",
#                     output_file = paste0("G:/Analyst Folders/Sara Brumfield/planning_year/2b_proposal_compilation/outputs/fy", params$fy, "/", str_trim(a, side = "right"), " Budget Enhancements.pdf"))
#   
# }