library(httr)
library(rio)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(knitr)
library(kableExtra)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")

options("openxlsx.numFmt" = "#,##0")

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  mutate(`Agency ID` = as.character(`Agency ID`)) %>%
  select(-(Operational:Notes))

import_review_files <- function() {
  
  list.files("G:/Fiscal Years/Fiscal 2021/Planning Year/3. Proposal/5. Budget Proposals/Budget Proposal Option Reviews",
             "^FY21", full.names = TRUE) %>%
    map(import) %>%
    map(mutate_at, vars("Amount", "Ranking"), as.numeric) %>%
    bind_rows() %>%
    arrange(desc(Amount)) %>%
    select(Agency:Notes)
}

import_review_files() %>%
  export_excel(
  "All Options", 
  "G:/Fiscal Years/Fiscal 2021/Planning Year/3. Proposal/5. Budget Proposals/Budget Proposal Option Reviews/Compiled - FY21 Option Reviews.xlsx", "new",
  col.width = c("auto", 60, "auto", 15, 60))
