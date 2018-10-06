library(tidyverse)
library(lubridate)
library(magrittr)
library(googlesheets)

setwd("C:/Users/braym/Box Sync/Personal/JobScraper") # No work-around that I can find... this should be the only spot it's needed

source(here::here("FL-Scraper.R"))
source(here::here("ASA-Scraper.R"))

# Update Google Sheet

scrape.asa.postings()
scrape.fl.postings()

asa.jobs <- read_csv(here::here("data",paste0("ASA-Jobs_",today(),".csv")),
                     col_types = cols(Source = col_character(),
                                     Position = col_character(),
                                     Faculty = col_character(),
                                     Link = col_character(),
                                     Description = col_character(),
                                     PostingDate = col_character(),
                                     DueDate = col_character())
                     )

fl.jobs <- read_csv(here::here("data",paste0("FL-Jobs_",today(),".csv")),
                    col_types = cols(Source = col_character(),
                                     Position = col_character(),
                                     Faculty = col_character(),
                                     Link = col_character(),
                                     Description = col_character(),
                                     PostingDate = col_character(),
                                     DueDate = col_character())
                    )

new.jobs <- bind_rows(asa.jobs,fl.jobs) %>%
  mutate(PositionLower = tolower(Position),
         Faculty = str_replace_all(Faculty,",",""),
         Description = str_replace_all(Description,"\n",""),
         Description = str_replace_all(Description,"\t",""),
         FacultyLower = tolower(Faculty)) %>%
  filter(str_detect(PositionLower, "faculty") | str_detect(PositionLower, "assistant")) %>%
  filter(!str_detect(PositionLower, "postdoc")) %>%
  filter(!str_detect(PositionLower, "post-doc")) %>%
  filter(!duplicated(select(.,PositionLower,FacultyLower))) %>%
  arrange(PostingDate,Faculty,Position) %>%
  select(-PositionLower,-FacultyLower)

sheet <- gs_title("Academic Job Postings")

gs_add_row(sheet, input = new.jobs)
