library(tidyverse)
library(lubridate)
library(magrittr)
library(googlesheets)
library(here)

source(here::here("FL-Scraper.R"))
source(here::here("ASA-Scraper.R"))

# Initialize Google Sheet

pinned.date <- ymd("2018-10-03")
commence.date <- ymd("2018-07-01")

scrape.asa.postings(tag = "Initial", start.date = commence.date, end.date = pinned.date)
scrape.fl.postings(tag = "Initial", start.date = commence.date, end.date = pinned.date)

asa.jobs <- read_csv(here::here("data","ASA-Jobs_Initial.csv"))
fl.jobs <- read_csv(here::here("data","FL-Jobs_Initial.csv"))

full.jobs <- bind_rows(asa.jobs,fl.jobs) %>%
  mutate(PositionLower = tolower(Position),
         Faculty = str_replace_all(Faculty,",",""),
         Description = str_replace_all(Description,"\n",""),
         Description = str_replace_all(Description,"\t","")) %>%
  filter(str_detect(PositionLower, "faculty") | str_detect(PositionLower, "assistant")) %>%
  filter(!str_detect(PositionLower, "postdoc")) %>%
  filter(!str_detect(PositionLower, "post-doc")) %>%
  filter(!duplicated(select(.,Position,Faculty))) %>%
  arrange(PostingDate,Faculty,Position) %>%
  select(-PositionLower)

gs_new(title="Academic Job Postings",input=full.jobs)
