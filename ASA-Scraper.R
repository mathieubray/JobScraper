library(tidyverse)
library(rvest)
library(lubridate)
library(magrittr)
library(here)

### Scrape ASA Jobs Board

scrape.asa.postings <- function(tag = today(), start.date = today(), end.date = today()){
  
  # Helper function to extract posting details
  
  get.asa.posting.info <- function(link){
    
    # Scrape job posting page
    link.html <- link %>%
      read_html
    
    # Extract posting date
    link.postingdate <- link.html %>%
      html_nodes('.bti-jd-details-action') %>%
      map(html_text) %>%
      unlist %>%
      str_trim %>%
      str_replace_all("\t","") %>%
      str_split("\r\n") %>%
      unlist %>%
      extract(2) %>%
      mdy
    
    # Extract posting description
    link.description <- link.html %>%
      html_nodes('.bti-jd-description') %>%
      map(html_text) %>%
      unlist %>%
      str_trim
    
    # Guess the due date
    link.description.split <- link.description %>%
      str_split("[[:whitespace:]]")
    
    possible.dates <- link.description.split %>%
      map2(month.abb,str_which) %>%
      unlist
    
    extract.date <- function(k){
      
      date <- link.description.split %>%
        unlist %>%
        extract(c(k,k+1,k+2)) %>%
        paste(collapse=" ")
      
      return(date)
    }
    
    link.duedate <- map_chr(possible.dates, extract.date) %>%
      mdy(quiet=TRUE) %>%
      unique %>%
      extract(1)
    
    # Extract posting information
    link.information <- link.html %>%
      html_nodes('.bti-jd-employer-info') %>%
      map(html_text) %>%
      unlist %>%
      str_trim
    
    link.fullinfo <- paste(link.description,link.information)
    
    
    # Exceptions for empty fields
    if (length(link.fullinfo) == 0){
      link.fullinfo <- NA
    }
    if (length(link.postingdate) == 0){
      link.duedate <- NA
    }
    if (length(link.duedate) == 0){
      link.duedate <- NA
    }
    
    # Return data frame of job posting information
    link.data <- tibble(Description = link.fullinfo,
                        PostingDate = link.postingdate,
                        DueDate = link.duedate)
    
    return(link.data)
    
  }
  
  
  # Runs scraper until there are no more pages of job postings within the bounded dates
  
  pagenum <- 1
  
  continue <- TRUE
  
  asa.list <- list()
  
  while (continue){
    
    # Scrape HTML
    
    url <- paste0("https://jobs.amstat.org/jobs/?page=",pagenum)
    
    asa.html <- url %>% 
      read_html 
    
    # Check whether there are jobs to scrape on this page
    asa.finalpage <- asa.html %>%
      html_nodes('.bti-ui-job-results-container') %>%
      html_text %>%
      unlist %>%
      str_detect("Sorry, we couldn't find any jobs that match your criteria") %>%
      sum
    
    if (asa.finalpage == 1){
      print(paste0("No Entries in  ",url))
      continue <- FALSE
      break
    }
    
    # Parse out job posting
    asa.title <- asa.html %>%
      html_nodes('.bti-job-detail-link') %>%
      map(html_text) %>%
      unlist
    
    # Parse out faculty
    asa.faculty <- asa.html %>%
      html_nodes('.bti-ui-job-result-detail-employer') %>%
      map(html_text) %>%
      unlist %>%
      str_trim
    
    # Parse out link to posting
    asa.links <- asa.html %>%
      html_nodes('.bti-ui-job-result-detail-title') %>%
      html_nodes("a") %>%
      html_attr('href') %>%
      map_chr(function(x){ paste0("https://jobs.amstat.org",x) })
    
    
    # Combine into data frame
    asa.jobs <- tibble(Source = "ASA", Position = asa.title, Faculty = asa.faculty, Link = asa.links)
    
    # Extract posting information from link
    asa.posting.info <- asa.links %>%
      map_df(get.asa.posting.info)
    
    # Bind columns to data frame
    asa.complete <- asa.jobs %>%
      cbind(asa.posting.info) %>%
      filter(PostingDate >= start.date & PostingDate <= end.date)
    
    if (nrow(asa.complete) == 0){
      print(paste0("No Entries in  ",url))
      continue <- FALSE
    } else {
      
      # Save data frame
      asa.list[[pagenum]] <- asa.complete
      
      print(paste0("Scraped ",url))
      
      pagenum <- pagenum + 1
      
    }
    
  }
  
  # Combine data frames from all pages
  asa.final <- asa.list %>%
    bind_rows 
  
  write_csv(asa.final,here::here("data",paste0("ASA-Jobs_",tag,".csv")))
  
}






