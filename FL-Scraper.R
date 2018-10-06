library(tidyverse)
library(rvest)
library(lubridate)
library(magrittr)
library(here)

### Scrape University of Florida Jobs Board

scrape.fl.postings <- function(tag = today(), start.date = today(), end.date = today()){
  
  # Helper function to extract posting details
  
  get.fl.posting.info <- function(link){
    
    # Scrape job posting page
    link.html <- link %>%
      read_html
    
    # Extract posting date
    link.information <- link.html %>%
      html_nodes('.gv-list-view-subtitle') %>%
      map(html_text) %>%
      unlist %>%
      str_replace_all("Company Information","") %>%
      str_replace_all("\n","")
    
    # Extract posting description
    link.description <- link.html %>%
      html_nodes('.gv-field-1-5') %>%
      map(html_text) %>%
      unlist %>%
      str_replace_all("Duties and Responsibilities","") %>%
      str_replace_all("\n","")
    
    link.fullinfo <- paste(link.information, link.description)
    
    # Extract the due date
    link.duedate <- link.html %>%
      html_nodes('.gv-field-1-12') %>%
      map(html_text) %>%
      unlist %>%
      str_replace_all("Application Deadline","") %>%
      str_replace_all("\n","") %>%
      mdy
    
    # Exceptions for empty fields
    if (length(link.fullinfo) == 0){
      link.information <- NA
    }
    if (length(link.duedate) == 0){
      link.duedate <- NA
    }
    
    # Return data frame of job posting information
    link.data <- tibble(Description = link.fullinfo,
                        DueDate = link.duedate)
    
    return(link.data)
  }
  
  
  
  
  # Run scraper until there are no more pages of job postings
  
  pagenum <- 1
  
  continue <- TRUE
  
  fl.list <- list()
  
  while (continue){
    
    # Scrape HTML
    url <- paste0("http://forms.stat.ufl.edu/statistics-jobs/?pagenum=",pagenum)
    
    fl.html <-url %>% 
      read_html 
    
    # Check whether there are jobs to scrape on this page
    fl.finalpage <- fl.html %>%
      html_nodes('.gv-list-view-title') %>%
      html_text %>%
      unlist %>%
      str_detect("No entries match your request.") %>%
      sum
    
    if (fl.finalpage == 1){
      print(paste0("No Entries in  ",url))
      continue <- FALSE
      break
    }
    
    # Parse out job posting
    fl.title <- fl.html %>%
      html_nodes('.gv-field-1-4') %>%
      map(html_text) %>%
      unlist 
    
    # Parse out faculty
    fl.faculty <- fl.html %>% 
      html_nodes('.gv-field-1-2') %>%
      map(html_text) %>%
      unlist
    
    # Parse out posting date
    fl.postingdate <- fl.html %>%
      html_nodes('.gv-field-1-date_created') %>%
      map(html_text) %>%
      map(str_replace_all,pattern="Listed",replacement="") %>%
      unlist %>%
      mdy
    
    # Parse out link to posting
    fl.links <- fl.html %>%
      html_nodes("a") %>%
      html_attr('href') %>%
      str_subset("forms.stat.ufl.edu/statistics-jobs/entry") %>%
      unlist
    
    # Combine into data frame
    fl.jobs <- tibble(Source = "UF Job Board", Position = fl.title, Faculty = fl.faculty, PostingDate = fl.postingdate, Link = fl.links) %>%
      filter(PostingDate >= start.date & PostingDate <= end.date)
    
    if (nrow(fl.jobs) == 0){
      print(paste0("No Entries in  ",url))
      continue <- FALSE
      break
    }
    
    # Extract posting information from link
    fl.posting.info <- fl.jobs$Link %>%
      map_df(get.fl.posting.info)
    
    # Bind columns to data frame
    fl.complete <- fl.jobs %>%
      cbind(fl.posting.info)
    
    # Save data frame
    fl.list[[pagenum]] <- fl.complete
    
    print(paste0("Scraped ",url))
    
    pagenum <- pagenum + 1
    
  }
  
  # Combine data frames from all pages
  fl.final <- fl.list %>%
    bind_rows 
  
  write_csv(fl.final,here::here("data",paste0("FL-Jobs_",tag,".csv")))
  
}





