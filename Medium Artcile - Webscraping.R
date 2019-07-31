setwd("C:/Users/slung/Desktop/RStudio/Download PDFs")
pkg <- c("tidyverse", "rvest", "furrr", "fs", "xopen", "glue", "readxl", "xlsx", "pdftools") 
install.packages(pkg)

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(glue) 
library(readxl)
library(xlsx)
library(pdftools)

# 1.0 Importing and Wrangling ----
base_url <- 'https://www.tsx.com/files/trading/daily-trading-report/'
path = "Flash File Dates.xlsx"

dates_tibble <- read_excel(path = path, sheet = 1) %>% 
    select(Date) %>% 
    format(as.Date(.), "%m")
number_rows <- dates_tibble %>% nrow()

dates_df <- data.frame(1:number_rows) %>% 
  bind_cols(dates_tibble) %>% 
  mutate(url = Date %>% as.character())

#2.0 Webscrape_pdf function ----
webscrape_pdf <- function(link){
    
    #Download data and split by \n
    download_data <- pdf_text(link)
    download_data2 <- strsplit(download_data, "\n")
    
    #Pick 2nd page 
    download_data3 <- download_data2[[2]][1:4] %>%
        strsplit("\r") %>%
        unlist()
    
    doc_split <- strsplit(download_data3, "  ")
    doc_split <- lapply(doc_split, function(x){
        doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
        if (is.na(doc1)) doc1 <- ""
        # doc2 takes the next non-empty piece of text
        doc2 <- x[x != ""] 
        if (doc1 != "") doc2 <- doc2[-1]
        if (length(doc2) == 0) doc2 <- ""
        # Sometimes there is more text needed to be extracted. 
        # I try to give it to either doc1 or doc2 depending on the size of it.
        while (sum(nchar(doc2)) > 27) {
            doc1 <- paste(doc1, doc2[1], collapse = " ")
            doc2 <- doc2[-1]
        }
        # Clean it before returning it
        doc2 <- paste(doc2, collapse = " ")
        doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
        doc2 <- str_trim(doc2)
        list(doc1 = doc1, doc2 = doc2)
    })
    # 
    doc1 <- sapply(doc_split, `[[`, 1) # First column
    doc2 <- sapply(doc_split, `[[`, 2) # Second column
    
    doc1
    
}

#3.0 For loop to put it all together ----
for (i in seq_along(dates_df$Date)){
  dates_df$url[i] <- paste0(base_url, 'Daily_Trading_Report_',dates_df$Date[i], ".pdf")
  dates_df$volume[i] <-  webscrape_pdf(dates_df$url[i])[2] 
  dates_df$value[i] <- webscrape_pdf(dates_df$url[i])[3]
  dates_df$trades[i] <- webscrape_pdf(dates_df$url[i])[4]
}

#4.0 Export into csv ----
write.csv(dates_df, file = "webscraped_tbl.csv")

link <- dates_df$url[8]
x <- doc_split

webscrape_pdf <- function(link){
  
  #Download data and split by \n
  download_data <- pdf_text(link)
  download_data2 <- strsplit(download_data, "\n")
  
  #Pick 2nd page 
  download_data3 <- download_data2[[2]][1:4] %>%
    strsplit("\r") %>%
    unlist()
  
  doc_split <- strsplit(download_data3, "  ")
  doc_split <- lapply(doc_split, function(x){
    doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
    if (is.na(doc1)) doc1 <- ""
    # doc2 takes the next non-empty piece of text
    doc2 <- x[x != ""] 
    if (doc1 != "") doc2 <- doc2[-1]
    if (length(doc2) == 0) doc2 <- ""
    # Sometimes there is more text needed to be extracted. 
    # I try to give it to either doc1 or doc2 depending on the size of it.
    while (sum(nchar(doc2)) > 27) {
      doc1 <- paste(doc1, doc2[1], collapse = " ")
      doc2 <- doc2[-1]
    }
    # Clean it before returning it
    doc2 <- paste(doc2, collapse = " ")
    doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
    doc2 <- str_trim(doc2)
    list(doc1 = doc1, doc2 = doc2)
  })
  # 
  doc1 <- sapply(doc_split, `[[`, 1) # First column
  doc2 <- sapply(doc_split, `[[`, 2) # Second column
  
  doc1
  
}















# Download dates and convert to list
path = "Flash File Dates.xlsx"
dates_tbl <- read_excel(path = path) %>% 
  pull(Date) %>% 
  as.list()
number_rows <- length(dates_tbl)
tsx_tbl <- list(1:number_rows)
dates_tbl[1:13]

# Setup loop based on website names
for (i in seq_along(dates_tbl)){
  dates_tbl[[i]] <- paste0(base_url, 'Daily_Trading_Report_',dates_tbl[[i]], ".pdf")
  # print(paste0(base_url, "Daily_Trading_Report_",dates_tbl[i],".pdf"))
}

# Reproducible Function for TSX ----

data <- dates_df[,3][1] 
x <- data %>% tsx_web_scrape_process()
  
  
tsx_web_scrape_process <- function(data){
  scrape_data <- data %>% 
    pdf_text(data) %>% 
    strsplit(., "\n")
  
  scrape_data <- scrape_data[[2]][1:4] %>% 
    strsplit("\r") %>% 
    unlist() %>% 
    strsplit(., "  ")
}
tsx_filter_data_process <- function(x){
  doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
  if (is.na(doc1)) doc1 <- ""
  # doc2 takes the next non-empty piece of text
  doc2 <- x[x != ""] 
  if (doc1 != "") doc2 <- doc2[-1]
  if (length(doc2) == 0) doc2 <- ""
  # Sometimes there is more text needed to be extracted. 
  # I try to give it to either doc1 or doc2 depending on the size of it.
  while (sum(nchar(doc2)) > 28) {
    doc1 <- paste(doc1, doc2[1], collapse = " ")
    doc2 <- doc2[-1]
  }
  # Clean it before returning it
  doc2 <- paste(doc2, collapse = " ")
  doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
  doc2 <- str_trim(doc2)
  list(doc1 = doc1)
}

doc1

for (i in seq_along(dates_tbl)){
  unlisted <- dates_tbl[i] %>% unlist() 
  tsx_tbl[i] <- tsx_web_scrape_process(unlisted) %>%
    lapply(., tsx_filter_data_process) %>% unlist() %>% as_tibble()
}


write.csv(tsx_tbl, file = "my_data.csv")

# data <- dates_tbl[14] %>% unlist()
# x <- scrape_data
# unlisted <- dates_tbl[14] %>% unlist()

tsx_web_scrape_process(unlisted) %>%
  lapply(., tsx_filter_data_process) %>%
  unlist() %>%
  as_tibble()


tsx_tbl %>% unlist() %>% 
  str_split(pattern = "Daily ", simplify = TRUE) %>% 
  as.tibble() %>% 
  rename(TSX = V1, Measures = V2) %>% 
  str_split(Measures, pattern = "Volume ", simplify = TRUE) %>% 
  View()


# Webscraping TSXV part -----
tsxv_tbl <- list(1:number_rows)
tsxv_web_scrape_process <- function(data){
  scrape_data <- data %>% 
    pdf_text(data) %>% 
    strsplit(., "\n")
  
  scrape_data <- scrape_data[[3]][10:13] %>% 
    strsplit("\r") %>% 
    unlist() %>% 
    strsplit(., "  ")
}
tsxv_filter_data_process <- function(x){
  doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
  if (is.na(doc1)) doc1 <- ""
  # doc2 takes the next non-empty piece of text
  doc2 <- x[x != ""] 
  if (doc1 != "") doc2 <- doc2[-1]
  if (length(doc2) == 0) doc2 <- ""
  # Sometimes there is more text needed to be extracted. 
  # I try to give it to either doc1 or doc2 depending on the size of it.
  while (sum(nchar(doc2)) > 28) {
    doc1 <- paste(doc1, doc2[1], collapse = " ")
    doc2 <- doc2[-1]
  }
  # Clean it before returning it
  doc2 <- paste(doc2, collapse = " ")
  doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
  doc2 <- str_trim(doc2)
  list(doc1 = doc1)
}
for (i in seq_along(dates_tbl)){
  unlisted <- dates_tbl[i] %>% unlist() 
  tsxv_tbl[i] <- tsxv_web_scrape_process(unlisted) %>%
    lapply(., tsxv_filter_data_process) %>% unlist() %>% as_tibble()
}

unlisted_tbl <- tsxv_tbl %>% 
  unlist() 
write.csv(unlisted_tbl, file = "tsxv.csv")


# Webscraping Alpha Part ----
# Weblink
base_url <- 'https://www.tsx.com/files/alpha/daily-record/AlphaDailyRecord'
link1 <- "07032019.pdf"
event_url <- paste0(base_url, link1)
event_url 

path = "Flash File Dates.xlsx"
alpha_dates_tbl <- read_excel(path = path) %>% 
  select(Date) %>% 
  separate(Date, into = c("year", "month", "day"), sep = "-") %>% 
  unite(month, day, year, col = "date", sep="") %>% 
  pull(date) %>% 
  as.list()

alpha_number_rows <- length(alpha_dates_tbl)
alpha_tbl <- list(1:number_rows)

# Setup loop based on website names
for (i in seq_along(alpha_dates_tbl)){
  alpha_dates_tbl[[i]] <- paste0(base_url, alpha_dates_tbl[[i]], ".pdf")
  # print(paste0(base_url, "Daily_Trading_Report_",dates_tbl[i],".pdf"))
}

alpha_web_scrape_process <- function(data){
  scrape_data <- data %>% 
    pdf_text(data) %>% 
    strsplit(., "\n")
  
  scrape_data <- scrape_data[[1]][5] %>% 
    strsplit("\r") %>% 
    unlist() %>% 
    strsplit(., "  ") 
  
  scrape_data <- scrape_data[[1]][c(5,10, 13)] %>% as.list()
}




for (i in seq_along(alpha_dates_tbl)){
  unlisted <- alpha_dates_tbl[i] %>% unlist() 
  alpha_tbl[i] <- alpha_web_scrape_process(unlisted) 
}
alpha_tbl


?matrix
matrix(, nrow=13, ncol=4)

unlisted <- alpha_dates_tbl[1] %>% unlist()
scrape_data <- unlisted %>%
  pdf_text(unlisted) %>%
  strsplit(., "\n") 


scrape_data <- scrape_data[[1]][5] %>%
  strsplit("\r") %>%
  unlist() %>%
  strsplit(., "  ")

scrape_data
scrape_data <- scrape_data[[1]][c(5, 10, 13)] %>% as.list()

alpha_web_scrape_process(unlisted)

unlisted_tbl <- alpha_tbl %>% 
  unlist() 
write.csv(unlisted_tbl, file = "alpha.csv")


