setwd("C:/Users/slung/Desktop/RStudio/Download PDFs")
setwd("C:/Users/admin/Dropbox/Business University Science/TMX")
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
    select(Date)
    # mutate(Month = format(as.Date(Date), "%m"))
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


