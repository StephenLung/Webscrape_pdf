setwd("C:/Users/slung/Desktop/RStudio/Download PDFs")
setwd("C:/Users/admin/Dropbox/Business University Science/TMX")
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(glue) 
library(readxl)
library(xlsx)
library(pdftools)
library(lubridate)

# 1.0 Importing and Wrangling ----
base_url <- 'https://www.tsx.com/files/trading/daily-trading-report/'
alpha_url <- 'https://www.tsx.com/files/alpha/daily-record/AlphaDailyRecord'
path = "Flash File Dates.xlsx"

dates_tibble <- read_excel(path = path, sheet = 1) %>% 
  select(Date) %>% 
    mutate(month = format(as.Date(Date), "%m"),
                      day = format(as.Date(Date), "%d"),
                      year = format(as.Date(Date), "%Y"))
number_rows <- dates_tibble %>% nrow()

dates_tsx <- data.frame(1:number_rows) %>% 
  bind_cols(dates_tibble) %>% 
  mutate(url = Date %>% as.character(),
         alpha_url = paste0(alpha_url,
                            month,
                            day,
                            year,
                            ".pdf"))

#2.0 Webscrape_pdf function ----
webscrape_pdf_tsx <- function(link){
    
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
webscrape_pdf_tsxv <- function(link){
    
    #Download data and split by \n
    download_data <- pdf_text(link)
    download_data2 <- strsplit(download_data, "\n")
    
    #Pick 3rd page 
    download_data3 <- download_data2[[3]][10:13] %>%
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
webscrape_pdf_alpha <- function(link){
    
    #Download data and split by \n
    download_data <- pdf_text(link)
    download_data2 <- strsplit(download_data, "\n")
    
    #Pick 1st page page 
    download_data3 <- download_data2[[1]][3:5] %>%
        strsplit("\r") %>%
        unlist()
    
    extract_text <- download_data3 %>% 
        as.tibble() %>% 
        slice(3) %>% 
        as.character() %>% 
        strsplit("\\s+")
    
    extract_text
    # strsplit(x, "\\s+")[[1]][2]
}

#3.0 For loop to put it all together ----
for (i in seq_along(dates_tsx$Date)){
  dates_tsx$url[i] <- paste0(base_url, 'Daily_Trading_Report_',dates_tsx$Date[i], ".pdf")
  dates_tsx$volume_tsx[i] <-  webscrape_pdf_tsx(dates_tsx$url[i])[2]
  dates_tsx$value_tsx[i] <- webscrape_pdf_tsx(dates_tsx$url[i])[3]
  dates_tsx$trades_tsx[i] <- webscrape_pdf_tsx(dates_tsx$url[i])[4]
  dates_tsx$volume_tsxv[i] <-  webscrape_pdf_tsxv(dates_tsx$url[i])[2]
  dates_tsx$value_tsxv[i] <- webscrape_pdf_tsxv(dates_tsx$url[i])[3]
  dates_tsx$trades_tsxv[i] <- webscrape_pdf_tsxv(dates_tsx$url[i])[4]
  dates_tsx$volume_alpha[i] <- webscrape_pdf_alpha(dates_tsx$alpha_url[i])[[1]][2]
  dates_tsx$value_alpha[i] <- webscrape_pdf_alpha(dates_tsx$alpha_url[i])[[1]][3]
  dates_tsx$trades_alpha[i] <- webscrape_pdf_alpha(dates_tsx$alpha_url[i])[[1]][4]
  # dates_tsx[[i]] <- paste0(base_url, 'Daily_Trading_Report_',dates_tsx[[i]], ".ptsx")
}

dates_tbl <- dates_tsx %>% 
  as.tibble() %>% 
  mutate(volume_tsx = gsub("Daily{0,1} Volume{0,1} [ ]{0,1}", "", volume_tsx),
         value_tsx = gsub("Daily{0,1} Value{0,1} [ ]{0,1}", "", value_tsx),
         trades_tsx = gsub("Daily{0,1} Trades{0,1} [ ]{0,1}", "", trades_tsx),
         volume_tsxv = gsub("Daily{0,1} Volume{0,1} [ ]{0,1}", "", volume_tsxv),
         value_tsxv = gsub("Daily{0,1} Value{0,1} [ ]{0,1}", "", value_tsxv),
         trades_tsxv = gsub("Daily{0,1} Trades{0,1} [ ]{0,1}", "", trades_tsxv)
         )

#4.0 Export into csv ----
write.csv(dates_tbl, file = "test1.csv")
