# 6_open_alex.R
# get details for case and control abstracts from OpenAlex, including author country
# February 2025
library(dplyr)
library(stringr)
library(rentrez)
library(countrycode)
library(openalexR) # have added mailto in .Rprofile; for country codes
source('0_my_pubmed_key_do_not_share.R')
source('99_functions.R') # for null_na and affiliation; and main function

# get case and control date, from 5_concatenate_case_control.R
load('data/5_case_control.RData')
directory = 'open_alex'
# get address data, from 0_address_data.R
load('data/0_address.RData')

## loop through PMIDs ##

# refine existing files list, update after any crash
files = dir(directory, pattern='\\.rds')
files = str_remove(files, pattern = 'ex_|\\.rds$') # just the PMID

# just run once per PMID (can be multiple errors per abstract) and add back errors in next file
data = dplyr::select(data, pmid, case, year, date, word_count_pubmed) %>%
  arrange(pmid, year) %>%
  group_by(pmid) %>%
  slice(1) %>% # take earliest if two years
  ungroup() %>%
  unique()
N = nrow(data)
#table(table(data$pmid)) # check, should all be 1

# PMIDs to try (speed up process after re-runs)
index = !data$pmid %in% files
cat('Remaning = ', sum(index), '\n', sep='')

# big loop through PMIDs
for (k in which(index)){

  # exclude few PMIDs which no longer exists
  if(data$pmid[k] %in% c('39484944','39422428')){
    outfile = paste(directory, '/ex_', data$pmid[k], '.rds', sep='') # update outfile to include 'ex' for excluded
    eframe = data.frame(pmid = data$pmid[k], case = data$case[k], reason = 'No longer on PubMed')
    saveRDS(eframe, file = outfile)
    next # skip to next PMID
  }
    
  # check if result for this PMID already exists (safety net from above code)
  pattern = paste(data$pmid[k], '\\.', sep = '')
  exists = any(str_detect(pattern = pattern, files))
  if(exists == TRUE){
    next # move on to next PMID
  }
  outfile = paste(data$pmid[k], '.rds', sep='') # 
  outfile = paste(directory, '/', outfile, sep='') # update outfile to include folder
  
  # run for single PMID:
  frame = get_abstract_data_open_alex(data$pmid[k], address_info)
  #
  if(is.null(frame)){ # exclude if no available data in OpenAlex, e.g., 29320037
    outfile = paste(directory, '/ex_', data$pmid[k], '.rds', sep='') # update outfile to include 'ex' for excluded
    eframe = data.frame(pmid = data$pmid[k], case = data$case[k], reason = 'No data in OpenAlex')
    saveRDS(eframe, file = outfile)
    next # skip to next PMID
  }
  if(is.na(frame$language) == FALSE){
    if(frame$language != 'en'){ # exclude if not English, e.g., 34644882 which is in English and French
      outfile = paste(directory, '/ex_', data$pmid[k], '.rds', sep='') # update outfile to include 'ex' for excluded
      eframe = data.frame(pmid = data$pmid[k], case = data$case[k], reason = 'Language other than English')
      saveRDS(eframe, file = outfile)
      next # skip to next PMID
    }
  }
  
  # select key existing data to carry-over - do not add error date -> done in next stage due to potential multiple errors per abstract
  frame$pmid = data$pmid[k]
  frame$case = data$case[k]
  frame$year = data$year[k]
  frame$date = data$date[k]
  word_count_pubmed = ifelse(is.na(data$word_count_pubmed[k]), 0, data$word_count_pubmed[k])
  frame$word_count_pubmed = word_count_pubmed # for comparison to flag problem abstracts
  frame$date_extracted = as.Date(Sys.Date()) # date of extraction, needed for citations
  # save
  saveRDS(frame, file = outfile)
  
}

## no saving needed, done using RDS
