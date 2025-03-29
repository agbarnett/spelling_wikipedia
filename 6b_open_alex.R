# 6b_open_alex.R
# re-run for some excluded results (after 6_open_alex.R)
# Feb 2025
library(dplyr)
library(stringr)
library(rcrossref) # for pmid to DOI converter
library(rentrez)
library(countrycode)
library(openalexR) # have added mailto in .Rprofile; for country codes
source('0_my_pubmed_key_do_not_share.R')
source('99_functions.R') # for null_na and affiliation; and main function

# get case and control date, from 5_concatenate_case_control.R
load('data/5_case_control.RData')
# get address data, from 0_address_data.R (`address_info`)
load('data/0_address.RData')

# get list of RDs files to read in
directory = 'open_alex'
files = dir(directory, pattern = '^ex_') # just excluded
N = length(files)

# concatenate the exclusions
excluded = NULL
for (k in 1:N){
  infile = paste(directory, '/', files[k], sep='')
  result = readRDS(infile)
  excluded = bind_rows(excluded, result)
}

# excluded for not being in OpenAlex
no_alex = filter(excluded, reason == 'No data in OpenAlex') %>% 
  pull(pmid)

# loop through PMIDs for those with no openalex data
excluded_again = NULL
for (this_pmid in no_alex){
  
  # data row
  this_row = filter(data, pmid == this_pmid)[1,] # just take first row for multiple errors
  
  # convert PMID to DOI:
  rec = id_converter(this_pmid)$records
  if(is.null(rec$doi)){
    eframe = data.frame(pmid = this_pmid, case = this_row$case, reason = 'No PMID conversion')
    excluded_again = bind_rows(excluded_again, eframe)
    next # skip to next
  }
  
  # check OpenAlex data for a single DOI:
  frame = get_abstract_data_open_alex(id = rec$doi, 
                                      address_info = address_info, 
                                      type = 'doi',
                                      pmid = this_pmid)
  
  # outfile
  outfile = paste(this_pmid, '.rds', sep='') # 
  outfile = paste(directory, '/', outfile, sep='') # update outfile to include folder
  
  #
  if(is.null(frame)){ # exclude if no available data in OpenAlex, e.g., 29320037
    eframe = data.frame(pmid = this_pmid, case = this_row$case, reason = 'No data in OpenAlex')
    excluded_again = bind_rows(excluded_again, eframe)
    next # skip to next PMID
    # do not need to write rds file
  }
  if(is.na(frame$language) == FALSE){
    if(frame$language != 'en'){ # exclude if not English, e.g., 34644882 which is in English and French
      outfile = paste(directory, '/ex_', this_pmid, '.rds', sep='') # update outfile to include 'ex' for excluded
      eframe = data.frame(pmid = this_pmid, case = this_row$case, reason = 'Language other than English')
      saveRDS(eframe, file = outfile)
      next # skip to next PMID
    }
  }
  
  # select key existing data to carry-over - do not add error date -> done in next stage due to potential multiple errors per abstract
  frame$pmid = this_pmid
  frame$case = this_row$case
  frame$year = this_row$year
  frame$date = this_row$date
  word_count_pubmed = ifelse(is.na(this_row$word_count_pubmed), 0, this_row$word_count_pubmed)
  frame$word_count_pubmed = word_count_pubmed # for comparison to flag problem abstracts
  frame$date_extracted = as.Date(Sys.Date()) # date of extraction, needed for citations
  # save
  saveRDS(frame, file = outfile)

  # delete excluded version of file
  outfile = paste('ex_', this_pmid, '.rds', sep='') # 
  outfile = paste(directory, '/', outfile, sep='') # update outfile to include folder
  file.remove(outfile)
  
}


