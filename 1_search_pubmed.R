# 1_search_pubmed.R
# run the yearly search of pubmed for individual spelling errors
# save individual results in folder due to time-outs
# February 2025
library(rentrez)
library(dplyr)
library(stringr)
source('0_my_pubmed_key_do_not_share.R')

# get the list of errors, from 0_errors_list.R
load('data/0_errors_list.RData')
years = 2008:2024

# directory to store results:
directory = 'pubmed_search_results'
contents = dir(directory, pattern='\\.rds')
contents = str_remove_all(contents, pattern = '\\.rds')

# function to run one search
search_one_pubmed = function(e, years){
  
  # save results in this file
  outfile = paste('pubmed_search_results/', e, '.rds', sep='')
  
  # basics
  place = '[tiab]' # search in title and abstract
  time_text = paste(years[1], ':', years[length(years)], sep='') # paste full range of years
  
  # search for error in any years, if none then skip to next error (speeds up search)
  query = paste('"', e, '"', place, ' AND ', time_text, '[pdat] AND English[Language]', sep ='')
  res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key, retmax=9999 )
  if(res$count == 0){
    zframe = data.frame(error = e) # store zeros
    saveRDS(zframe, file = outfile) # saves null file
  }
  data = NULL
  if(res$count >= 0){
    for (y in years){
      query = paste('"', e, '"', place, ' AND ', y, '[pdat] AND English[Language]', sep ='')
      res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key, retmax=9999 )
      if(res$count == 0){
        next # skip to next year if no results
      }
      if(res$count > 9999){cat('overflow for year', y, ', term', e, '\n', sep='')}
      frame = data.frame(year = y, error = e, pmid = res$ids) # keep all PMIDs with error
      # errors can appear in multiple years, so just take first year
      frame = arrange(frame, pmid, year) %>%
        group_by(pmid) %>%
        slice(1) %>% # just take first error
        ungroup()
      #
      data = bind_rows(data, frame)
    }
  }
  
  # save results as a RDS file
  saveRDS(data, file = outfile)
}

## loop through all errors
for (e in errors$error){
  
  # check if error search already exists
  exists = e %in% contents
  if(exists == TRUE){
    next # move on to next error
  }
  
  # run the search if the results don't exist
  search_one_pubmed(e, years)
  
}

# no saving needed, done in folder

# check that folder contains all errors
contents = dir(directory, pattern='\\.rds')
if(length(contents) != nrow(errors)){cat('Error, some missing words.')}
any(duplicated(contents)) # must be false