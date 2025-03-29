# 4_sample_controls.R
# get random sample of control abstracts without errors matched by date
# February 2025
library(dplyr)
library(rentrez)
library(stringr)
source('0_my_pubmed_key_do_not_share.R')
TeachingDemos::char2seed('stags')

# get the case data, from 3_expunge_acronyms_names.R
load('data/3_pubmed.RData')
remove(excluded) # not needed

# basics:
n_controls = 2 # controls per case
directory = 'control_results' # where to save results


## function to run one match
random_sample = function(indata,  
                         directory, # directory to store RDS results
                         all_errors_pmids, # list of all case PMIDs
                         d, # date
                         n_controls) # number of controls
{
  
  # save results in this file
  outfile = paste(directory, '/', d, '.rds', sep='')
  
  # create PubMed search
  mindate = format(as.Date(d-2), '%Y/%m/%d') # two days before ...
  maxdate = format(as.Date(d+2), '%Y/%m/%d') # ... two days after
  date_text = paste(format(as.Date(mindate), '%Y/%m/%d'), ':',
                    format(as.Date(maxdate), '%Y/%m/%d'), '[edat]', sep='') # using edat to match entrez date (which is the case date)
  query = paste(date_text, " AND English[Language]", sep='') # 
  res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key, retmax=9999 )
  case_date = filter(indata, date == d) 
  n_cases = nrow(case_date) # number of cases
  if(n_cases==0){
    stop(paste('Error, date with no cases, date = ', d, '.\n'), sep='')
  }
  if(res$count <= n_controls*n_cases){
    stop("error, insufficient controls for date ", d, ', matches =', res$count,'case numbers = ', n_cases, '\n', sep='')
  }
  # get all PMIDs from search
  pmids = res$ids
  # knock out control IDs that are also cases (with spelling errors)
  pmids = pmids[!pmids %in% all_errors_pmids] # use all data here for all errors
  # randomly sample controls
  pmid_sample = sample(pmids, size = n_controls * n_cases, replace = FALSE) 
  # make frame with year and date
  frame = data.frame(case = 0, # not a case with a spelling error
                     year = as.numeric(format(as.Date(d), '%Y')),
                     date = as.Date(d), # use this date, will only be a day or two out at max
                     error = NA,
                     in_title = FALSE,
                     in_abstract = FALSE, # may be NA for papers without an abstract  - fixed in later file
                     pmid = pmid_sample)
  frame = bind_rows(case_date, frame) # add case date
  
  # save to file because of time-outs
  saveRDS(frame, file = outfile)
  
}


## big loop through all case dates ##
# refine existing files list - remember to update after breaks
files = dir(directory, pattern='\\.rds')
files = as.numeric(str_remove_all(files, pattern='\\.rds'))

# loop through every observation in the "case" data to find controls
data = mutate(data, case = 1) # make all cases
dates = unique(data$date)
# list of all PMIDs with a spelling error (used to exclude cases from controls)
all_errors_pmids = unique(data$pmid)

# loop through all dates
for (d in dates){ # 
  
  Sys.sleep(1) # to avoid time-outs
  
  # check if control data already exists
  exists = as.numeric(d) %in% files # compared as numbers
  if(exists == TRUE){
    next # move on to next date
  }
  
  # run the matching if the results don't already exist
  random_sample(data, directory, all_errors_pmids, d, n_controls)
}

# check that all dates are there
okay = as.numeric(dates) %in% files
table(okay)

# no saving needed, done using rds files
