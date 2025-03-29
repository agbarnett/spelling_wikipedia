# 5_concatenate_case_control.R
# concatenate the case and control data
# February 2025
library(dplyr)

# get the case data, from 3_expunge_acronyms_names.R
load('data/3_pubmed.RData')
case_data = data
remove(excluded) # not needed

# loop and read in the control results
directory = 'control_results'
control_search_results = dir(directory, pattern='\\.rds')

# concatenate the data 
data = no_data = NULL
for (file in control_search_results){ # big loop
  infile = paste(directory, '/', file, sep='')
  result = readRDS(infile)
  empty = is.null(result) | !'pmid' %in% names(result) # empty result or with no results for this error
  if(empty == TRUE){ 
    date = str_remove_all(file, pattern = '\\.rds')
    frame = data.frame(date = date)
    no_data = bind_rows(no_data, frame)
  }
  if(empty == FALSE){
    data = bind_rows(data, result)
  }
}

## checks
# should be null
nrow(no_data)

# check that all cases are covered
for_join = filter(data, case == 1) %>% select(pmid, error)
check = anti_join(case_data, for_join, by=c('pmid','error')) # rows in original case data that are not in case/control data 
warning = NULL
if(nrow(check)>0){
  paste('Warning, missing case data in case-control. There are ', nrow(check), ' cases missing.', sep='')
}
# check case proportion
prop.table(table(data$case))

# save
save(data, file='data/5_case_control.RData')
