# 2_pubmed_concat.R
# run through each abstract to remove non-errors including names and acronyms; also adds publication date
# February 2025
library(dplyr)
library(stringr)

# directory of pubmed search results:
directory = 'pubmed_search_results'
pubmed_search_results = dir(directory, pattern = '.rds')

# loop through files and add to data or zeros
data = zeros = NULL
for (file in pubmed_search_results){
  infile = paste(directory, '/', file, sep='')
  result = readRDS(infile)
  empty = is.null(result) | !'pmid' %in% names(result) # empty result or with no results for this error
  if(empty == TRUE){ 
    word = str_remove_all(file, pattern = '\\.rds')
    frame = data.frame(error = word)
    zeros = bind_rows(zeros, frame)
  }
  if(empty == FALSE){
    if(any(is.na(result$pmid))){stop('Empty PMID for ', file, '.\n', sep='')}
    data = bind_rows(data, result)
  }
}

## check that all error are covered
load('data/0_errors_list.RData') # from 0_errors_list.R
all_errors = pull(errors, error)
errors_in_data = select(data, error) %>% unique() %>% pull(error)
errors_in_zeros = select(zeros, error) %>% unique() %>% pull(error)
errors_covered = c(errors_in_data, errors_in_zeros)
not_covered = all_errors[all_errors %in% errors_covered == FALSE]
if(length(not_covered)>0){
  example = sample(not_covered, size=1)
  cat("Warning: not all errors covered, e.g., ", example, ".", sep='')
}
# check there are no files in folder that should not be there
extra_covered = errors_covered[errors_covered %in% all_errors == FALSE]
if(length(extra_covered)>0){
  example = sample(extra_covered, size=1)
  cat("Warning: extra PubMed searches made, e.g., ", example, ".", sep='')
}

# save
save(data, zeros, file = 'data/2_pubmed.RData')
