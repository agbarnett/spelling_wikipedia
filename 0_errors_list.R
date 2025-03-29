# 0_errors_list.R
# get the list of errors from Wikipedia and our stats-specific errors
# remove non-errors, add potential names
# February 2025
library(dplyr)
library(readxl)
library(rentrez)
library(stringr)
source('0_my_pubmed_key_do_not_share.R')

# key files:
errors_file = 'data/wikipedia_list.xlsx' # list of errors from Wikipedia
errors_file_additional = 'data/stats_errors.txt' # shorter list of statistical errors
non_errors_file = 'data/checks_complete.xlsx' # my checks of what errors should be removed

# record numbers excluded for flow chart
numbers = NULL

## read in the long list of errors
# original url = 'https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines'
errors1 = read_excel(errors_file, col_types = 'text')
# get additional statistical errors from the LSE blog
errors2 = read.table(errors_file_additional, sep='!', header=FALSE, comment.char='', quote='\"', skip=1) %>% 
  rename('error' = 'V1',
         'correct' = 'V2') 
errors = bind_rows(errors1, errors2) %>%
  mutate(error = str_remove(error, '-$'),
         error = str_squish(error)) %>%
  unique() %>% # safety net for duplicates
  mutate(exclude = ifelse(is.na(exclude), 'no', exclude))

# update numbers
frame = data.frame(reason = 'Wikipedia list', n = nrow(errors1))
numbers = bind_rows(numbers, frame)
frame = data.frame(reason = 'Statistical errors', n = nrow(errors2))
numbers = bind_rows(numbers, frame)

## remove non-errors
# manual checks found that these are not errors
to_remove_1 = read_excel(non_errors_file) %>%
  filter(exclude == 'yes') %>%
  mutate(error = str_squish(error)) %>% # remove any rogue spaces
  pull(error)
# update numbers
n_start = nrow(errors)
errors = filter(errors,
                #exclude != 'yes', # exclusions found by trial and error
                !error %in% to_remove_1) # exclusions found by checking common errors
n_removed = n_start - nrow(errors)
frame = data.frame(reason = 'Removed by manual checks', n = n_removed)
numbers = bind_rows(numbers, frame)

## search did not work with apostrophes
to_remove_2 = filter(errors, str_detect(correct,"'")) %>% pull(error)
# update numbers
n_start = nrow(errors)
errors = filter(errors, !error %in% to_remove_2)
n_removed = n_start - nrow(errors)
frame = data.frame(reason = 'Removed due to apostrophes', n = n_removed)
numbers = bind_rows(numbers, frame)

## did not get hyphens
to_remove_3 = filter(errors, str_detect(correct,"-")) %>% pull(error)
to_remove_4 = filter(errors, str_detect(error,"-")) %>% pull(error)
to_remove_3_4 = c(to_remove_3, to_remove_4)
# update numbers
n_start = nrow(errors)
errors = filter(errors, !error %in% to_remove_3_4)
n_removed = n_start - nrow(errors)
frame = data.frame(reason = 'Removed due to hyphens', n = n_removed)
numbers = bind_rows(numbers, frame)

## potential acronyms
to_remove_5 = filter(errors, nchar(error) <= 4) %>% pull(error)
# update numbers
n_start = nrow(errors)
errors = filter(errors, !error %in% to_remove_5)
n_removed = n_start - nrow(errors)
frame = data.frame(reason = 'Removed due to acronyms', n = n_removed)
numbers = bind_rows(numbers, frame)

## safety net for doubles 
errors = unique(errors) %>%
  select(-notes, -exclude) # no longer needed

## add variable that flags potential surnames based on a pubmed search of author names (used by expunge_acronyms_names)
errors = mutate(errors, name_search = 0) 
for (k in 1:nrow(errors)){
  query = paste(errors$error[k], '[author]', sep ='')
  res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key, retmax=0)
  errors$name_search[k] = res$count
}
# add variable depending on search numbers
errors = mutate(errors, potential_name = name_search > 0) 
# hard-code two unlikely names
index = errors$error == 'fourty' # only two surnames and lots of errors so override
errors$potential_name[index] = FALSE
index = errors$error == 'Kaplan Meir' # only a name for `Kaplan`
errors$potential_name[index] = FALSE

# save
save(errors, numbers, file = 'data/0_errors_list.RData')
