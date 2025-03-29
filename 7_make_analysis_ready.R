# 7_make_analysis_ready.R
# put open alex data back together to make analysis data
# March 2025
library(dplyr)
library(stringr)

# get original data for checking, from 5_concatenate_case_control.R
load('data/5_case_control.RData')
original = data # rename to avoid over-write

# get list of RDs files to read in (files made by 6_open_alex.R and 6b_open_alex.R)
directory = 'open_alex'
files = dir(directory, pattern = '\\.rds')
N = length(files)
# check for any duplicates
f = str_remove_all(files, 'ex_|\\.rds')
any(duplicated(f)) # should be false

# concatenate the data and exclusions
data = excluded = NULL
for (k in 1:N){
  infile = paste(directory, '/', files[k], sep='')
  result = readRDS(infile)
  if(!str_detect(files[k], pattern = '^ex_')){ # file not excluded 
    data = bind_rows(data, result)
    result = NULL # safety net
    next # skip exclude if in data, as there were some re-runs for initially excluded
  }
  if(str_detect(files[k], pattern = '^ex_')){ # file excluded
    excluded = bind_rows(excluded, result)
    result = NULL # safety net
  }
}

## PMID check
# list of PMIDs that should be present in the new data:
pmids = dplyr::select(original, pmid) %>%
  unique() %>%
  pull(pmid)
# list of PMIDs that were in the data or excluded:
pmids_included = c(unique(data$pmid), unique(excluded$pmid))
pmids_missing = pmids[pmids %in% pmids_included == FALSE]
if(length(pmids_missing)>0){cat('Warning, missing ', length(pmids_missing),' PMIDs, e.g, ' , sample(pmids_missing, 1), '.', sep='')}

## merge OpenAlex data with errors
original = rename(original, 'pubmed_date' = 'date') # rename date
data = rename(data, 'alex_date' = 'date') %>% select(-year, - word_count_pubmed) # rename date, don't need year, can use pubmed count from data as has more in controls
# only PMIDs that were not excluded
original = filter(original, pmid %in% data$pmid)
n1 = nrow(original)
data = left_join(data, original, by=c('pmid','case')) # 
n2 = nrow(data)
if(n1 != n2){stop('Data merging error.\n')}

# fix for control abstracts that have no abstract and hence errors cannot be detected:
data = mutate(data, in_abstract = ifelse(word_count_abstract==0, NA, in_abstract))
# combine some small article types
data = mutate(data, 
               typeo = case_when(
                 type == 'dataset' ~ 'Other',
                 type == 'reference-entry' ~ 'Other',
                 type == 'book' ~ 'Other',
                 type == 'book-chapter' ~ 'Other',
                 type == 'other' ~ 'Other',
                 type == 'paratext' ~ 'Other',
                 type == 'supplementary-materials' ~ 'Other',
                 TRUE ~ str_to_title(type) # otherwise just change case
               ),
               publisher = case_when(
                 publisher == 'Q15088586' ~ 'Magnolia Press', # fix quirk from OpenAlex
                 publisher == 'Q16635223' ~ 'Sociedad Medica de Santiago', # fix quirk from OpenAlex
                 publisher == 'Q26794415' ~ 'Ingenta', # fix quirk from OpenAlex
                 publisher == 'National Institutes of Health' ~ 'Missing', # NIH is actually missing (default)
                 is.na(publisher) ~ 'Missing', # change to category
                 TRUE ~ as.character(publisher) # otherwise
               ),
               country = case_when(
                 is.na(country) ~ 'Missing', # change to category
                 TRUE ~ as.character(country) # otherwise
               )) %>%
  select(-language) # not needed as is always 'en'

# exclude one abstract with crazy word count
add_to_exclude = filter(data, word_count>5000) %>%
  select(pmid, case) %>%
  mutate(reason = 'Word count over 5000')
excluded = bind_rows(excluded, add_to_exclude)
data = filter(data, word_count <= 5000) # now remove from data

# exclude two abstracts with zero word count
add_to_exclude = filter(data, word_count==0) %>%
  select(pmid, case) %>%
  mutate(reason = 'Word count of zero')
excluded = bind_rows(excluded, add_to_exclude)
data = filter(data, word_count > 0) # now remove from data

# minor fix for publisher found by manual check
# trends journals from https://en.wikipedia.org/wiki/Cell_Press; also found some others
tjournals = c('Trends in Biochemical Sciences',
'Trends in Biotechnology',
'Trends in Cancer',
'Trends in Cell Biology',
'Trends in Chemistry',
'Trends in Cognitive Sciences',
'Trends in Ecology & Evolution',
'Trends in Endocrinology & Metabolism',
'Trends in Genetics',
'Trends in Immunology',
'Trends in Microbiology',
'Trends in Molecular Medicine',
'Trends in Neurosciences',
'Trends in Parasitology',
'Trends in Pharmacological Sciences',
'Trends in Plant Science',
"One Earth",
'Structure',
'Heliyon',
'Patterns',
'STAR protocols',
'Med')
jsearch = paste('^', tolower(tjournals), '$', collapse='|', sep='')
f1 = filter(data, str_detect(tolower(journal), jsearch)) %>%
  mutate(publisher = 'Cell Press') # use cell press as publisher
f2 = filter(data, !str_detect(tolower(journal), jsearch)) # other papers
f3 = filter(data, is.na(journal))
data = bind_rows(f1, f2, f3)

## remove duplicate errors due to different years from dates
# only needed for cases, if there are two controls that means they've been selected twice for 2+ cases
controls = filter(data, case == 0) 
cases = filter(data, case == 1) %>%
  select(-year) %>% # drop old year
  mutate(year = as.numeric(format(pubmed_date, '%Y'))) %>% # recalculate year based on pubmed
  unique()
data = bind_rows(controls, cases)

## fix few years with odd dates for a small number of controls, these are dates than just went into neighbouring years
data = mutate(data, 
    year = ifelse(year <= 2007, 2008, year), 
    year = ifelse(year == 2025, 2024, year)) 

# save
save(data, excluded, file = 'data/7_analysis_data.RData')
