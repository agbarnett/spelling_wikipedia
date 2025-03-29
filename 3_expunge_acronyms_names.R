# 3_expunge_acronyms_names.R
# remove acronyms, names, and non-English words using rentrez
# also adds entrez publication date: the date the citation was added to the PubMed database.
# Feb 2025
library(dplyr)
library(rentrez)
library(stringr)
library(tidyr)
source('0_my_pubmed_key_do_not_share.R')

## get data ##
# a) get errors
load('data/0_errors_list.RData') # from 0_errors_list.R
# hard code space/hyphen for `Kaplan Meir` and `odd ratio`
index = which(errors$error == 'Kaplan Meir')
errors$error[index] = 'Kaplan.Meir'
index = which(errors$error == 'Kapan Meier')
errors$error[index] = 'Kapan.Meier'
index = which(errors$error == 'odd ratio')
errors$error[index] = 'odd.ratio'
index = which(errors$error == 'odds ration')
errors$error[index] = 'odds.ration'
remove(numbers) # not needed

# b) pubmed searches
load('data/2_pubmed.RData')
remove(zeros) # not needed

# work out number of loops
break_size = 350
n_loops = floor(nrow(data) / break_size) + 1

# first loop to break up the run
outdata = excluded = NULL # start empty
for (k in 1:n_loops){
  start = ((k-1)*break_size)+1
  stop = k*break_size
  stop = min(stop, nrow(data)) # for last loop
  to_expunge = data[start:stop, ]
  
  # get data from PubMed
  ids = unique(to_expunge$pmid) # some doubles with multiple spelling errors, so do unique to save time
  recs = entrez_fetch(db = 'pubmed', id = ids, rettype="xml", parsed=TRUE) # get from pubmed in batches of multiple PMIDs
  recs_list <- XML::xmlToList(recs, simplify=FALSE) # make into a list
  
  # loop through individual abstracts
  pmid_check = NULL
  for (j in 1:length(recs_list)){ 
    
    # pull out key data
    p = recs_list[[j]]
    
    # separate code for rare abstracts in Book format, e.g. ?
    if(names(p)[1] =="BookDocument"){
      this_pmid = p$BookDocument$PMID$text
      title = paste(p$BookDocument$Book$BookTitle$text, collapse= ' ')
      ab = p$BookDocument$Abstract
      Date_list = p$PubmedBookData$History # list of all dates
    }
    if(names(p)[1] !="BookDocument"){
      this_pmid = p$MedlineCitation$PMID$text
      title = paste(p$MedlineCitation$Article$ArticleTitle, collapse= ' ')
      ab = p$MedlineCitation$Article$Abstract
      if(is.null(ab)){
        ab = p$MedlineCitation$OtherAbstract
      }
      Date_list = p$PubmedData$History # list of all dates
    }
    
    # extract date from list
    Date = bind_rows(Date_list) %>%
      filter(.attrs == 'entrez') # need to use entrez date - does not always agree with year
    date = as.Date(with(Date, ISOdate(Year, Month, Day))) # convert date
    
    # process abstract, which can be a complex list
    if(length(ab)==1){
      abstract = str_squish(paste(ab$AbstractText, collapse=' '))
    }
    if(length(ab)>1){
      list_names = names(ab) # list names
      abstract = ''
      for (a in 1:length(ab)){
        # simple structure ...
        if(length(ab[[a]]) == 1){
          if(str_detect(list_names[a], 'text|Text')){abstract = paste(abstract, ab[[a]])}
        }
        # ... more complex structure
        if(length(ab[[a]]) > 1){
          index = which(str_detect(names(ab[[a]]), 'text|Text')) # avoids key words and copyright
          to_add = paste(ab[[a]][index], collapse = ' ')
          abstract = paste(abstract, to_add, sep=' ') # was a minor error here, fixed now 
        }
      }
      abstract = str_squish(paste(abstract, collapse = ' '))
    }

    # remove ldots, e.g., 23885751
    title = str_replace_all(title, '…', ' ')
    if(!is.null(abstract)){abstract = str_replace_all(abstract, '…', ' ')}

    # 
    to_check = filter(to_expunge, pmid == this_pmid)
    # make alternative cases for all errors
    to_check = mutate(to_check, 
                      error_title1 = str_to_title(error), # all capitals (needed because of errors with spaces, e.g. 'odd ratio')
                      error_title2 = str_to_sentence(error)) %>% # first capital
      unite("error_title", c('error', 'error_title1', 'error_title2'), sep='|', remove = FALSE) %>%
      select(-error_title1, -error_title2) # no longer needed
    
    # get matching PMID as fetch can mix up order
    if(nrow(to_check) == 0){
      stop("Error for PMID = ", this_pmid, '\n', sep='')
    }
    
    # abstract word count (for later cross-checking with OpenAlex)
    if(is.null(abstract)){word_count = 0}
    if(!is.null(abstract)){word_count = str_count(abstract, '\\w+')}
    
    
    ## loop per errors as some abstracts have multiple errors
    n_errors = nrow(to_check)
    for (i in 1:n_errors){
      
      ## check errors in titles and abstracts
      # find if error can be a name, meaning title case is not an error
      in_abstract = in_title = NA
      pattern = paste('\\b', to_check$error[i], '\\b', sep='') # whole word only for checking against error database
      pattern = str_replace_all(pattern, ' ', '.') # replace space with any character for search
      potential_name = filter(errors, str_detect(error, pattern)) %>% pull(potential_name)
      # now check against title and abstract
      if(potential_name == TRUE){ # potential names just in lower case ...
        in_title = str_detect(title, pattern = to_check$error[i]) 
        in_abstract = str_detect(abstract, pattern = to_check$error[i])
      }
      if(potential_name == FALSE){ # ... otherwise in either case
        in_title = str_detect(title, pattern = to_check$error_title[i]) # using original case 
        in_abstract = str_detect(abstract, pattern = to_check$error_title[i])
      }
      if(is.null(abstract) == TRUE){in_abstract = NA} # if no abstract 
      if(is.null(abstract) == FALSE){if(abstract==''){in_abstract = NA}} # if no abstract 
      # not checked keywords from PubMed as they don't match, e.g., 28101769
      
      # put data together (can work with multiple errors per abstract, e.g., 6534870)
      frame = mutate(to_check[i,],
                     date = date, # add date
                     word_count_pubmed = word_count, # keep this for comparison with openalex word count
                     in_title = in_title,
                     in_abstract = in_abstract) %>%
        select(-error_title) # no longer needed
      
      # add to data or excluded
      ex = filter(frame, in_title == FALSE & (is.na(in_abstract)|in_abstract == FALSE)) # use filter due to potential multiple error per abstract
      if(nrow(ex) > 0){
        excluded = bind_rows(excluded, ex)
      }
      can_add = filter(frame, in_title == TRUE | in_abstract == TRUE) # use filter due to potential multiple error per abstract
      if(nrow(can_add) > 0){
        outdata = bind_rows(outdata, can_add)
      }
      
    } # end of loop for multiple errors in the same abstract
    
    # clean up
    pmid_check = c(pmid_check, this_pmid)
    this_pmid = title = ab = abstract = can_add = ex = pattern = potential_name = NULL
  } # end of individual abstract loop
  
  # check that loop covered all abstracts
  if(length(intersect(pmid_check, ids)) != length(ids)){
    stop("error, missing PMIDs. Loop = ", k, "\n", sep='')
    # 
  }
}

## because of breaks
excluded = unique(excluded)
data = unique(data)

## check
check = nrow(excluded) + nrow(outdata) == nrow(data)
if(check==FALSE){cat('Error, numbers don`t add\n')}

## check word counts
summary(data$word_count_pubmed)
hist(data$word_count_pubmed)

## save
excluded = select(excluded, -in_abstract, -in_title) # always false, so not needed
data = outdata # rename for save
save(excluded, data, file = 'data/3_pubmed.RData')

