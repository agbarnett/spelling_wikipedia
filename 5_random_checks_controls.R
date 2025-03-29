# 5_random_checks_controls.R
# randomly sample controls to check for any spelling errors
# Feb 2025
library(rentrez)
source('0_my_pubmed_key_do_not_share.R')
library(rmarkdown)
library(stringr)
library(dplyr)
TeachingDemos::char2seed('mansfield')

## part 1: randomly sample controls ##
# get case and control date, from 5_concatenate_case_control.R
n_check = 200
load('data/5_case_control.RData')
control_sample = filter(data, case == 0) %>% # controls only
  sample_n(size = n_check, replace = FALSE) %>%
  pull(pmid)


## part 2: get abstract from pubmed ##
recs = entrez_fetch(db = 'pubmed', id = control_sample, rettype="xml", parsed=TRUE) # get from pubmed in batches of multiple PMIDs
recs_list <- XML::xmlToList(recs, simplify=FALSE) # make into a list
# loop through individual abstracts
abstracts = list()
for (k in 1:n_check){ 
  
  # pull out key data
  p = recs_list[[k]]
  
  # separate code for rare abstracts in Book format, e.g. ?
  if(names(p)[1] =="BookDocument"){
    this_pmid = p$BookDocument$PMID$text
    title = paste(p$BookDocument$Book$BookTitle$text, collapse= ' ')
    ab = p$BookDocument$Abstract
  }
  if(names(p)[1] !="BookDocument"){
    this_pmid = p$MedlineCitation$PMID$text
    title = paste(p$MedlineCitation$Article$ArticleTitle, collapse= ' ')
    ab = p$MedlineCitation$Article$Abstract
    if(is.null(ab)){
      ab = p$MedlineCitation$OtherAbstract
    }
  }
  
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
        abstract = paste(abstract, to_add, sep=' ')
      }
    }
    abstract = str_squish(paste(abstract, collapse = ' '))
  }
  
  # concatenate and clean up
  mini_list = list()
  mini_list$pmid = this_pmid
  mini_list$title = title
  mini_list$abstract = abstract
  abstracts = c(abstracts, list(mini_list))
  this_pmid = title = ab = abstract = mini_list = NULL
} # end of individual abstract loop
# save for rmarkdown
save(n_check, abstracts, file='data/5_checks_controls.RData')

## part 3: make a report that can be opened in Word and run spell check ##
render(input = "5_check_controls.Rmd",
       output_format = 'word_document',
	   output_dir = 'checks',
       output_file = "5_check_controls.docx")
