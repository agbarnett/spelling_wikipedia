# 99_functions
# general functions

# null_na.R
# function to change null to missing, used by 6_open_alex.R
null_na = function(input){
  not_there = is.null(input)
  if(not_there == TRUE){
    ret = NA
  }
  if(not_there == FALSE){
    ret = input
  }
  return(ret)
}


# get_affiliation, used by 6_open_alex.R
# function to extract country from affiliation text; copied from my baseline_tables repository
get_affiliation = function(address, address_info){
  
  # start blank
  affiliation = NA
  
  # 1) search for country
  potential = str_locate_all(address, pattern = address_info$countries)
  match = which(lapply(potential, length) > 0) # which elements of the list match 
  if(length(match) > 0){
    all = NULL
    for (m in match){
      f = as.data.frame(potential[[m]]) %>% mutate(country = address_info$countries[m])
      all = bind_rows(all, f)
    }
    affiliation = arrange(all, start) %>% slice(1) %>% pull(country) # first country in address
  }
  
  # 2) if still empty look for email in address
  if(is.na(affiliation) == TRUE){ 
    email = str_split(str_split(string=address, '@')[[1]][2], ' ')[[1]][1] # get first email address after the @
    if(is.na(email) == FALSE){
      to_search = paste(address_info$emails$email, '\\b', sep='')
      potential = str_extract_all(email, pattern = to_search)
      match = which(lapply(potential, length)>0) # which elements of the list match 
      if(length(match)>0) {
        affiliation = address_info$emails$country[match]
        if(length(affiliation)>1) {
          affiliation = affiliation[length(affiliation)] # take last one if multiple, assuming last bit of email is country, e.g., 9629531 
        }
      }
    } # 
  }
  
  # 3) if still empty look for US state
  if(is.na(affiliation) == TRUE){ 
    state_search = paste('\\b', paste(c(address_info$states$state, address_info$states$postal), collapse='\\b|\\b'), '\\b', sep='') # state names and postcodes, whole words only
    potential = str_extract_all(address, pattern = state_search) # search for state name or postcode
    match = which(lapply(potential, length)>0) # which elements of the list match 
    if(length(match) > 0){affiliation = 'USA'} # if any matches then must be USA
  }
  
  # 4) if still empty try capital cities
  if(is.na(affiliation) == TRUE){ 
    capitals = mutate(address_info$capitals, city_town = paste('\\b', city_town, '\\b', sep='')) # whole words only
    # search for capital
    potential = str_locate_all(address, pattern = capitals$city_town)
    match = which(lapply(potential, length) > 0) # which elements of the list match 
    if(length(match) > 0){
      all = NULL
      for (m in match){
        f = as.data.frame(potential[[m]]) %>% mutate(country = capitals$country[m])
        all = bind_rows(all, f)
      }
      affiliation = arrange(all, start) %>% slice(1) %>% pull(country) # first country in address
    }
  }
  
  # last step: if there's an affiliation, then convert to its countrycode
  if(is.na(affiliation) == FALSE){
    affiliation = countrycode(affiliation, origin = 'country.name', destination = 'iso2c')
  }
  
  # return
  return(affiliation)
} # end of function

##
## function to extract key details for a single PMID, used by 6[b]_open_alex.R ##
get_abstract_data_open_alex = function(id, address_info, type = 'pmid', pmid = NULL){
  # part 1: using openalex
  if(type == 'pmid'){
    single_work <- oa_fetch(
      entity = "works",
      pmid = id,
      verbose = FALSE
    )
    this_pmid = id
  }
  if(type == 'doi'){
    single_work <- oa_fetch( # 
      entity = "works",
      doi = id,
      verbose = FALSE
    )
    # update PMID if using DOI
    this_pmid = pmid
  }
  # rare null result
  if(is.null(single_work)){
    return(NULL)
  }
  
  # take first record for rare doubles
  single_work = single_work[1, ]
  # extract key data
  type = null_na(single_work$type) # publication type
  publisher =  null_na(single_work$host_organization) # is not always the publisher
  journal = null_na(single_work$so) # 
  language = null_na(single_work$language)
  retracted = null_na(single_work$is_retracted)
  open_access = null_na(single_work$is_oa)
  citations = null_na(single_work$cited_by_count)
  # add highest scoring subfield and field (see here: https://help.openalex.org/hc/en-us/articles/24736129405719-Topics)
  subfield = filter(single_work$topics[[1]], name =='subfield') %>% arrange(desc(score)) %>% slice(1) %>% pull(id)
  subfield = str_remove(subfield, pattern = 'https://openalex.org/subfields/') # just get subfield ID number
  single_work$subfield = subfield
  field = filter(single_work$topics[[1]], name =='field') %>% arrange(desc(score)) %>% slice(1) %>% pull(id)
  field = str_remove(field, pattern = 'https://openalex.org/fields/') # just get field ID number
  single_work$field = field
  # add first author institution
  institution = single_work$author[[1]][1,] %>% pull(au_id)
  institution = str_remove(institution, pattern = 'https://openalex.org/') # just get ID number
  single_work$institution = institution
  
  # depends if there are authors or not ...
  no_authors = is.na(single_work$author[[1]])[1]
  if(no_authors == TRUE){ # if no authors
    author = NA
    country = NA
    n_authors = 0
  }
  if(no_authors == FALSE){
    author = single_work$author[[1]]
    country = author$institution_country_code[1] # just first author; can be multiple as multiple authors
    n_authors = nrow(author) # number of authors
    
    # if country is missing then guess from affiliation
    if(is.na(country) == TRUE & is.null(author$au_affiliation_raw) == FALSE){
      address = author$au_affiliation_raw[1]
      if(!is.na(address)){country = get_affiliation(address, address_info)}
    }
  }
  
  # part 2: using rentrez as abstract is not always available for openalex
  recs <- entrez_fetch(db="pubmed", id = this_pmid, rettype="xml") # get from pubmed
  # separate code for rare abstracts in Book format
  title = abstract = ''
  if(str_detect(recs, pattern= 'PubmedBookArticle') == TRUE){
    # title
    start = str_locate(recs, '\\<ArticleTitle(.*?)\\>')
    end = str_locate(recs, '\\</ArticleTitle')
    if(!is.na(start)[1] & !is.na(end)[1]){title = str_sub(recs, start[2]+1, end[1]-1)}
    # abstract
    start = str_locate(recs, '\\<AbstractText(.*?)\\>')
    end = str_locate(recs, '\\</AbstractText')
    if(!is.na(start)[1] & !is.na(end)[1]){abstract = str_sub(recs, start[2]+1, end[1]-1)}
  }
  # code for standard abstracts
  if(str_detect(recs, pattern ='PubmedBookArticle') == FALSE){
    p = parse_pubmed_xml(recs)
    title = p$title
    abstract = p$abstract
  }
  
  # removing languages other than English
  languages = str_extract_all(recs,  '(?<=Language=\")[a-z]{3}')[[1]]
  n_languages = length(languages) # number of languages other than English
  if(n_languages>0){
    # remove non-English abstracts
    abstract = abstract[1] # assuming one position per language and that English is first
  }
  
  # word counts
  abstract = paste(abstract, collapse=' ')
  title_abstract = paste(title, abstract, collapse=' ') 
  word_count_title = str_count(title, "\\w+") # title word count
  word_count_abstract = str_count(abstract, "\\w+") # abstract word count
  if(length(abstract) ==0){word_count_abstract = 0}
  word_count = word_count_title + word_count_abstract
  
  # if country is missing then try again using rentrez result
  if(is.na(country)){
    # extract the affiliation
    start = str_locate(recs, '\\<Affiliation(.*?)\\>')
    end = str_locate(recs, '\\</Affiliation')
    if(!is.na(start)[1] & !is.na(end)[1]){
      address = str_sub(recs, start[2]+1, end[1]-1)
      if(!is.na(address)){country = get_affiliation(address, address_info)} # from get_affiliation_R.
    }
  }
  
  # if publisher is missing then try again using rentrez result
  if(is.na(publisher)| publisher == 'National Institutes of Health' | publisher ==''){
    # extract the publisher
    start = str_locate(recs, '\\<PublisherName(.*?)\\>')
    end = str_locate(recs, '\\</PublisherName')
    if(!is.na(start)[1] & !is.na(end)[1]){publisher = str_sub(recs, start[2]+1, end[1]-1)}
  }
  
  # if journal is missing then try again using rentrez result
  if(is.na(journal) | journal == 'PubMed'){
    # extract the journal
    start = str_locate(recs, '\\<Title\\>')
    end = str_locate(recs, '\\</Title')
    if(!is.na(start)[1] & !is.na(end)[1]){journal = str_sub(recs, start[2]+1, end[1]-1)}
  }
  
  # return combined data
  frame = data.frame(type = type,
                     journal = journal,
                     publisher = publisher,
                     language = language,
                     retracted = retracted,
                     open_access = open_access,
                     citations = citations,
                     word_count_title = word_count_title, 
                     word_count_abstract = word_count_abstract, 
                     word_count = word_count, 
                     n_languages = n_languages, 
                     n_authors = n_authors,
                     country = country) # 
  
  # return
  return(frame)
  
}


## repeated processing of country name
my_country_long = function(country){
  country_long = countrycode(country, # add longer country labels
                             origin ='genc2c', 
                             destination ='country.name')
  country_long = str_remove_all(country_long, pattern='[A-Z]{3} [A-Z|a-z]{5}') # tidy
  country_long = case_when(
    country == 'Missing' ~ 'Missing',
    country == 'Other' ~ 'Other',
    TRUE ~ as.character(country_long)
  )
  country_long = str_squish(country_long)
  return(country_long)
}
