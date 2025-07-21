# 1_pubmed_denominator.R
# get yearly denominator counts of all abstracts from PubMed
# July 2025
library(rentrez)
library(dplyr)
source('0_my_pubmed_key_do_not_share.R')

# get the denominator per year of all abstracts
years = 2008:2024
denom = NULL
for (y in years){
  query = paste(y, '[pdat] AND English[Language]', sep ='')
  res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key)
  frame = data.frame(year = y, denom = res$count)
  denom = bind_rows(denom, frame)
}

## Publication types ##
# Manually allocate publication types
pub_types = read.table(header=TRUE, sep='!', stringsAsFactors = FALSE, text='
type!typeo
Adaptive Clinical Trial!Article
Case Reports!Article
Classical Article!Article
Clinical Study!Article
Clinical Trial!Article
Clinical Trial, Phase I!Article
Clinical Trial, Phase II!Article
Clinical Trial, Phase III!Article
Clinical Trial, Phase IV!Article
Clinical Trial Protocol!Article
Clinical Trial, Veterinary!Article
Comparative Study!Article
Controlled Clinical Trial!Article
Dataset!Other
Editorial!Editorial
Equivalence Trial!Article
Evaluation Study!Article
Guideline!Other
Introductory Journal Article!Article
Journal Article!Article
Letter!Letter
Meta-Analysis!Review
Multicenter Study!Article
Observational Study!Article
Observational Study, Veterinary!Article
Preprint!Preprint
Pragmatic Clinical Trial!Article
Published Erratum!Erratum
Randomized Controlled Trial!Article
Retracted Publication!Retraction
Retraction of Publication!Retraction
Review!Review
Systematic Review!Review
Twin Study!Article
Validation Study!Article') %>%
  filter(typeo %in% c('Article','Editorial','Letter','Review')) # just four common types

# get the denominator by article type
denom_type = NULL
types = pub_types$type
for (y in years){
  for (t in types){
    query = paste(y, '[pdat] AND English[Language] AND ', t, '[ptyp]', sep ='')
    res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key)
    frame = data.frame(year = y, type = t, denom = res$count)
    denom_type = bind_rows(denom_type, frame)
  }
}
# add in broad types and get yearly counts
denom_type = left_join(denom_type, pub_types, by = 'type') %>%
  group_by(year, typeo) %>%
  summarise(denom = sum(denom)) %>%
  ungroup()

# save
save(denom, denom_type, years, file = 'data/1_denominator.RData')

