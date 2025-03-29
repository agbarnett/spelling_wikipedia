# 1_pubmed_denominator.R
# get yearly denominator counts of all abstracts from PubMed
# February 2025
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

# save
save(denom, years, file = 'data/1_denominator.RData')
