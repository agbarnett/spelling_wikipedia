# 99_data_to_excel.R
# export main data to Excel format for sharing on github
# March 2025
library(openxlsx)

# get the data
load('data/7_analysis_data.RData')

# export
outfile = 'data/7_analysis_data.xlsx'
wb <- createWorkbook(title = 'Analysis data of case and control abstracts', creator = 'Adrian Barnett', subject = 'created by 4_export_comments.R')
addWorksheet(wb, sheet = 'data', gridLines = TRUE)
freezePane(wb, sheet = 'data')
writeData(wb, sheet = 'data', x = data, startRow = 1)

# add data dictionary
dictionary = read.table(header=TRUE, sep='!', text='
label!explanation
pmid!PubMed ID
type!Article type
typeo!Article type with fewer categories
journal!Journal name
publisher!Publisher
retactred!Retracted
open_access!Open access
citations!Number of citations
word_count_title!Word count for title
word_count_abstract!Word count for abstract
word_count!Word count for title and abstract combined
n_languages!Number of detected languages
n_authors!Number of authors
country!Country code for first author
case!0 = case abstract with spelling errors, 1 = control abstract without spelling errors
alex_date!Date from OpenAlex
date_extracted!Date queried from OpenAlex, so date citations were counted
year!Year
error!Spelling error, NA for control abstracts
pubmed_date!Date from PubMed
word_count_pubmed!Word count from PubMed for cross-checking
in_title!Is there a spelling error in the title
in_abstract!Is there a spelling error in the abstract
')
addWorksheet(wb, sheet = 'dictionary', gridLines = TRUE)
writeData(wb, sheet = 'dictionary', x = dictionary, startRow = 1)

# save
saveWorkbook(wb, outfile, overwrite = TRUE)
