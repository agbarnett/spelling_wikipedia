# 3_random_checks_errors.R
# randomly sample errors and check if they are actual errors; see file in checks sub-folder
# Feb 2025
library(rmarkdown)
library(openxlsx)
library(dplyr)
TeachingDemos::char2seed('mansfield')

# get the data, from 3_expunge_acronyms_names.R
load('data/3_pubmed.RData')

## part 1: randomly sample ##
n_check = 200
sample = sample_n(data, size = n_check*4, replace = FALSE) %>% # over-sample as multiple examples of the same error can be selected
  select(pmid, error) %>%
  group_by(error) %>% 
  slice(1) %>% # just one example per error
  ungroup() %>%
  sample_n(size = n_check, replace=FALSE) %>%
  arrange(error, pmid) # grouping errors together makes checking easier

# export checks to Excel for manual checking
excel_file = 'checks/errors_manually_check_not_complete.xlsx' # not_complete version; remove when done
wb = createWorkbook()
addWorksheet(wb, sheetName = 'errors_to_check')
writeDataTable(wb, sheet = 1, x = sample, colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, excel_file, overwrite = TRUE) 


## part 2: report on manual error checks ##
# after doing manual checks
render(input = "3_manual_checks.Rmd",
       output_format = 'word_document',
       output = "checks/3_manual_checks_summary.docx")
