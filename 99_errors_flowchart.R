# 99_errors_flowchart.R
# flow chart of included spelling errors
# March 2025
library(diagram)
library(stringr)
library(dplyr)

## data
load('data/0_errors_list.RData') # from 0_errors_list.R for numbers
start_numbers = filter(numbers, str_detect(reason, 'Wikipedia'))%>%pull(n)
total_excluded = filter(numbers, str_detect(reason, '^Removed')) %>% summarise(n = sum(n)) %>% pull(n)
stats_errors = filter(numbers, str_detect(reason, 'Statistical'))%>%pull(n) 

# add errors that were never found
load('data/2_pubmed.RData') # from 2_pubmed_concat.R for zeros
zeros = nrow(zeros)
non_zero = select(data, 'error') %>% unique() %>% nrow() # for check

# add errors excluded because they were an acronym or name or in the keywords
# these exclusions are in terms of abstracts, not Wikipedia errors
load('data/3_pubmed.RData') # from 3_expunge_acronyms_names.R, for excluded
excluded = select(excluded, error) %>% unique() 
in_data = select(data, error) %>% unique() 
errors_excluded = setdiff(excluded, in_data) %>% nrow() # knock out those errors that were also in the data
checked = select(data, error) %>% unique() 
checked_n = nrow(checked)
# 
load('data/7_analysis_data.RData') # from 7_make_analysis_ready.R
final_errors = filter(data, case==1) %>% select(error) %>% unique() 
final_errors_n  = nrow(final_errors)
#
setdiff(checked, final_errors)
n_alex = checked_n - final_errors_n # last exclusions

# labels
l1 = paste('Wikipedia list\nof errors (N=', start_numbers, ')', sep='') # 
l2 = paste('Excluded (N=', total_excluded,  
           ')\n- Acronym (N=', filter(numbers, str_detect(reason, 'acronyms$'))%>%pull(n),
           ')\n- Manual checks (N=', filter(numbers, str_detect(reason, 'manual checks$'))%>%pull(n), 
           ')\n- Apostrophe (N=', filter(numbers, str_detect(reason, 'apostrophes$'))%>%pull(n), 
           ')\n- Hyphen (N=', filter(numbers, str_detect(reason, 'hyphens$'))%>%pull(n), 
           ')', sep='') # 
l3 = paste('Additional\nstatistical\nerrors (N=', stats_errors, ')', sep='')
l4 = paste('Excluded as keyword\nor noun (N=', errors_excluded, ')', sep='')
l5 = paste('Final errors (N=', final_errors_n, ')', sep='')
l6 = paste('Error not found\nin PubMed (N=', zeros, ')', sep='')
l7 = paste('Multiple languages or no\nOpen Alex data (N=', n_alex, ')', sep='')
labels = c(l1, l2, l3, l4, l5, l6, l7, '', '', '', '', '', '')
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin, < 1 = wide and short 
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.33	0.92	white	square	0.3	0.16
2	0.77	0.73	white	square	0.55	0.21
3	0.115	0.61	white	square	0.77	0.11
4	0.77	0.36	white	square	0.27	0.19
5	0.33	0.06	white	square	0.27	0.17
6	0.77	0.5	white	square	0.27	0.19
7	0.77	0.2	white	square	0.27	0.19
8	0.33	0.73	transparent	square	0.0	0.0
9	0.23	0.61	transparent	square	0.0	0.0
10	0.33	0.36	transparent	square	0.0	0.0
11	0.33	0.5	transparent	square	0.0	0.0
12	0.33	0.2	transparent	square	0.0	0.0
13	0.33	0.61	transparent	square	0.0	0.0')
# last dots are dummies for arrows
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[8, 1] = "' '"
M[5, 8] = "' '"
M[2, 8] = "' '" # to first excluded box
M[9, 3] = "' '" # adding statistics arrow; need two because arrow gets hidden
M[13, 9] = "' '" # ... second arrow for stats
M[4, 10] = "' '" # last exclusion arrow
M[6, 11] = "' '" # exclusion arrow to zero
M[7, 12] = "' '" # exclusion arrow to no Alex

## make figure 
jpeg('figures/99_consort_flow_errors.jpg', width=5, height=5, units='in', res=500, quality = 100)
par(mai=c(0,0.01,0.01,0.01))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        arr.pos = 0.45,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
dev.off()

# final numbers
4310-250+30-2025-135-14
