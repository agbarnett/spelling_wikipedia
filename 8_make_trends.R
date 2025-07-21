# 8_make_trends.R
# calculate the trends with a denominator of abstract numbers and word count
# July 2025
library(dplyr)
library(binom) # for confidence intervals

# get the data from 7_make_analysis_ready.R
load('data/7_analysis_data.RData')
indata = data # simple rename
# get the denominators, from 1_pubmed_denominator.R
load('data/1_denominator.RData')

## part 0: get average word counts for cases and controls (used later)
word_counts = group_by(indata, year) %>%
  summarise(mean = mean(word_count),
            sd = sd(word_count),
            n = n(),
            sem = sd/sqrt(n),
            z = qnorm(0.975),
            lower = mean - (z*sem),
            upper = mean + (z*sem),
            lower = ifelse(lower<0, 0, lower))
# now word counts by type; types need a minimum number to plot to reduce clutter
numbers = group_by(indata, typeo) %>%
  tally() %>%
  filter(n > 200) %>%
  pull(typeo) #%>% filter(year <= 2023)
#
word_counts_type = filter(indata, typeo %in% numbers) %>%
  group_by(year, typeo) %>%
  summarise(mean = mean(word_count),
            sd = sd(word_count),
            n = n(),
            sem = sd/sqrt(n),
            z = qnorm(0.975),
            lower = mean - (z*sem),
            upper = mean + (z*sem),
            lower = ifelse(lower<0, 0, lower)) # fix to lower CI

## part 1) get error counts per abstract ##

# remove controls (only trends in cases)
indata = filter(indata, case==1) 

#
per_abstract = group_by(indata, pmid) %>% 
  tally() %>%
  ungroup()

# part 2) get error frequencies by year and error  
freqs = group_by(indata, year, error) %>% 
  tally() %>%
  ungroup()
# add zero years/errors
all_errors = unique(indata$error) # errors that were in the analysis data
full = expand.grid(year = years, error = all_errors)
freqs = full_join(freqs, full, by=c('year','error')) %>%
  mutate(n = ifelse(is.na(n), 0, n)) 

# add denominator and calculate binomial confidence interval
freqs = left_join(freqs, denom, by='year') %>%
  mutate(p = 10000*n / denom, # per 10,000 abstracts
         lower = binom.exact(n = denom, x = n)$lower,
         upper = binom.exact(n = denom, x = n)$upper,
         lower = lower*10000,
         upper = upper*10000)
#
freqs = arrange(freqs, error, year)

# add overall total of all errors
total = group_by(freqs, year, denom) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(
    error = 'TOTAL',
    p = 10000*n / denom, # per 10,000 abstracts
    lower = binom.exact(n = denom, x = n)$lower,
    upper = binom.exact(n = denom, x = n)$upper,
    lower = lower*10000,
    upper = upper*10000)
# add total to results
freqs = bind_rows(freqs, total)%>% 
  ungroup()

## part 3) trends per abstract per word count (per million words)
word_count_mean = select(word_counts, 'year','mean') %>% # smaller data for merging
  rename('wmean' = 'mean')
freqs_word = select(total, year, n, denom) %>% 
  left_join(word_count_mean, by='year') %>%
  mutate(total_words = denom*round(wmean), # total words per year (extrapolated from average)
         p = 1000000 * n / total_words, # per 1 million words
         lower = binom.exact(n = total_words, x = n)$lower,
         upper = binom.exact(n = total_words, x = n)$upper,
         lower = lower*1000000,
         upper = upper*1000000)


#### part 4) trends by article type
selected_type = c('Article','Editorial','Letter','Review') # just these predominant types as others are too small
freqs_type = filter(indata, typeo %in% selected_type) %>%
  group_by(year, typeo) %>% 
  tally() %>%
  ungroup()
# add zero years/errors
full = expand.grid(year = years, typeo = selected_type)
freqs_type = full_join(freqs_type, full, by=c('year','typeo')) %>%
  mutate(n = ifelse(is.na(n), 0, n)) 
freqs_type = left_join(freqs_type, denom_type, by=c('year','typeo')) %>%
  mutate(p = 10000*n / denom, # per 10,000 abstracts
         lower = binom.exact(n = denom, x = n)$lower,
         upper = binom.exact(n = denom, x = n)$upper,
         lower = lower*10000,
         upper = upper*10000)
#
freqs_type = arrange(freqs_type, typeo, year)


## save ##
trends = freqs
trends_type = freqs_type
trends_words = freqs_word
save(per_abstract, trends, trends_type, trends_words, word_counts, word_counts_type, file='data/8_trends.RData')
