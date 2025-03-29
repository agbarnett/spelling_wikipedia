# 8_case_model_common.R
# starting code used by both 8_case_model_horseshoe.R and 8_case_poisson_model_horseshoe.R
library(nimble)
library(coda)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
# seeds for MCMC:
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('bristol')
seeds[2] = TeachingDemos::char2seed('rovers')

# from 7_make_analysis_ready.R
load('data/7_analysis_data.RData')

# smaller model for starters
#for_model = sample_n(data, size = 5000, replace = FALSE) # useful for getting model started
for_model = data

# countries must have a minimum sample size
country_freq = group_by(for_model, country) %>%
  tally() %>%
  rename('country_n' = 'n')
for_model = left_join(for_model, country_freq, by='country') %>%
  mutate(country = ifelse(country_n < 200, 'Other', country))
# make labels for plots
country_labels = select(for_model, country, country_n) %>%
  mutate(country_n = ifelse(country == 'Other', -99, country_n)) %>% # needed because Other countries do not have unique n
  unique() %>%
  arrange(desc(country_n)) %>%
  mutate(country_num = 1:n()) %>% # US as number 1, so reference category
  select(country, country_num)

# publishers must have a minimum sample size 
publisher_freq = group_by(for_model, publisher) %>%
  tally() %>%
  rename('publisher_n' = 'n')
for_model = left_join(for_model, publisher_freq, by='publisher') %>%
  mutate(publisher = ifelse(publisher_n < 200, 'Other', publisher))
# make labels for plots
publisher_labels = select(for_model, publisher, publisher_n) %>%
  mutate(publisher_n = ifelse(publisher == 'Other', -99, publisher_n)) %>% # needed because Other countries do not have unique n
  unique() %>%
  arrange(desc(publisher_n)) %>%
  mutate(publisher_num = 1:n()) %>% # Elsevier as number 1, so reference category
  select(publisher, publisher_num)
# need to add back labels to data
for_model = left_join(for_model, country_labels, by='country')
for_model = left_join(for_model, publisher_labels, by='publisher')

# what's the best transformation for author numbers and word counts
powers = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3) # fractional polynomials
fit = NULL
for (p in powers){
  data_p = mutate(for_model,
                  n_authors = n_authors +1 ,
                  word_count = word_count +1 ,
                  author_power = case_when(
                    p == 0 ~ log2(n_authors),
                    p != 0 ~ n_authors ^ p),
                  word_count_power = case_when(
                    p == 0 ~ log2(word_count),
                    p != 0 ~ word_count ^ p)
  )
  model1 = glm(case ~ author_power + sqrt(word_count), data = data_p)
  model2 = glm(case ~ word_count_power, data = data_p)
  frame1 = data.frame(model = 'authors', power = p, AIC=AIC(model1))
  frame2 = data.frame(model = 'word count', power = p, AIC=AIC(model2))
  fit = bind_rows(fit, frame1, frame2)
}
gplot = ggplot(data = fit, aes(x = power, y=AIC, color=factor(model)))+
  geom_point()+
  geom_line()+
  facet_wrap(~model, scales='free_y')+
  theme_bw()
gplot
# sqrt best for authors; sqrt for word count

# numbers needed for data set up
n_publisher = length(unique(for_model$publisher)) # number of publishers
n_country = length(unique(for_model$country)) # number of countries
n_type = length(unique(for_model$typeo)) # number of articles (with small categories combined)
