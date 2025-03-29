# 8_citation_model_horseshoe.R
# Bayesian model of citations dependent on spelling errors using horseshoe priors
# see https://umr-astre.pages.mia.inra.fr/training/nimble-Weibull-Shrinkage/#approach-2-horseshoe-prior
# Feb 2025
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
# make time since published (years)
data = mutate(data,
       time = as.numeric(date_extracted - pubmed_date)/365.25)

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
country_labels = data.frame(country = unique(for_model$country)) %>%
  mutate(country_num = as.numeric(as.factor(country)))

# publishers must have a minimum sample size 
publisher_freq = group_by(for_model, publisher) %>%
  tally() %>%
  rename('publisher_n' = 'n')
for_model = left_join(for_model, publisher_freq, by='publisher') %>%
  mutate(publisher = ifelse(publisher_n < 200, 'Other', publisher))
# make labels for plots
publisher_labels = data.frame(publisher = unique(for_model$publisher)) %>%
  mutate(publisher_num = as.numeric(as.factor(publisher)))

# what's the best transformation for author numbers and word counts
powers = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3) # fractional polynomials
fit = NULL
for (p in powers){
  data_p = mutate(for_model,
                  n_authors = n_authors +1 ,
                  author_power = case_when(
                    p == 0 ~ log2(n_authors),
                    p != 0 ~ n_authors ^ p),
                  time = time +1 ,
                  time_power = case_when(
                    p == 0 ~ log2(time),
                    p != 0 ~ time ^ p)
  )
  model1 = glm(citations ~ author_power, family=poisson(), data = data_p)
  frame1 = data.frame(model = 'authors', power = p, AIC=AIC(model1))
  model2 = glm(citations ~ time_power, family=poisson(), data = data_p)
  frame2 = data.frame(model = 'time', power = p, AIC=AIC(model2))
  fit = bind_rows(fit, frame1, frame2)
}
gplot = ggplot(data = fit, aes(x = power, y=AIC))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~model, scale='free_y')
gplot
# linear is best for author numbers; inverse for time

# bugs code for Poisson regression with horseshoe prior for sparse variables
hsCode <- nimbleCode ({
  beta0 ~ dnorm(0, sd=1E6)
  gamma1  ~ dnorm(0, sd=1E6)
  gamma2  ~ dnorm(0, sd=1E6)
  gamma3  ~ dnorm(0, sd=1E6)
  # Horseshoe priors, Truncated Cauchy(0,1), i.e. the Gelman 2006 approach
  for (k in 1:n_country){
    beta1[k]   ~ dnorm(mean=0, sd = tau1*lambda1) # country
    beta1.c[k] <- beta1[k] - mu.beta1 # centre for mixing
  }
  mu.beta1 <- mean(beta1[1:n_country]) 
  for (k in 1:n_publisher){
    beta2[k]   ~ dnorm(mean=0, sd = tau2*lambda2) # publisher
    beta2.c[k] <- beta2[k] - mu.beta2 # centre for mixing (takes additional time)
  }
  mu.beta2 <- mean(beta2[1:n_publisher]) 
  beta3[1] <- 0 # corner-point reference category to help with mixing
  for (k in 2:n_type){
    beta3[k]   ~ dnorm(mean=0, sd = tau3*lambda3) # type
  }
  # hyper-priors
  lambda1 ~ T(dt(mu=0, sigma=1, df=1), 0, Inf) # Truncated Cauchy(0,1)
  tau1     ~ T(dt(mu=0, sigma=1, df=1), 0, Inf)
  lambda2 ~ T(dt(mu=0, sigma=1, df=1), 0, Inf) # Truncated Cauchy(0,1)
  tau2     ~ T(dt(mu=0, sigma=1, df=1), 0, Inf)
  lambda3 ~ T(dt(mu=0, sigma=1, df=1), 0, Inf) # Truncated Cauchy(0,1)
  tau3     ~ T(dt(mu=0, sigma=1, df=1), 0, Inf)
  # likelihood
  for (i in 1:n_obs) {
    citations[i] ~ dpois(mu[i])
    log(mu[i]) <- beta0 + beta1.c[country[i]] + beta2.c[publisher[i]] + beta3[type[i]] + gamma1*authors[i] + gamma2*time[i] + gamma3*spelling_error[i]
  }
})

# set up data
n_publisher = length(unique(for_model$publisher)) # number of countries
n_country = length(unique(for_model$country)) # number of countries
n_type = length(unique(for_model$typeo)) # number of articles (with small categories combined)
constants = list(n_obs = nrow(for_model), 
                 time = ((for_model$time^(-1)) - 0.14)/1, # inverse and scaled
                 authors = (for_model$n_authors-5)/5, # scaled 
                 country = as.numeric(as.factor(for_model$country)),
                 publisher = as.numeric(as.factor(for_model$publisher)),
                 type = as.numeric(as.factor(for_model$typeo)),
                 spelling_error = for_model$case,
                 n_publisher = n_publisher,
                 n_country = n_country,
                 n_type = n_type)
data = list(citations = for_model$citations) # spelling error, 1 = yes

# get reasonably starting values
glm_start = glm(for_model$citations ~ constants$authors + constants$time, family=poisson())

# initial values
inits = list(beta0 = as.numeric(glm_start$coefficients[1]), # use starting values from GLM above
             gamma1 = as.numeric(glm_start$coefficients[2]), 
             gamma2 = as.numeric(glm_start$coefficients[3]), 
             gamma3 = 0,
             beta1 = rep(0, n_country), beta2 = rep(0, n_publisher), beta3 = rep(0, n_type),
             tau1 = 1, lambda1 = 1, tau2 = 1, lambda2 = 1, tau3 = 1, lambda3 = 1)
parms = c('beta0','gamma1','gamma2','gamma3','beta1.c','beta2.c','beta3','tau1','tau2','tau3','lambda1','lambda2','lambda3')

# Build NIMBLE model (takes a while for bigger models)
hsR  <- nimbleModel(
  hsCode,
  const = constants,
  inits = inits,
  data = data) 

# run model
MCMC = 10000
thin = 3
MCMC = 800
thin = 1
n.chains = 2
mcmc =  nimbleMCMC(model = hsR,
                   inits = inits,
                   monitors = parms,
                   niter = MCMC*2*thin, # times 2 for burn-in 
                   thin = thin,
                   nchains = n.chains, 
                   nburnin = MCMC,
                   summary = TRUE, 
                   setSeed = seeds,
                   WAIC = FALSE) # 

# formatting data frame of stats
for_stats = mcmc$summary$all.chains
df = data.frame(for_stats) %>%
  tibble::rownames_to_column(var = "var") %>%
  clean_names() %>%
  select(-median, -st_dev) %>%
  rename('lower' = 'x95_ci_low',
         'upper' = 'x95_ci_upp'
  ) %>%
  mutate(var = case_when(
    var == 'alpha' ~ "Intercept",
    TRUE ~ as.character(var)
  ))

# add posterior p-values
pos = rbind(mcmc$samples$chain1, mcmc$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(n.chains*MCMC)) # two-sided; 2 chains
df = bind_cols(df, pval)
names(df)[5] = 'pval'

# save results, plot in errors_horseshoe.Rmd
save(thin, MCMC, df, mcmc, publisher_labels, country_labels, file = 'data/8_results_citations_horseshoe.RData')
