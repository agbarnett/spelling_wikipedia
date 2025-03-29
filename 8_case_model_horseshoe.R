# 8_case_model_horseshoe.R
# Bayesian model of case/control for spelling errors using horseshoe priors
# modelling probability on absolute scale
# see https://umr-astre.pages.mia.inra.fr/training/nimble-Weibull-Shrinkage/#approach-2-horseshoe-prior
# Feb 2025

# common code to read in data and prepare it for model:
source('8_case_model_common.R')

# bugs code for normal regression using probability as an outcome with horseshoe prior for sparse variables
hsCode <- nimbleCode ({
  beta0 ~ dnorm(0, sd=1E6)
  gamma1  ~ dnorm(0, sd=1E6)
  gamma2  ~ dnorm(0, sd=1E6)
  tau ~ dgamma(0.1, 0.1)
  sigma <- sqrt(1/tau)
  # Horseshoe priors, Truncated Cauchy(0,1)
  beta1[1] <- 0 # corner-point reference category to help with mixing
  for (k in 2:n_country){
    beta1[k]   ~ dnorm(mean=0, sd = tau1*lambda1) # country
  }
  beta2[1] <- 0 # corner-point reference category to help with mixing
  for (k in 2:n_publisher){
    beta2[k]   ~ dnorm(mean=0, sd = tau2*lambda2) # publisher
  }
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
    error[i] ~ dnorm(mu[i], sd = sigma)
    mu[i] <- beta0 + beta1[country[i]] + beta2[publisher[i]] + beta3[type[i]] + gamma1*word_count[i] + gamma2*authors[i]
  }
  # predictions for word count
  for (j in 1:n_pred) {
    pred[j] <- gamma1*word_count_pred[j]
  }
})

# for word count predictions
wc = c(0,10,20,30,40,seq(50,500,50)) # range to predict, more for smaller numbers
n_pred = length(wc)
word_count_pred = (sqrt(wc) - 15)/4 # same transformation as in model

## TO HERE< need to model 1 error per abstract for those with multiple errors

# set up data
constants = list(n_obs = nrow(for_model), 
                 word_count = (sqrt(for_model$word_count) - 15)/4, # square root transformation, then scaled
                 authors = (sqrt(for_model$n_authors)-2.2)/1, # transformed and scaled 
                 country = for_model$country_num,
                 publisher = for_model$publisher_num,
                 type = as.numeric(as.factor(for_model$typeo)),
                 word_count_pred = word_count_pred,
                 n_pred = n_pred,
                 n_publisher = n_publisher,
                 n_country = n_country,
                 n_type = n_type)
data = list(error = for_model$case) # spelling error, 1 = yes

# initial values
inits = list(beta0 = 1/3, gamma1 = 0, gamma2 = 0, 
             beta1 = rep(0, n_country), beta2 = rep(0, n_publisher), beta3 = rep(0, n_type),
             tau =1, tau1 = 1, lambda1 = 1, tau2 = 1, lambda2 = 1, tau3 = 1, lambda3 = 1)
parms = c('beta0','sigma','gamma1','gamma2','beta1.c','beta2.c','beta3','pred')

# Build NIMBLE model (takes a while for bigger models)
hsR  <- nimbleModel(
  hsCode,
  const = constants,
  inits = inits,
  data = data) 

# run model
MCMC = 10000
thin = 3
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
    var == 'beta' ~ "Trend",
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
save(thin, MCMC, wc, df, mcmc, 
     publisher_labels, country_labels,
     file = 'data/8_results_errors_horseshoe.RData')

# checking autocorrelation in chains
samples = mcmc$samples$chain1
lag1 = NULL
for (k in 1:ncol(samples)){
  name = colnames(samples)[k]
  if(str_detect(name, pattern='^lambda|^tau')){next} # remove lambda and tau as correlation here does not matter
  res = acf(samples[,k],1,plot=FALSE)
  frame = data.frame(var = colnames(samples)[k], k=k, lag1 = res$acf[,,1][2])
  lag1 = bind_rows(lag1, frame)
}
head(arrange(lag1, desc(lag1)))
