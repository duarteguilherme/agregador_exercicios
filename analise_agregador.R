setwd('~/Documentos/academic/agregador_exercicios/')


library(rstan)
library(tidyverse)
library(lubridate)

# Actual results for Brazilian elections
resultados <- read.csv('Resultados - Presidente.csv', stringsAsFactors = F)

# Poll results for 2014 - Presidential elections (2º round)
polls <- read.csv('polls_2014_Presidential_2.csv', stringsAsFactors = F)
#polls <- read.csv('PollingData - 2014-T2-BRASIL-BR-Presidente.csv', stringsAsFactors = F, sep=';')
#polls <- select(polls, Data, Instituto, Dilma_PT = Dilma..PT., Aecio_PSDB = Aecio..PSDB.,
#                Br.Nulo.Nenhum, NS.NR, n_entrevistas = Entrevistas  )

#polls <- gather(polls, candidate, percentage, Dilma_PT, Aecio_PSDB, Br.Nulo.Nenhum, NS.NR)
#write.csv(polls, "polls_2014_Presidential_2.csv", row.names=F)



# wrangling data
election_day <- ymd("2014-10-26")
start_2round_day <- ymd("2014-10-5")

polls <- polls %>%
  mutate(election_day = election_day,
         Data = ymd(Data),
         percentage = percentage %>% str_replace(',', '.') %>%  as.numeric %>% `/`(., 100)
         ) %>%
  filter(Data >= start_2round_day) %>%
  mutate(t = as.integer(Data - start_2round_day) + 1,
         id_instituto = Instituto %>% as.factor %>% as.integer)




##################################################################
# E-1
# Running a model without likelihood
##################################################################

agg_model <- '
data {
  int<lower=1> n_days;            // number of days
}
parameters {
  real<lower=0, upper=1> mu[n_days];               // underlying state of vote intention
}
model {
   // state model
  mu[1]  ~ normal(0.5, 0.0025);
  for (i in 2:n_days)
      mu[i] ~ normal(mu[i - 1], 0.0025);
}
'

model_ex1 <- stan(model_code = agg_model, data = list(n_days = max(polls$t)), chains = 1, iter = 2500)


extract_summary <- function(model, first_day) {
  # Extract summaries from Stan simulated data
    tibble(
      median = apply(model$mu, 2,median),
      p10 = apply(model$mu, 2,function(x) quantile(x, .1)),
      p90 = apply(model$mu, 2,function(x) quantile(x, .90)),
      p05 = apply(model$mu, 2,function(x) quantile(x, .05)),
      p95 = apply(model$mu, 2,function(x) quantile(x, .95)),
      t = 1:dim(model$mu)[2],
      days = first_day + 1:dim(model$mu)[2]
  )
}

# Checking convergence
traceplot(model_ex1)
model_ex1_data <- rstan::extract(model_ex1)


# Plotting data
model_ex1_data %>%
  extract_summary(., start_2round_day) %>%
  ggplot() +
  geom_line(aes(x = days, y = median )) +
  geom_ribbon(aes(x = days, ymin = p05,  ymax = p95), alpha = 0.2) +
  theme_bw() + labs(y = "percentage")





##################################################################
# E-2
# Running a model with likelihood
##################################################################

create_datalist <- function(df,candidate_name) {
  # Return data list for using inside Stan
  df <- dplyr::filter(df, candidate == candidate_name)
  return(
    list(n_days = max(df$t),
         y_n = nrow(df),
         y_values = df$percentage,
         y_days = df$t,
         n_sample = df$n_entrevistas,
         id_company = df$id_instituto
    )
  )
}


agg_model2 <- '
data {
  int<lower=1> n_days;            // number of days
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real n_sample[y_n];             // sample size for each poll
//  int<lower=0> id_company;        // id for research companies
}
parameters {
  real<lower=0, upper=1> mu[n_days];               // underlying state of vote intention
}
model {
  mu[1] ~ uniform(0, 1);
  for (i in 2:n_days)
      mu[i] ~ normal(mu[i - 1], 0.0025);

  for(x in 1:y_n) // likelihood
//      y_values[x] ~ normal(mu[y_days[x]], 0.01 );
      y_values[x] ~ normal(mu[y_days[x]], sqrt(y_values[x]*(1-y_values[x])/n_sample[x]) );
}
'

ex2_input_data_dilma <- create_datalist(polls, "Dilma_PT")
model_ex2_dilma <- stan(model_code = agg_model2, data = ex2_input_data_dilma, chains = 1, iter = 2500)
ex2_input_data_aecio <- create_datalist(polls, "Aecio_PSDB")
model_ex2_aecio <- stan(model_code = agg_model2, data = ex2_input_data_aecio, chains = 1, iter = 2500)




# Checking convergence
traceplot(model_ex2_dilma)
traceplot(model_ex2_aecio)


model_ex2_data_dilma <- rstan::extract(model_ex2_dilma)
model_ex2_data_aecio <- rstan::extract(model_ex2_aecio)

# Merging data
model_ex2_data_dilma <- model_ex2_data_dilma %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Dilma_PT"))

model_ex2_data_aecio <- model_ex2_data_aecio %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Aecio_PSDB"))


bind_rows(model_ex2_data_dilma, model_ex2_data_aecio) %>%
  ggplot() +
  geom_line(aes(x = days, y = median, colour = candidate )) +
  geom_ribbon(aes(x = days, ymin = p05,  ymax = p95, fill = candidate), alpha = 0.2) +
  geom_point(aes(x = days, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") +
  scale_fill_manual(values=c("#0000ff", "#ff0000")) +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))






##################################################################
# E-3
# Giving weight to each company
##################################################################


polls %>%
  filter(Instituto == "Ibope") %>%
  select(id_instituto) %>%
  slice(1)

polls %>%
  filter(Instituto == "Datafolha") %>%
  select(id_instituto) %>%
  slice(1)

# Datafolha -> id = 1
# Ibope -> id = 2

agg_model3 <- '
data {
  int<lower=1> n_days;            // number of days
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real n_sample[y_n];             // sample size for each poll
  int<lower=0> id_company[y_n];        // id for research companies
}
parameters {
  real<lower=0, upper=1> mu[n_days];               // underlying state of vote intention
}
model {
mu[1] ~ uniform(0, 1);
for (i in 2:n_days)
mu[i] ~ normal(mu[i - 1], 0.0025);

  for(x in 1:y_n)  {
    if (id_company[x] < 3) {
      y_values[x] ~ normal(mu[y_days[x]], sqrt(y_values[x]*(1-y_values[x])/n_sample[x]) );
    } else {
      y_values[x] ~ normal(mu[y_days[x]], 2*sqrt(y_values[x]*(1-y_values[x])/n_sample[x]) );
    }
  }
}'

ex3_input_data_dilma <- create_datalist(polls, "Dilma_PT")
model_ex3_dilma <- stan(model_code = agg_model3, data = ex3_input_data_dilma, chains = 1, iter = 2500)
ex3_input_data_aecio <- create_datalist(polls, "Aecio_PSDB")
model_ex3_aecio <- stan(model_code = agg_model3, data = ex3_input_data_aecio, chains = 1, iter = 2500)




# Checking convergence
traceplot(model_ex3_dilma)
traceplot(model_ex3_aecio)


model_ex3_data_dilma <- rstan::extract(model_ex3_dilma)
model_ex3_data_aecio <- rstan::extract(model_ex3_aecio)

# Merging data
model_ex3_data_dilma <- model_ex3_data_dilma %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Dilma_PT"))

model_ex3_data_aecio <- model_ex3_data_aecio %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Aecio_PSDB"))


bind_rows(model_ex3_data_dilma, model_ex3_data_aecio) %>%
  ggplot() +
  geom_line(aes(x = Data, y = median, colour = candidate )) +
  geom_ribbon(aes(x = Data, ymin = p05,  ymax = p95, fill = candidate), alpha = 0.2) +
  geom_point(aes(x = Data, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") +
  scale_fill_manual(values=c("#0000ff", "#ff0000")) +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))







##################################################################
# E-4
# Calculating house effects
##################################################################

create_datalist4 <- function(df,candidate_name,actual_result) {
  # Return data list for using inside Stan
  df <- dplyr::filter(df, candidate == candidate_name)
  return(
    list(n_days = max(df$t),
         y_n = nrow(df),
         y_values = df$percentage,
         y_days = df$t,
         n_sample = df$n_entrevistas,
         id_company = df$id_instituto,
         n_companies = length(unique(df$id_instituto)),
         actual_result = actual_result
    )
  )
}


agg_model4 <- '
data {
  int<lower=1> n_days;            // number of days
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real n_sample[y_n];             // sample size for each poll
  int<lower=0> id_company[y_n];        // id for research companies
  int<lower=1> n_companies; // number of companies 
  real actual_result;
}
parameters {
  real<lower=0, upper=1> mu[n_days];               // underlying state of vote intention
  real gamma[n_companies];
}
model {
  mu[1] ~ uniform(0, 1);
  for (i in 2:(n_days-1))
    mu[i] ~ normal(mu[i - 1], 0.0025);
  
  mu[n_days] ~ normal(actual_result, 0.0025 );

  gamma ~ normal(0, 0.02);


  for(x in 1:y_n)  {
    y_values[x] ~ normal(mu[y_days[x]]  + gamma[id_company[x]], 
        sqrt(y_values[x]*(1-y_values[x])/n_sample[x]) );
  }
  
}'

ex4_input_data_dilma <- create_datalist4(polls, "Dilma_PT", 0.48366024)
model_ex4_dilma <- stan(model_code = agg_model4, data = ex4_input_data_dilma, chains = 1, iter = 2500)
ex4_input_data_aecio <- create_datalist4(polls, "Aecio_PSDB", 0.45293976)
model_ex4_aecio <- stan(model_code = agg_model4, data = ex4_input_data_aecio, chains = 1, iter = 2500)




# Checking convergence
traceplot(model_ex4_dilma)
traceplot(model_ex4_aecio)


model_ex4_data_dilma <- rstan::extract(model_ex4_dilma)
model_ex4_data_aecio <- rstan::extract(model_ex4_aecio)

# Merging data
model_ex4_data_dilma <- model_ex4_data_dilma %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Dilma_PT"))

model_ex4_data_aecio <- model_ex4_data_aecio %>%
  extract_summary(., start_2round_day) %>%
  inner_join(filter(polls, candidate == "Aecio_PSDB"))


bind_rows(model_ex4_data_dilma, model_ex4_data_aecio) %>%
  ggplot() +
  geom_line(aes(x = Data, y = median, colour = candidate )) +
  geom_ribbon(aes(x = Data, ymin = p05,  ymax = p95, fill = candidate), alpha = 0.2) +
  geom_point(aes(x = Data, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") +
  scale_fill_manual(values=c("#0000ff", "#ff0000")) +
  scale_colour_manual(values=c("#0000ff", "#ff0000"))


# Analyzing house effects

extract_house_effects <- function(model) {
  tibble(
    median = apply(model$gamma, 2,median),
    p10 = apply(model$gamma, 2,function(x) quantile(x, .1)),
    p90 = apply(model$gamma, 2,function(x) quantile(x, .90)),
    p05 = apply(model$gamma, 2,function(x) quantile(x, .05)),
    p95 = apply(model$gamma, 2,function(x) quantile(x, .95)),
    id_instituto = 1:dim(model$gamma)[2]
  )
  
}

gamma_dilma <- rstan::extract(model_ex4_dilma) %>% 
  extract_house_effects %>%
  inner_join( polls %>%
                distinct(id_instituto, Instituto)
    ) %>%
  mutate(candidate = "Dilma_PT") 


# Convergence for gamma
traceplot(model_ex4_dilma, "gamma")


# House effects (Companies)
gamma_dilma %>%
  ggplot(aes(x = Instituto, y = median)) +
  geom_pointrange(aes(ymin = p05, ymax = p95)) +
  theme_bw() +
  labs(y = "House Effect") +
  coord_flip()
  
  










