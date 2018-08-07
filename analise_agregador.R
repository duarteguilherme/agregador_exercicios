setwd('~/Jota/agregador_exercicios/')


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
         percentage = percentage %>% str_replace(',', '.') %>%  as.numeric
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
  mu[n_days]  ~ normal(0.5, 0.0025);
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.0025);
}
'

model_ex1 <- stan(model_code = agg_model, data = list(n_days = max(polls$t)), chains = 1, iter = 2500)


extract_summary <- function(model, first_day) { 
  print(model)
  # Extract summaries from Stan simulated data
    tibble(
      median = apply(model$mu, 2,median),
      p10 = apply(model$mu, 2,function(x) quantile(x, .1)),
      p90 = apply(model$mu, 2,function(x) quantile(x, .90)),
      p05 = apply(model$mu, 2,function(x) quantile(x, .05)),
      p95 = apply(model$mu, 2,function(x) quantile(x, .95)),
      t = 1:dim(model)[3],
      days = first_day + dim(model)[3]
  )
}

# Checking convergence
traceplot(model_ex1)
model_ex1_data <- rstan::extract(model_ex1)

model_ex1_data %>%
  extract_summary(., start_2round_day) %>%
  ggplot() +
  geom_line(aes(x = days, y = median )) +
  geom_ribbon(aes(x = days, ymin = p05,  ymax = p95))





##################################################################
# E-2
# Running a model with likelihood
##################################################################

create_datalist <- function(df,candidate) {
  # Return data list for using inside Stan
  df <- filter(df, candidate == candidato)
  return(
    list(n_days = max(df$t), 
         y_n = nrow(df),
         y_values = df$percentage,
         y_days = df$t,
         n_interview = df$n_entrevistas,
         inst = df$id_instituto
    )
  )
}





