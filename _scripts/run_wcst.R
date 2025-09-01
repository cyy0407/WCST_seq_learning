library(rstan)
library(ggplot2)
library(data.table)
library(dplyr)
library(R.matlab)
library(loo)

task_name       = "wcs"
model_name      = "sql"
model           = "wcs_sql" 
data_columns    = c("subjID", "choice", "outcome")
parameters      = list(
  "r" = c(0, 0.1, 1),
  "p" = c(0, 0.1, 1),
  "d" = c(0, 1, 5)
)

niter          = 2000
nwarmup        = 1000
nchain         = 4
ncore          = 4
nthin          = 1
inits          = "random" 
indPars        = "mean"
modelRegressor = FALSE
vb             = FALSE 
inc_postpred   = FALSE
adapt_delta    = 0.95
stepsize       = 1
max_treedepth  = 10

regressors = NULL
postpreds = "y_pred"

raw_data <- CTdata 

.N <- NULL
subjid <- NULL

DT_trials <- raw_data %>% slice_max(trial, n = 1, by = c(subjID))
subjs     <- DT_trials$subjID
n_subj    <- length(subjs)
t_subjs   <- DT_trials$trial
t_max     <- max(t_subjs)

general_info <- list(
  subjs   = subjs,
  n_subj  = n_subj,
  b_subjs = NULL, 
  b_max   = NULL, 
  t_subjs = t_subjs,
  t_max   = t_max
)

wcs_preprocess_func <- function(raw_data, general_info, answersheet_path) {
  subjs   <- general_info$subjs
  n_subj  <- general_info$n_subj
  t_subjs <- general_info$t_subjs
  t_max   <- general_info$t_max 

  answer <- read.table(answersheet_path, header = TRUE)

  choice           <- array( 0, c(n_subj, 4, t_max))
  outcome          <- array(-1, c(n_subj, t_max))
  choice_match_att <- array( 0, c(n_subj, t_max, 1, 3)) 
  deck_match_rule  <- array( 0, c(t_max, 3, 4))         

  for (i in 1:n_subj) {
    subj <- subjs[i]
    t_current <- t_subjs[i]
    DT_subj <- raw_data[raw_data$subjID == subj, ] 
    DT_subj_choice  <- DT_subj$choice
    DT_subj_outcome <- DT_subj$outcome

    for (tr in 1:t_current) { 
      ch <- DT_subj_choice[tr]
      ou <- DT_subj_outcome[tr]
      choice[i, ch, tr]            <- 1
      outcome[i, tr]               <- ou
      choice_match_att[i, tr, 1, ] <- answer[, tr] == ch 
    }
  }

  for (tr in 1:t_max) {
    for (ru in 1:3) {
      deck_match_rule[tr, ru, answer[ru, tr]] <- 1
    }
  }

  data_list <- list(
    N                = n_subj,
    T                = t_max,
    Tsubj            = t_subjs,
    choice           = choice,
    outcome          = outcome,
    choice_match_att = choice_match_att,
    deck_match_rule  = deck_match_rule
  )

  return(data_list)
}

answersheet_path <- "wcs_answersheet.txt" 
data_list <- wcs_preprocess_func(raw_data, general_info, answersheet_path)

pars <- character()
if (model_type != "single") { 
  pars <- c(pars, paste0("mu_", names(parameters)), "sigma")
}
pars <- c(pars, names(parameters)) 
if ((task_name == "dd") && (model_type == "single")) {
  log_parameter1 <- paste0("log", toupper(names(parameters)[1]))
  pars <- c(pars, log_parameter1)
}
pars <- c(pars, "log_lik") 
if (modelRegressor) {
  pars <- c(pars, names(regressors))
}
if (inc_postpred) {
  pars <- c(pars, postpreds)
}

rstan_options(auto_write = TRUE)
options(mc.cores = ncore) 

model_path <- "wcs_sql.stan" 
stanmodel_arg <- rstan::stan_model(model_path)

gen_init <- function() {
  primes <- numeric(length(parameters))
  for (i in 1:length(parameters)) {
    p_name <- names(parameters)[i]
    lb <- parameters[[i]][1]   
    ub <- parameters[[i]][3]   
    init_val <- parameters[[i]][2] 
    
    if (is.infinite(lb) && is.infinite(ub)) {
      primes[i] <- init_val                             
    } else if (is.infinite(ub)) {
      primes[i] <- log(init_val - lb)                   
    } else {
      primes[i] <- qnorm((init_val - lb) / (ub - lb))   
    }
  }
  
  group_level             <- list(mu_pr = primes,
                                  sigma = rep(1.0, length(primes)))
  
  individual_level        <- lapply(primes, function(x) rep(x, n_subj))
  names(individual_level) <- paste0(names(parameters), "_pr")
  
  init_list <- c(group_level, individual_level)
  return(init_list)
}

fit <- rstan::sampling(object  = stanmodel_arg,
                       data    = data_list,
                       pars    = pars,
                       init    = gen_init, 
                       chains  = nchain,
                       iter    = niter,
                       warmup  = nwarmup,
                       thin    = nthin,
                       control = list(adapt_delta   = adapt_delta,
                                      stepsize      = stepsize,
                                      max_treedepth = max_treedepth))

parVals <- rstan::extract(fit, permuted = TRUE)

estimate_mode <- function(x) {
  d = density(x)
  Estimate = d$x[which.max(d$y)]
  return(Estimate)
}
measure_indPars <- switch(indPars, mean = mean, median = median, mode = estimate_mode)

which_indPars <- names(parameters)

allIndPars <- as.data.frame(array(NA, c(n_subj, length(which_indPars))))
if (model_type == "single") {
  allIndPars[1, ] <- mapply(function(x) measure_indPars(parVals[[x]]), which_indPars)
} else {
  for (i in 1:n_subj) {
    allIndPars[i, ] <- mapply(function(x) measure_indPars(parVals[[x]][, i]), which_indPars)
  }
}
allIndPars <- cbind(subjID = subjs, allIndPars) 
colnames(allIndPars) <- c("subjID", which_indPars)

modelData                   <- list()
modelData$model             <- model
modelData$allIndPars        <- allIndPars
modelData$parVals           <- parVals
modelData$fit               <- fit

save(modelData, file = "modelData_wcs_sql.RData")