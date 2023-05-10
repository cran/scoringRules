## ----preliminaries, echo=FALSE, results='hide'----------------------
# Using knitr for manuscript
library(knitr)
render_sweave()
opts_chunk$set(engine='R', tidy=FALSE)

# JSS code formatting
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

# Formatting
options(scipen = 1, digits = 3)
Sys.setenv(LANG = 'en')

# RNG initialization
set.seed(42)

# Required packages
library(scoringRules)
if (requireNamespace("crch", quietly = TRUE)) {
  use_crch <- TRUE
  library(crch)
  } else {
  use_crch <- FALSE
}
if (requireNamespace("ggplot2", quietly = TRUE)) {
  use_ggplot <- TRUE
  library(ggplot2)
  } else {
  use_ggplot <- FALSE
}

## ----Chaining-func-example, echo=FALSE, dev='pdf', fig.width=2.5, fig.height=2.5, fig.align="center"----
if (use_ggplot) {
  a1 <- a2 <- 2.5
  b1 <- b2 <- 7.5
  
  x <- pmin(pmax(rnorm(10, 5, 1.5), 0.5), 9.5)
  y <- pmin(pmax(rnorm(10, 5, 1.5), 0.5), 9.5)
  w_ind <- (x < a1) | (x > b1) | (y < a2) | (y > b2)
  v_x <- pmin(pmax(x, a1), b1)
  v_y <- pmin(pmax(y, a2), b2)
  
  ggplot() + geom_rect(aes(xmin = a1, xmax = b1, ymin = a2, ymax = b2), fill = "lightblue") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(a1, b1), 
                       labels = c(expression(a[1]), expression(b[1])), name = expression(z[1])) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(a2, b2), 
                       labels = c(expression(a[2]), expression(b[2])), name = expression(z[2])) +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_segment(aes(x = x[w_ind], y = y[w_ind], xend = v_x[w_ind], yend = v_y[w_ind]),
                 lineend = "round", linejoin = "round", linewidth = 1,
                 arrow = arrow(length = unit(0.1, "cm")), col = "red") +
    geom_point(aes(x = x, y = y), size = 2) +
    geom_point(aes(x = v_x, y = v_y), size = 2, shape = 4)
}

## ----Univ-weighted-score-example-1----------------------------------
obs <- rnorm(5)
sample_m <- matrix(rnorm(5e4), nrow = 5)
score_df <- data.frame(crps = crps_sample(obs, sample_m),
                       owcrps = owcrps_sample(obs, sample_m),
                       twcrps = twcrps_sample(obs, sample_m))
print(score_df)

## ----Univ-weighted-score-example-2----------------------------------
t <- 0
score_df <- data.frame(crps = crps_sample(obs, sample_m),
                       owcrps = owcrps_sample(obs, sample_m, a = t),
                       twcrps = twcrps_sample(obs, sample_m, a = t))
print(score_df)

## ----Weighted-score-illustration, echo=FALSE, dev='pdf', fig.width=10.4, fig.height=4.1, fig.align="center", out.width = "\\linewidth"----

y <- rnorm(1000)
dat_norm <- matrix(rnorm(1000*100), nrow = 1000)
dat_logis <- matrix(rlogis(1000*100), nrow = 1000)
x <- seq(-3, 3, 0.1)
ow_scores_norm <- sapply(x, function(t) mean(owcrps_sample(y, dat_norm, a = t)))
tw_scores_norm <- sapply(x, function(t) mean(twcrps_sample(y, dat_norm, a = t)))
ow_scores_logis <- sapply(x, function(t) mean(owcrps_sample(y, dat_logis, a = t)))
tw_scores_logis <- sapply(x, function(t) mean(twcrps_sample(y, dat_logis, a = t)))

# Initialize plot
par(mai = c(0.9, 3.5, 0.3, 3.4), cex = 1.1)
plot(x, tw_scores_norm, type = "l", col = "purple", lwd = 2,
     xlim = c(-3, 3), ylim = c(0, 1),
     xlab = "Threshold a", ylab = "Score value",
     xaxt = "n", yaxt = "n")
lines(x, ow_scores_norm, col = "darkorange", lwd = 2)
lines(x, tw_scores_logis, col = "purple", lty = "dashed", lwd = 2)
lines(x, ow_scores_logis, col = "darkorange", lty = "dashed", lwd = 2)
axis(1, at = c(-3, 0, 3))
axis(2, at = c(0, 0.5, 1))

# Add legend
legend("top", bty = "n", legend = c("twCRPS", "owCRPS", "Normal", "Logistic"), 
  col = c("purple", "darkorange", "grey", "grey"),
  lty = c(1, 1, 1, 2), lwd = c(2, 2), ncol = 2)



## ----Multiv-weighted-score-example-1--------------------------------
d <- length(obs)
twes_sample(obs, sample_m, a = t)
twes_sample(obs, sample_m, a = rep(t, d))

## ----Custom-weight-function-example-1-------------------------------
mu <- 0; sigma <- 1
weight_func <- function(x) pnorm(x, mean = mu, sd = sigma)
owcrps_sample(obs, sample_m, weight_func = weight_func)

## ----Custom-weight-function-example-2-------------------------------
mu <- rnorm(d, 0, 0.5); sigma <- runif(d, 0.5, 1.5)
weight_func <- function(x) prod(pnorm(x, mean = mu, sd = sigma))
owes_sample(obs, sample_m, weight_func = weight_func)

## ----Custom-weight-function-example-3-------------------------------
chain_func <- function(x) (x - mu)*pnorm(x, mu, sigma) + 
  (sigma^2)*dnorm(x, mu, sigma)
mu <- 0; sigma <- 1
twcrps_sample(obs, sample_m, chain_func = chain_func)

mu <- rnorm(d, 0, 0.5); sigma <- runif(d, 0.5, 1.5)
twes_sample(obs, sample_m, chain_func = chain_func)

## ----Prepare-post-processing-example, echo=FALSE--------------------
if (use_crch) {
  # load data
  data("RainIbk", package = "crch")
  RainIbk <- sqrt(RainIbk)
  ensfc <- RainIbk[, grep('^rainfc', names(RainIbk))]
  RainIbk$ensmean <- apply(ensfc, 1, mean)
  RainIbk$enssd <- apply(ensfc, 1, sd)
  RainIbk <- subset(RainIbk, enssd > 0)  
  
  # split into training and test data
  data_train <- subset(RainIbk, as.Date(rownames(RainIbk)) <= "2004-11-30")
  data_eval <- subset(RainIbk, as.Date(rownames(RainIbk)) >= "2005-01-01")  
  
  # fit post-processing models
  CRCHgauss <- crch(rain ~ ensmean | log(enssd), data = data_train,
                    left = 0, dist = "gaussian")
  gauss_mu <- predict(CRCHgauss, data_eval, type = "location")
  gauss_sc <- predict(CRCHgauss, data_eval, type = "scale")
  
  CRCHlogis <- crch(rain ~ ensmean | log(enssd), data = data_train, 
                    left = 0, dist = "logistic")
  logis_mu <- predict(CRCHlogis, data_eval, type = "location")
  logis_sc <- predict(CRCHlogis, data_eval, type = "scale")
  
  CRCHstud <- crch(rain ~ ensmean | log(enssd), data = data_train, 
                   left = 0, dist = "student")
  stud_mu <- predict(CRCHstud, data_eval, type = "location")
  stud_sc <- predict(CRCHstud, data_eval, type = "scale")
  stud_df <- CRCHstud$df
  
  # get raw ensemble forecast
  ens_fc <- data_eval[, grep('^rainfc', names(RainIbk))] 
  obs <- data_eval$rain
}

## ----Computing-scores-Gauss-----------------------------------------
if (use_crch){
  gauss_crps <- crps_cnorm(y = obs, location = gauss_mu, scale = gauss_sc, 
                           lower = 0, upper = Inf)
}

## ----Computing-scores-logis-stud, echo=FALSE------------------------
if (use_crch){
  ens_crps <- crps_sample(obs, dat = as.matrix(ens_fc))
  logis_crps <- crps_clogis(obs, location = logis_mu, scale = logis_sc, 
                            lower = 0, upper = Inf)
  stud_crps <- crps_ct(obs, df = stud_df, location = stud_mu, scale = stud_sc, 
                       lower = 0, upper = Inf)
}

## ----Calculating-average-scores-------------------------------------
if (use_crch){
  scores <- data.frame(Logistic = logis_crps, Gaussian = gauss_crps,
                       Students_t = stud_crps, Ensemble = ens_crps)
  sapply(scores, mean)
}

## ----Post-processing-sample-----------------------------------------
if (use_crch){
  ens_size <- 1000
  n <- length(obs)
  gauss_sample <- replicate(ens_size, rnorm(n, gauss_mu, gauss_sc))
  gauss_sample[gauss_sample < 0] <- 0
}

## ----Post-processing-twcrps-Gauss-----------------------------------
if (use_crch){
  t <- sqrt(30)
  gauss_twcrps <- twcrps_sample(y = obs, dat = gauss_sample, a = t)
}

## ----Post-processing-twcrps-logis-stud, echo=FALSE------------------
if (use_crch){
  ens_twcrps <- twcrps_sample(obs, dat = as.matrix(ens_fc), a = t)
  logis_sample <- replicate(ens_size, rlogis(n, logis_mu, logis_sc))
  logis_sample[logis_sample < 0] <- 0
  logis_twcrps <- twcrps_sample(obs, dat = logis_sample, a = t)
  stud_sample <- replicate(ens_size, rt(n, df = stud_df)*stud_sc + stud_mu)
  stud_sample[stud_sample < 0] <- 0
  stud_twcrps <- twcrps_sample(obs, dat = stud_sample, a = t)
}

## ----Calculating-average-weighted-scores----------------------------
if (use_crch){
  scores <- data.frame(Logistic = logis_twcrps, Gaussian = gauss_twcrps,
                       Students_t = stud_twcrps, Ensemble = ens_twcrps)
  sapply(scores, mean)
}

## ----Post-processing-twcrps-custom-Gauss----------------------------
if (use_crch){
  sigma <- 1
  weight_func <- function(x) pnorm(x, mean = t, sd = sigma)
  chain_func <- function(x) (x - t)*pnorm(x, mean = t, sd = sigma) + 
    (sigma^2)*dnorm(x, mean = t, sd = sigma)
  gauss_twcrps <- twcrps_sample(obs, gauss_sample, chain_func = chain_func)
}

## ----Post-processing-twcrps-custom-logis-stud, echo=FALSE-----------
if (use_crch){
  ens_twcrps <- twcrps_sample(obs, dat = as.matrix(ens_fc), chain_func = chain_func)
  logis_twcrps <- twcrps_sample(obs, dat = logis_sample, chain_func = chain_func)
  stud_twcrps <- twcrps_sample(obs, dat = stud_sample, chain_func = chain_func)
}

## ----Calculating-average-weighted-scores-custom---------------------
if (use_crch){
  scores <- data.frame(CRCHlogis = logis_twcrps, CRCHgauss = gauss_twcrps,
                       CRCHstud = stud_twcrps, Ensemble = ens_twcrps)
  sapply(scores, mean)
}

## ----Data-MCMC-example, echo=FALSE----------------------------------
data("gdp", package = "scoringRules")
data_train <- subset(gdp, vint == "2014Q1")
data_eval <- subset(gdp, vint == "2015Q1" & grepl("2014", dt))

h <- 4
m <- 5000 # use modest number of MCMC iterations for computational simplicity
fc_params <- ar_ms(data_train$val, forecast_periods = h, n_rep = m)

mu <- t(fc_params$fcMeans)
Sd <- t(fc_params$fcSds)

X <- matrix(rnorm(h * m, mean = mu, sd = Sd), nrow = h, ncol = m)

obs <- data_eval$val
names(obs) <- data_eval$dt

## ----Univ-scores-MCMC-example---------------------------------------
scores_crps <- crps_sample(obs, X)
scores_logs <- logs_sample(obs, X)
print(cbind(scores_crps, scores_logs))

## ----Weighted-univ-scores-MCMC-example------------------------------
t <- 0
scores_owcrps <- owcrps_sample(obs, X, b = t)
scores_twcrps <- twcrps_sample(obs, X, b = t)
scores_cols <- clogs_sample(obs, X, b = t, cens = FALSE)
scores_cels <- clogs_sample(obs, X, b = t)
print(cbind(scores_owcrps, scores_twcrps, scores_cols, scores_cels))

## ----Custom-weight-MCMC-example-------------------------------------
a <- 0
b <- 9 
weight_func <- function(x) as.numeric((x < a) | (x > b))
chain_func <- function(x) (x < a)*(x - a) + (x > b)*(x - b) + a

## ----Custom-weighted-univ-scores-MCMC-example-----------------------
scores_owcrps <- owcrps_sample(obs, X, weight_func = weight_func)
scores_twcrps <- twcrps_sample(obs, X, chain_func = chain_func)
print(cbind(scores_owcrps, scores_twcrps))

## ----Weighted-multiv-scores-MCMC-example----------------------------
d <- 2
scores_twes <- twes_sample(obs[1:d], X[1:d, ], b = 0)
scores_twvs <- twvs_sample(obs[1:d], X[1:d, ], b = 0)
scores_twmmds <- twmmds_sample(obs[1:d], X[1:d, ], b = 0)
print(cbind(scores_twes, scores_twvs, scores_twmmds))

