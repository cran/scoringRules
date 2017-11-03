## ----include=FALSE-------------------------------------------------------
library(knitr)
render_sweave()
opts_chunk$set(engine='R', tidy=FALSE)
options(scipen = 1, digits = 3)

## ----preliminaries, echo=FALSE, results='hide'----------------------
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(scoringRules)
library(crch)

## ----echo=FALSE-----------------------------------------------------
rm(list=ls())
library(scoringRules)
set.seed(42)

## -------------------------------------------------------------------
obs <- rnorm(10)
crps(obs, family = "normal", mean = c(1:10), sd = c(1:10))
logs(obs, family = "normal", mean = c(1:10), sd = c(1:10))

## -------------------------------------------------------------------
crps_y <- function(y) crps(y, family = "gamma", shape = 2, scale = 1.5)
logs_y <- function(y) logs(y, family = "gamma", shape = 2, scale = 1.5)

## ----score-illustration, echo=FALSE, dev='pdf', fig.width=10.4, fig.height=4.1, fig.align="center", out.width = "\\linewidth"----
par(mai = c(0.9, 3.5, 0.3, 3.4), cex = 1.1)
plot(NULL, type = "n", xlim = c(0, 9), ylim = c(0, 5), bty = "n", 
     xlab = "Observation y", ylab = "Score value",
     xaxt = "n", yaxt = "n")
axis(1, at = c(0, 4, 8))
axis(2, at = c(0, 2.5, 5))
z <- seq(0, 9, .01)
bg <- 15 * dgamma(z, shape = 2, scale = 1.5)
polygon(c(z, rev(z)), c(rep(0, length(bg)), rev(bg)), 
        col="gray80", border="gray80")
legend("top", bty = "n", legend = c("LogS", "CRPS"), 
       col = c("purple", "darkorange"), lty = c(1,1), 
       lwd = c(2,2))
plot(crps_y, from = 0, to = 9, col = "darkorange", lwd = 2, add = TRUE)
plot(logs_y, from = 0, to = 9, col = "purple", lwd = 2, add = TRUE)

## ----message=FALSE--------------------------------------------------
obs_n <- c(0, 1, 2)
sample_nm <- matrix(rnorm(3e4, mean = 2, sd = 3), nrow = 3)
crps_sample(obs_n, dat = sample_nm)
logs_sample(obs_n, dat = sample_nm)

## ----message=FALSE--------------------------------------------------
obs_1 <- obs_n[1]
sample_m <- sample_nm[1, ]
mgrid <- seq(from = 50, to = length(sample_m), by = 50)
crps_approx <- logs_approx <- numeric(length(mgrid))
for (i in seq_along(mgrid)) {
  size <- mgrid[i]
  crps_approx[i] <- crps_sample(obs_1, dat = sample_m[1:size])
  logs_approx[i] <- logs_sample(obs_1, dat = sample_m[1:size])
}

## ----plot1, echo=FALSE, dev='pdf', fig.width=10.4, fig.height=4.2, fig.align ="center", out.width = "\\linewidth"----
crps_true <- crps(obs_1, family = "normal", mean = 2, sd = 3)
logs_true <- logs(obs_1, family = "normal", mean = 2, sd = 3)

par(mai = c(.9, .9, .3, .3), pty = "s", cex = 1.1, omi = c(0, .7, 0, 1.3))
layout(matrix(1:2, nrow = 1))
plot(NULL, type = "n", bty = "n",
main = "CRPS", xlab = "Sample size", ylab = "Score value",
xlim = c(0, 1e4), ylim = crps_true * c(0.9, 1.1),
xaxt = "n", yaxt = "n")
axis(1, at = c(2000, 6000, 10000))
axis(2, at = c(1.1, 1.2, 1.3))
abline(h = crps_true, lty = 2)
lines(mgrid, crps_approx, col = "darkorange", lwd = 2)

plot(NULL, type = "n", bty = "n",
main = "LogS", xlab = "Sample size", ylab = "",
xlim = c(0, 1e4), ylim = logs_true * c(0.9, 1.1),
xaxt = "n", yaxt = "n")
axis(1, at = c(2000, 6000, 10000))
axis(2, at = c(2.0, 2.2, 2.4))
abline(h = logs_true, lty = 2)
lines(mgrid, logs_approx, col = "purple", lwd = 2)

## -------------------------------------------------------------------
library(crch)
data(RainIbk)
RainIbk <- sqrt(RainIbk)
RainIbk$ensmean <- apply(RainIbk[,grep('^rainfc',names(RainIbk))], 1, mean)
RainIbk$enssd <- apply(RainIbk[,grep('^rainfc',names(RainIbk))], 1, sd)
RainIbk <- subset(RainIbk, enssd > 0)

## -------------------------------------------------------------------
data_train <- subset(RainIbk, as.Date(rownames(RainIbk)) <= "2004-11-30")
data_eval <- subset(RainIbk, as.Date(rownames(RainIbk)) >= "2005-01-01")

## -------------------------------------------------------------------
CRCHgauss <- crch(rain ~ ensmean | log(enssd), data_train,
  dist = "gaussian", left = 0)
gauss_mu <- predict(CRCHgauss, data_eval, type = "location")
gauss_sc <- predict(CRCHgauss, data_eval, type = "scale")

## ----echo=FALSE-----------------------------------------------------
CRCHlogis <- crch(rain ~ ensmean | log(enssd), data = data_train, 
left = 0, dist = "logistic")
CRCHstud <- crch(rain ~ ensmean | log(enssd), data = data_train, 
left = 0, dist = "student")
logis_mu <- predict(CRCHlogis, data_eval, type = "location")
logis_sc <- predict(CRCHlogis, data_eval, type = "scale")
stud_mu <- predict(CRCHstud, data_eval, type = "location")
stud_sc <- predict(CRCHstud, data_eval, type = "scale")
stud_df <- CRCHstud$df

## -------------------------------------------------------------------
ens_fc <- data_eval[, grep('^rainfc', names(RainIbk))]

## ----postprocplot, echo=FALSE, dev='pdf', fig.width=10.4, fig.height = 3.7, fig.align="center", out.width="\\linewidth"----
ID.list <- c(206,953,2564)

m <- matrix(c(1, 2, 3), nrow = 1)
layout(mat = m, widths = c(3.55, 2.95, 3.90))
par(pty = "s", cex = 1.1)

for(ID in ID.list){
  col.logis <- "blue"
  col.gauss <- "green3"
  col.stud <- "darkorange"
  
  z <- seq(0,10,0.01)
  flogis.plot <- suppressWarnings(flogis(z, logis_mu[ID], logis_sc[ID], lower = 0, lmass = "cens"))
  flogis.p0 <- plogis(0, logis_mu[ID], logis_sc[ID])
  fnorm.plot <- suppressWarnings(fnorm(z, gauss_mu[ID], gauss_sc[ID], lower = 0, lmass = "cens"))
  fnorm.p0 <- pnorm(0, gauss_mu[ID], gauss_sc[ID])
  fstud.plot <- suppressWarnings(ft(z, stud_df, stud_mu[ID], stud_sc[ID], lower = 0, lmass = "cens"))
  fstud.p0 <- pt(-stud_mu[ID] / stud_sc[ID], stud_df)
  
  if (ID == ID.list[1]) par(mai = c(0.9, 0.9, 0.3, 0.15))
  if (ID == ID.list[2]) par(mai = c(0.9, 0.3, 0.3, 0.15))
  if (ID == ID.list[3]) par(mai = c(0.9, 0.3, 0.3, 1.1))
  
  plot(NULL, type = "n", bty = "n", xaxt = "n", yaxt = "n",
       ylim = c(-0.025, 0.5), xlim = c(-0.4,10),
       ylab = ifelse(ID == ID.list[1], "Density", ""), xlab = "Precipitation amount in mm",
       main = rownames(data_eval)[ID])
  axis(1, at = c(0, 5, 10))
  if (ID == ID.list[1]) axis(2, at = c(0, 0.25, 0.5))
  
  lines(z, flogis.plot, col = col.logis)
  lines(z, fnorm.plot, col = col.gauss)
  lines(z, fstud.plot, col = col.stud)	  
  
  p0.offset <- 0.2
  segments(0, 0, 0, flogis.p0, col = col.logis, lwd = 3)	  
  segments(-p0.offset, 0, -p0.offset, fnorm.p0, col = col.gauss, lwd = 3)	  
  segments(-2*p0.offset, 0, -2*p0.offset, fstud.p0, col = col.stud, lwd = 3)
  
  segments(data_eval$rain[ID], 0, data_eval$rain[ID], 0.5, lty = 2)
  
  ens.fc <- as.numeric(data_eval[, grep('^rainfc',names(RainIbk))][ID,])
  for (j in 1:length(ens.fc)) {
    segments(ens.fc[j], -0.025, ens.fc[j], -0.005)
  }
}

par(xpd = TRUE)
legend(6.5, .45, legend = c("cens. logistic", "cens. Gaussian", "cens. Student's t"),
       lty = rep(1,3), col = c("blue", "green3", "darkorange"), ncol = 1, bty ="n")

## -------------------------------------------------------------------
obs <- data_eval$rain
gauss_crps <- crps(obs, family = "cnorm", location = gauss_mu, 
  scale = gauss_sc, lower = 0, upper = Inf)
ens_crps <- crps_sample(obs, dat = as.matrix(ens_fc))

## ----echo=FALSE-----------------------------------------------------
logis_crps <- crps(obs, family = "clogis", location = logis_mu, 
scale = logis_sc, lower = 0, upper = Inf)
stud_crps <- crps(obs, family = "ct", df = stud_df, location = stud_mu, 
scale = stud_sc, lower = 0, upper = Inf)

## ----echo=FALSE-----------------------------------------------------
df <- data.frame(mean(logis_crps), mean(gauss_crps), mean(stud_crps), mean(ens_crps))
names(df) <- c("CRCHlogis", "CRCHgauss", "CRCHstud", "Ensemble")
print(df, row.names = FALSE)

## ----echo=TRUE------------------------------------------------------
data(gdp, package = "scoringRules")
data_train <- subset(gdp, vint == "2014Q1")
data_eval <- subset(gdp, vint == "2015Q1" & grepl("2014", dt))

## -------------------------------------------------------------------
h <- 4; m <- 20000
fc_params <- ar_ms(data_train$val, forecast_periods = h, n_rep = m)

## -------------------------------------------------------------------
mu <- t(fc_params$fcMeans)
Sd <- t(fc_params$fcSds)

## -------------------------------------------------------------------
X <- matrix(rnorm(h * m, mean = mu, sd = Sd), nrow = h, ncol = m)

## ----mcmcplot, echo=FALSE, dev='pdf', fig.width=10.4, fig.height = 3.2, fig.align = "center", out.width="\\linewidth"----
fmix <- function(m, s) {
function(x) {
40000 * sapply(x, function(z) mean(dnorm(z, mean = m, sd = s)))
}
}
	
# Plot histograms
    layout(mat = matrix(1:4, nrow = 1), widths = c(3.05, 2.45, 2.45, 2.45))
    par(mai = c(0.9, 0.9, 0.3, 0.15), pty = "s", cex = 1.1)
for (jj in seq_along(data_eval$dt)) {
  act <- data_eval$val[jj]
  x <- X[jj, ]
  
  hist(x, main = data_eval$dt[jj],
       xlim = c(-20, 20), ylim = c(0, 8000),
       xlab = "Growth rate in %", ylab = ifelse(jj == 1, "Frequency", ""),
       xaxt = "n", yaxt = "n")
  axis(1, at = c(-20, 0, 20))
  if (jj == 1) axis(2, at = c(0, 4000, 8000))
  segments(act, 0, act, 8000, lty = 2)
  plot(fmix(mu[jj, ], Sd[jj, ]), from = min(x), to = max(x), lwd = 2, add = TRUE)

      if (jj == 1) par(mai = c(0.9, 0.3, 0.3, 0.15))
}

## ----message=FALSE--------------------------------------------------
obs <- data_eval$val
names(obs) <- data_eval$dt
w <- matrix(1/m, nrow = h, ncol = m)
crps_mpe <- crps(obs, "normal-mixture", m = mu, s = Sd, w = w)
logs_mpe <- logs(obs, "normal-mixture", m = mu, s = Sd, w = w)
crps_ecdf <- crps_sample(obs, X)
logs_kde <- logs_sample(obs, X)
print(cbind(crps_mpe, crps_ecdf, logs_mpe, logs_kde))

## ----echo=FALSE-----------------------------------------------------
names(obs) <- NULL

## ----results='hold'-------------------------------------------------
es_sample(obs, dat = X)
vs_sample(obs, dat = X)

