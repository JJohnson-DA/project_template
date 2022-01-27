# Package and Data Load ========================================================
# Load Packages
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

# Load Data
data_raw <- read.csv('./data/clean/daily_data.csv')
glimpse(data_raw)

data <- data_raw %>%
  mutate(l1 = lag(spend, 1)
         ,l7 = lag(spend, 7)
         ,l1_spend_delta = spend - l1
         ,l7_spend_delta = spend - l7
  ) %>%
  na.omit()

glimpse(data)

# Create Time Series Objects for variables
cpa <- ts(data$cpa, start = c(2019, 1, 09), frequency = 365)
spend <- ts(data$spend, start = c(2019, 1, 09), frequency = 365)
l7_spend_delta <- ts(data$l7_spend_delta, start = c(2019, 1, 09), frequency = 365)
l1_spend_delta <- ts(data$l7_spend_delta, start = c(2019, 1, 09), frequency = 365)


# Pre Checks ===================================================================

# Visualize each series
plot(cpa, main='CPA - Daily', ylab='Dollars ($$$)')
plot(l7_spend_delta, main='Change in Spend - Lag(7)', ylab='Dollars ($$$)')

# Scatter plot for cpa and spend change
ggplot(filter(data, cpa<50)) +
  geom_point(aes(x = cpa, y = l7_spend_delta))


# Check if data are stationary
pp.test(cpa) # Yes < 0.01
pp.test(l7_spend_delta) # Yes < 0.01


# Bind cpa and spend_delta
ts1 <- cbind(cpa, l7_spend_delta)
colnames(ts1) <- cbind('cpa', 'l7_spend_delta')


# Find optimal lag value
# ts1 = 29
lagselect <- VARselect(ts1, lag.max=50, type='const')
lagselect$selection
lagselect$criteria


# Modeling =====================================================================
# Fit - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Fit Model with optimal lags found above
mod1 <- VAR(ts1, p=29, type='const', exog=NULL, season=NULL)
summary(mod1)


# Model Diagnostics - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Autocorrelation of Residuals - Failed
(serialcorr <- serial.test(mod1))


# Heteroscedasticity - Failed
(arch <- arch.test(mod1))


# Normality - Failed
(norm <- normality.test(mod1, multivariate.only = TRUE))


# Stability - Passed
stab <- stability(mod1, type = "OLS-CUSUM")
plot(stab)


# Check Granger Causality for Spend Change on cpa
(grangerCPA <- causality(mod1, cause='l7_spend_delta'))


# IRF Checks for spend_delta on cpa
cpaIRF <- irf(mod1, impulse = "l7_spend_delta", response = "cpa", n.ahead = 20, boot = TRUE)
plot(cpaIRF, ylab = "Lag (7) Spend Delta", main = "Spend Delta Shock to CPA")


# Forecast Error Variance Decomposition
FEVD1 <- fevd(mod1, n.ahead = 30)
FEVD1
plot(FEVD1)
