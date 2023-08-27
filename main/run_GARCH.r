library("here")
library("PerformanceAnalytics")
library("xts")

#loading the data from the rda file
load(file = here("data", "returns.rda"))
source(file = here("functions", "f_forecast_var.r"))

#number of returns from dataset
T = 1000

SP <- l_rets[1:T,1]
FTSE <- l_rets[1:T,2]

SP_t <- l_rets[,1]
FTSE_t <- l_rets[,2]

theta0 <- c(0.1 * var(SP), 0.1, 0.8)
f_ht(theta0, SP)
f_nll(theta = theta0, SP)

#VaR forecast of T+1 for 95% confidence
VAR_SP <- f_forecast_var(SP, 0.95)$VaR_Forecast
VAR_FTSE <- f_forecast_var(as.numeric(FTSE), 0.95)$VaR_Forecast

#Determining which is riskier
f_risky(VAR_FTSE, VAR_SP)

#BACKTESTING using rolling window
b_VAR_SP <- rep(NA, 1000)
b_VAR_FTSE <- rep(NA, 1000)

#backtest
b_VAR_SP <- f_backtestVAR(SP_t, T, 0.95)
b_VAR_FTSE <- f_backtestVAR(FTSE_t, T, 0.95)


#Plotting chart
chart_VAR_SP <- xts(b_VAR_SP, order.by = index(SP_t[(T+1):(2*T)]))
chart_VAR_FTSE <- xts(b_VAR_FTSE, order.by = index(FTSE_t[(T+1):(2*T)]))

chart_SP <- SP_t[T:(2*T)]
chart_FTSE <- FTSE_t[T:(2*T)]



png(filename = "VAR_plot.png")

par(mfrow = c(2,1))
plot(cbind(chart_VAR_FTSE, chart_FTSE),
                 main = "Realized Returns and the VaR estimates for FTSE",
                 col = c("black", "purple"),
                 wealth.index = FALSE,
                 legend.loc = "topleft")


plot(cbind(chart_VAR_SP, chart_SP),
     main = "Realized Returns and the VaR estimates for SP500",
     col = c("red", "orange"),
     wealth.index = TRUE,
     legend.loc = "bottomright")

dev.off()


 






