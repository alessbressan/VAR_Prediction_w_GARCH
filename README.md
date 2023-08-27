# Stats Methods for Financial Data (HEC Montreal)
##Description
The objective is to estimate and backtest the valut-at-risk (VaR) of portfolio of two indices using the GARCH(1,1)-Normal model.

The GARCH model is used to capture the heteroscedasticity of financial data: the dynamics of volatility. It
is widely used in risk management. We used it to forecast the value-at-risk, the conditional quantile of the predictive distribution of asset returns.

### 1. Data
SP500, FTSE100 prices

### 2. Computation of VAR forecast
The function’s inputs are a (T × 1) vector of past log-returns y and the Value-at-Risk level level. The estimation of the model is done by maximum likelihood.
The function outputs the next-step-ahead VaR at the desired risk level, VaR, the (T + 1 × 1) vector of conditional variances sig2 and the set of MLE theta.

### 3. Static Estimation of VAR
Used the first T = 1000 log-returns to estimate the VaR of each index at the 95% risk level

### 4. Backtesting
Used a rolling window of T = 1000 days, computed and stored the next-step-ahead VaR at the 95% risk level for the next 1000 days

## Realized Returns and VAR Estimates
![](/img/VAR_plot.png)
