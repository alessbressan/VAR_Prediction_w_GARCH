library("PerformanceAnalytics")
library("here")

#**** LOAD DATA **********
#*
location <- here("data","indices.rda")
load(file = location)

#**** CONVERT DATA *******

prices <- prices["2005-01-01/"]
l_rets <- Return.calculate(prices = prices, method = "log")

l_rets <- l_rets[-1]

save(l_rets, file = "data/returns.rda")
