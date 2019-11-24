setwd('H:/F - Applied Time Series Analysis/Assignments/4')

# Load the RData file after changing to the directory containing it.
library("xlsx")
library(aTSA)
data_1 <- read.xlsx("seismic1.xlsx", sheetName = 'Sheet1', header=FALSE)
vk_1 = ts(data_1)
plot(vk_1)
# Upon visual inspection there seems to be no type of Integrating or Trend effects.


adf.test(vk_1, nlag=2)
# DF test rejects the null hypothesis that there is an integrating effect.
adf.test(vk_1)
# ADF test rejects the null hypothesis that there is an integrating effect.
pp.test(vk_1)
# PP test rejects the null hypothesis that there is an integrating effect.
kpss.test(vk_1)
# kpss test rejects the null hypothesis that there is an integrating effect.

data_2 <- read.xlsx("seismic2.xlsx", sheetName = 'Sheet1', header=FALSE)
vk_2 = ts(data_2)
plot(vk_2)
# Upon visual inspection there seems to be a quadratic Trend.

adf.test(vk_2, nlag=2)
# DF test rejects the null hypothesis that there is an integrating effect and
# suggests there is a drift and trend component.
adf.test(vk_2)
# ADF test rejects the null hypothesis that there is an integrating effect.
# Also suggests drift and trend.
pp.test(vk_2)
# PP test rejects the null hypothesis that there is an integrating effect.
# Also suggests drift and trend.
kpss.test(vk_2)
# kpss test rejects the null hypothesis that there is an integrating effect.
# Also suggests drift and trend.