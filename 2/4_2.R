plot(UKgas)
plot(acf(UKgas))
plot(pacf(UKgas))

with(quakes,plot(head(mag,100), type='l'))
plot(acf(quakes['mag']))
plot(pacf(quakes['mag']))

with(CO2,plot(conc, type='l'))
plot(acf(CO2['conc']))
plot(pacf(CO2['conc']))

with(CO2,plot(uptake, type='l'))
plot(acf(CO2['uptake']))
plot(pacf(CO2['uptake']))