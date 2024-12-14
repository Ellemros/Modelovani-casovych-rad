#install.packages(c("tseries", "forecast", "readxl", "urca", "vars", "tsDyn", "multiwave", "mlVAR", "MTS", "arfima", "rugarch", "fGarch", "FinTS"))

library(tseries)
library(forecast)
library(readxl)
library(urca)
library(vars)
###################################
# Prakticky datovy soubor - HDP, mzdy
###################################

data_HDP_mzdy <- read_excel(path="C:\\Users\\KOKOD\\Dropbox\\My PC (DESKTOP-A6T61OC)\\Documents\\R\\TimeSeries\\projekt\\projekt2_data.xlsx", sheet="Sheet1") # nacteni datoveho souboru z Excelu
HDP <- ts(data_HDP_mzdy$HDP, start=c(2001,1), frequency=4) # data jako casova rada
mzdy <- ts(data_HDP_mzdy$mzdy, start=c(2001,1), frequency=4) # data jako casova rada
hdpmzdy <- cbind(HDP, mzdy) 

# Graficke zobrazeni casove rady
#-------------------------------

plot.ts(HDP, main="Mezictvrtletni vyvoj HDP v CR", col=2, lwd=2, ylab="HDP", xlab="obdobi")
plot.ts(mzdy, main="Mezictvrtletni vyvoj mezd a platu v CR", col=2, lwd=2, ylab="mzdy", xlab="obdobi")

adf.test(HDP)
adf.test(mzdy)

# Vytvorit sezonni diference 
# Create seasonal differences for both HDP and mzdy
HDP_diff <- diff(HDP, lag = 4)
mzdy_diff <- diff(mzdy, lag = 4)
hdpmzdy <- cbind(HDP_diff, mzdy_diff) 

# Plot the seasonal differences
plot.ts(HDP_diff, main = "Sezonni diference HDP v CR", col = 2, lwd = 2, ylab = "HDP", xlab = "obdobi")
plot.ts(mzdy_diff, main = "Sezonni diference mezd v CR", col = 2, lwd = 2, ylab = "mzdy", xlab = "obdobi")

pp.test(HDP_diff) # Phillips-Perron test
pp.test(mzdy_diff) # Phillips-Perron test

# Krizovy korelogram
#-------------------

ccf(HDP_diff, mzdy_diff)


# Automaticky navrh VAR modelu
#-----------------------------

VARselect(hdpmzdy, type = "const") # idk zeptat se
VARselect(hdpmzdy, type = "trend")
VARselect(hdpmzdy, type = "both")
VARselect(hdpmzdy, type = "none")


# Odhad VAR modelu
#-----------------
# p znamena jako rad ktery tam ma byt
hdpmzdy.odhad1 <- VAR(hdpmzdy, p = 5, type = "const"); summary(hdpmzdy.odhad1)
hdpmzdy.odhad4 <- VAR(hdpmzdy, p = 5, type = "trend"); summary(hdpmzdy.odhad4)
hdpmzdy.odhad3 <- VAR(hdpmzdy, p = 5, type = "both"); summary(hdpmzdy.odhad3)
hdpmzdy.odhad2 <- VAR(hdpmzdy, p = 5, type = "none"); summary(hdpmzdy.odhad2)


# Analyza zvoleneho VAR modelu
#-----------------------------
# Resim ktery odhad je nejlepsi 
plot(hdpmzdy.odhad4)

serial.test(hdpmzdy.odhad4)
plot(serial.test(hdpmzdy.odhad4))

arch.test(hdpmzdy.odhad4)

normality.test(hdpmzdy.odhad4)


# Dekompozice chyby predpovedi
#-----------------------------

fevd(hdpmzdy.odhad4, n.ahead = 10)
plot(fevd(hdpmzdy.odhad4, n.ahead = 10))


# Stabilita parameteru VAR modelu
#--------------------------------

plot(stability(hdpmzdy.odhad4, type = "Rec-CUSUM"))


# Predikce VAR modelu
#--------------------

predict(hdpmzdy.odhad4, n.ahead = 10, ci = 0.95)
plot(predict(hdpmzdy.odhad4, n.ahead = 10, ci = 0.95))


# Impulse-Response analyza
#-------------------------

irf(hdpmzdy.odhad4, impulse = "HDP_diff", response = "mzdy_diff", boot = TRUE)
plot(irf(hdpmzdy.odhad4, impulse = "HDP_diff", response = "mzdy_diff", boot = TRUE))

irf(hdpmzdy.odhad4, impulse = "mzdy_diff", response = "HDP_diff", boot = TRUE)
plot(irf(hdpmzdy.odhad4, impulse = "mzdy_diff", response = "HDP_diff", boot = TRUE))


#---------------------Grangerova kauzalita
#---------------------

causality(hdpmzdy.odhad4, cause="HDP_diff")

causality(hdpmzdy.odhad4, cause="mzdy_diff")

