# Author: Vojtech Matulik
# Time Series project
# Cast 1 (ANALYZA JEDNOROZMERNYCH NESTACIONARNICH NESEZONNICH CASOVYCH RAD) ---------------------------------------------------------------------------------------------

# Nacteni knihoven
library(readxl); library(tseries); library(forecast); library(urca); library(rmarkdown)


# Nacteni dat-----------------------------------------
data <- read_excel(path="C:\\Users\\KOKOD\\Dropbox\\My PC (DESKTOP-A6T61OC)\\Documents\\R\\TimeSeries\\projekt\\project_data.xlsx", sheet="Sheet1") 
# Smazani NA hodnot
data <- na.omit(data)
# Konverze sloupce GDP na numerický formát
data$GDP <- as.numeric(data$GDP)
# Vytvoøení èasové øady
GDP <- ts(data$GDP, start=min(data$time), frequency=1)


# Graficke zobrazeni casove rady-------------------------------
plot.ts(GDP, main="GDP v Nemecku", col=2, lwd=2, ylab="Velikost GDP", xlab="Obdobi")
# Grafické zobrazení èasové øady diferencovaného GDP
plot.ts(diff(GDP), main="První diference GDP v Nemecku", col=2, lwd=2, ylab="Prvni diference GDP", xlab="Obdobi")


# Korelogramy-------------------------

# Nastaveni vystupniho okna pro grafy 2x1
layout(matrix(1:2,2,1))

acf(GDP, main="Autokorelacni funkce (ACF) pro casovou GDP Nemecka")
pacf(GDP, main="Parcialni autokorelacni funkce (PACF) pro casovou radu GDP Nemecka")

acf(diff(GDP), main="Autokorelacni funkce (ACF) pro casovou radu prvnich diferenci GDP Nemecka")
pacf(diff(GDP), main="Parcialni autokorelacni funkce (PACF) pro casovou radu prvnich diferenci GDP Nemecka")

# Nastaveni vystupniho okna pro grafy do puvodni podoby
layout(matrix(1:1,1,1)) 


# Testovani jednotkoveho korene------------------------------
adf.test(GDP) # rozsireny Dickey-Fulleruv test, zpozdeni k = trunc((length(x)-1)^(1/3))) ci lze zvolit
pp.test(GDP) # Phillips-Perron test
kpss.test(GDP) # KPSS test

adf.test(diff(GDP))
pp.test(diff(GDP))
kpss.test(diff(GDP))


# Automaticky navrh ARIMA procesu--------------------------------
auto.arima(GDP)
auto.arima(diff(GDP))


# Odhad procesu a ulozeni jeho vystupu do objektu-------------------------------------------------
GDP_fit <- Arima(GDP, order=c(1,1,0))
summary(GDP_fit)

prehled <- function (object){
  coef <- coef(object)
  if (length(coef) > 0) {
    mask <- object$mask
    sdev <- sqrt(diag(vcov(object)))
    tpodil <- rep(NA, length(mask))
    tpodil[mask] <- coef[mask]/sdev
    pval <- 2 * pnorm(-abs(tpodil))
    setmp <- rep(NA, length(mask))
    setmp[mask] <- sdev
    sum <- rbind(coef, setmp, tpodil, pval)
    dimnames(sum) <- list(c("parametr", "s.e.", "t-podil", "p-hodnota"),
                          names(coef))
    return(sum)
  } else return(NA)
}
prehled(GDP_fit)


# Verifikace (diagnostika) modelu - zamereni se na rezidua modelu ----------------------------------------------------------------

plot.ts(GDP_fit$residuals, main="Graf rezidui")
hist(GDP_fit$residuals, main="Histogram rezidui")
boxplot(GDP_fit$residuals, main="Krabicovy graf rezidui")

layout(matrix(1:2,2,1))
acf(GDP_fit$residuals, lag.max=10, main="Autokorelacni funkce rezidui")
pacf(GDP_fit$residuals, lag.max=10, main="Parcialni autokorelacni funkce rezidui")
layout(matrix(1:1,1,1))

Box.test(GDP_fit$residuals, lag=10, type="Ljung-Box")
shapiro.test(GDP_fit$residuals)
jarque.bera.test(GDP_fit$residuals)


# Zobrazeni skutecnych a vyrovnanych hodnot------------------------------------------

plot.ts(GDP, main="GDP v Nemecku", col=2, lwd=2, ylab="Velikost GDP", xlab="obdobi")
lines(GDP_fit$fitted, col=4, lwd=2)
legend("topleft", legend=c("skutecne", "vyrovnane"), col=c(2, 4), lwd = 2)


# Predikce casove rady---------------------
#h - delka predikce
GDP_fit_forecast <- forecast(GDP_fit, h=5); GDP_fit_forecast
plot(GDP_fit_forecast, col=2, lwd=2)
  
