attach(mert)
#'Ay' deðiþkenini kategorik veri haline getirmek için
counter = 1
array = {}
for(number in ay){
  if(number == 9) array[counter] = "Eylül"
  else if(number == 8) array[counter] = "Aðustos"
  else if(number == 7) array[counter] = "Temmuz"
  else if(number == 6) array[counter] = "Haziran"
  else if(number == 5) array[counter] = "Mayýs"
  counter = counter + 1
}
detach(mert)
mert[,5] = array
attach(mert)
#Ay Deðiþkenini Faktörleme
ayFactor = factor(ay)
detach(mert)
mert[,5] = ayFactor
attach(mert)
#Her bir veri için basit istatistikler
summary(mert)
#Elimizdeki verilerle bazý grafikler çizdirelim
split.screen(figs=c(2,2))
screen(n=1)
plot(ozon,main="Ozon",pch=23)
screen(n=2)
plot(solarR,main="SolarRadrasyon",pch=21)
screen(n=3)
plot(ruzgar,main="Rüzgar",pch = 20)
screen(n=4)
plot(sicaklik_F,main="Sýcaklýk",pch = 24)
close.screen(c(1,2,3,4),FALSE)
#Regresyon
split.screen(figs=c(2,2))
screen(n=1)
m = lm(ozon~solarR)
plot(ozon~solarR)
abline(m)
screen(n=2)
m = lm(solarR~ruzgar)
plot(solarR~ruzgar)
abline(m)
screen(n=3)
m = lm(sicaklik_F~ruzgar)
plot(sicaklik_F~ruzgar)
abline(m)
screen(n=4)
m = lm(sicaklik_F~ozon)
plot(sicaklik_F~ozon)
abline(m)
close.screen(c(1,2,3,4),FALSE)

m = lm(solarR~sicaklik_F)
plot(solarR~sicaklik_F)
abline(m)
#Saçýlým Grafikleri
coplot(ozon~sicaklik_F | ay)
coplot(ruzgar~sicaklik_F | ay)
#Çubuk Grafiði
split.screen(figs=c(2,2))
screen(n=1)
barplot(ozon,main="Ozon Miktarý")
screen(n=2)
barplot(solarR,main = "Solar Radrasyon")
screen(n=3)
barplot(ruzgar,main="Rüzgar")
screen(n=4)
barplot(sicaklik_F,main = "Sýcaklýk")
close.screen(c(1,2,3,4),FALSE)
#Grafiðe Ortalama Üzerinden Çizgi Çekmek
split.screen(figs=c(2,2))
screen(n=1)
plot(ozon,col="Red")
m = mean(ozon)
abline(h = m,col = "Red")
screen(n=2)
plot(solarR,col="Blue")
m = mean(solarR)
abline(h = m,col = "Blue")
screen(n=3)
plot(ruzgar,col="Orange")
m = mean(ruzgar)
abline(h = m,col = "Orange")
screen(n=4)
plot(ruzgar)
m = mean(ruzgar)
abline(h = m)
close.screen(c(1,2,3,4),FALSE)
#Boxplot Grafiði
install.packages("Rcmdr")
library(Rcmdr)
split.screen(figs=c(2,2))
screen(n=1)
Boxplot(ozon~ay,col=c("red","blue","orange"),main="Ozon")
screen(n=2)
Boxplot(solarR~ay,col=c("red","blue","orange"),main="SolarRadrasyon")
screen(n=3)
Boxplot(ruzgar~ay,col=c("red","blue","orange"),main="Rüzgar")
screen(n=4)
Boxplot(sicaklik_F~ay,col=c("red","blue","orange"),main="Sýcaklýk")
close.screen(c(1,2,3,4),FALSE)
#Histogram Grafiði
split.screen(figs=c(2,2))
screen(n=1)
hist(ozon,20,probability = T,col="Red")
lines(density(ozon))
screen(n=2)
hist(solarR,20,probability = T,col="blue")
lines(density(solarR))
screen(n=3)
hist(ruzgar,20,probability = T,col="Orange")
lines(density(ruzgar),type = "l")
screen(n=4)
hist(sicaklik_F,20,probability = T,col="gray")
lines(density(sicaklik_F))
close.screen(c(1,2,3,4),FALSE)
#R Commander ile Yapýlan Grafikler (R Studio kapatýlýp açýldýðýnda library(Rcmdr) tekrar yazýlmalý.)

#Normallik Testi
library(Rcmdr)
normalityTest(~ruzgar, test="shapiro.test", data=mert)
normalityTest(~sicaklik_F, test="shapiro.test", data=mert)
densityPlot(ozon~ay,with(mert, plotMeans(ozon, ay, error.bars="se", connect=TRUE)))
#Ortalamaya Göre Plot Grafiði
library(Rcmdr)
split.screen(figs=c(2,2))
screen(n=1)
with(mert, plotMeans(ozon, ay, error.bars="se", connect=TRUE))
screen(n=2)
with(mert, plotMeans(solarR, ay, error.bars="se", connect=TRUE))
screen(n=3)
with(mert, plotMeans(ruzgar, ay, error.bars="se", connect=TRUE))
screen(n=4)
with(mert, plotMeans(sicaklik_F, ay, error.bars="se", connect=TRUE))
close.screen(c(1,2,3,4),FALSE)
# 'Yüksek' Deðiþkenleri
library(Rcmdr)
stripchart(yuksek_ozon ~ ay, vertical=TRUE, method="stack", ylab="yuksek_ozon", data=mert,col="red")
ifelse(sum(yuksek_ozon==1)>sum(yuksek_ozon==0),print("Ortancadan Yüksek"),print("Ortancadan Düþük"))

stripchart(yuksek_ruzgar ~ ay, vertical=TRUE, method="stack", ylab="yuksek_ruzgar", data=mert,col="Blue")
ifelse(sum(yuksek_ruzgar==1)>sum(yuksek_ruzgar==0),print("Ortancadan Yüksek"),print("Ortancadan Düþük"))

stripchart(yuksek_sicaklik ~ ay, vertical=TRUE, method="stack", ylab="yuksek_sicaklik", data=mert,col="Orange")
ifelse(sum(yuksek_sicaklik==1)>sum(yuksek_sicaklik==0),print("Ortancadan Yüksek"),print("Ortancadan Düþük"))

stripchart(yuksek_solar ~ ay, vertical=TRUE, method="stack", ylab="yuksek_solar", data=mert,col="Black")
ifelse(sum(yuksek_solar==1)>sum(yuksek_solar==0),print("Ortancadan Yüksek"),print("Ortancadan Düþük"))
#3D Grafik
library(Rcmdr)
scatter3d(sicaklik_F~ozon+ruzgar|ay, data=mert, surface=FALSE, residuals=TRUE, parallel=TRUE, bg="white", 
          axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)