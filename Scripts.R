attach(mert)
#'Ay' de�i�kenini kategorik veri haline getirmek i�in
counter = 1
array = {}
for(number in ay){
  if(number == 9) array[counter] = "Eyl�l"
  else if(number == 8) array[counter] = "A�ustos"
  else if(number == 7) array[counter] = "Temmuz"
  else if(number == 6) array[counter] = "Haziran"
  else if(number == 5) array[counter] = "May�s"
  counter = counter + 1
}
detach(mert)
mert[,5] = array
attach(mert)
#Ay De�i�kenini Fakt�rleme
ayFactor = factor(ay)
detach(mert)
mert[,5] = ayFactor
attach(mert)
#Her bir veri i�in basit istatistikler
summary(mert)
#Elimizdeki verilerle baz� grafikler �izdirelim
split.screen(figs=c(2,2))
screen(n=1)
plot(ozon,main="Ozon",pch=23)
screen(n=2)
plot(solarR,main="SolarRadrasyon",pch=21)
screen(n=3)
plot(ruzgar,main="R�zgar",pch = 20)
screen(n=4)
plot(sicaklik_F,main="S�cakl�k",pch = 24)
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
#Sa��l�m Grafikleri
coplot(ozon~sicaklik_F | ay)
coplot(ruzgar~sicaklik_F | ay)
#�ubuk Grafi�i
split.screen(figs=c(2,2))
screen(n=1)
barplot(ozon,main="Ozon Miktar�")
screen(n=2)
barplot(solarR,main = "Solar Radrasyon")
screen(n=3)
barplot(ruzgar,main="R�zgar")
screen(n=4)
barplot(sicaklik_F,main = "S�cakl�k")
close.screen(c(1,2,3,4),FALSE)
#Grafi�e Ortalama �zerinden �izgi �ekmek
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
#Boxplot Grafi�i
install.packages("Rcmdr")
library(Rcmdr)
split.screen(figs=c(2,2))
screen(n=1)
Boxplot(ozon~ay,col=c("red","blue","orange"),main="Ozon")
screen(n=2)
Boxplot(solarR~ay,col=c("red","blue","orange"),main="SolarRadrasyon")
screen(n=3)
Boxplot(ruzgar~ay,col=c("red","blue","orange"),main="R�zgar")
screen(n=4)
Boxplot(sicaklik_F~ay,col=c("red","blue","orange"),main="S�cakl�k")
close.screen(c(1,2,3,4),FALSE)
#Histogram Grafi�i
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
#R Commander ile Yap�lan Grafikler (R Studio kapat�l�p a��ld���nda library(Rcmdr) tekrar yaz�lmal�.)

#Normallik Testi
library(Rcmdr)
normalityTest(~ruzgar, test="shapiro.test", data=mert)
normalityTest(~sicaklik_F, test="shapiro.test", data=mert)
densityPlot(ozon~ay,with(mert, plotMeans(ozon, ay, error.bars="se", connect=TRUE)))
#Ortalamaya G�re Plot Grafi�i
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
# 'Y�ksek' De�i�kenleri
library(Rcmdr)
stripchart(yuksek_ozon ~ ay, vertical=TRUE, method="stack", ylab="yuksek_ozon", data=mert,col="red")
ifelse(sum(yuksek_ozon==1)>sum(yuksek_ozon==0),print("Ortancadan Y�ksek"),print("Ortancadan D���k"))

stripchart(yuksek_ruzgar ~ ay, vertical=TRUE, method="stack", ylab="yuksek_ruzgar", data=mert,col="Blue")
ifelse(sum(yuksek_ruzgar==1)>sum(yuksek_ruzgar==0),print("Ortancadan Y�ksek"),print("Ortancadan D���k"))

stripchart(yuksek_sicaklik ~ ay, vertical=TRUE, method="stack", ylab="yuksek_sicaklik", data=mert,col="Orange")
ifelse(sum(yuksek_sicaklik==1)>sum(yuksek_sicaklik==0),print("Ortancadan Y�ksek"),print("Ortancadan D���k"))

stripchart(yuksek_solar ~ ay, vertical=TRUE, method="stack", ylab="yuksek_solar", data=mert,col="Black")
ifelse(sum(yuksek_solar==1)>sum(yuksek_solar==0),print("Ortancadan Y�ksek"),print("Ortancadan D���k"))
#3D Grafik
library(Rcmdr)
scatter3d(sicaklik_F~ozon+ruzgar|ay, data=mert, surface=FALSE, residuals=TRUE, parallel=TRUE, bg="white", 
          axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)