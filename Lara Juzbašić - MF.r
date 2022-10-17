library(quantmod)
library(fBasics)
library(tseries)
library(moments)
library(car)
library(sde)
library(fGarch)
options(warn=-1)


setwd("C:/Users/LARA/Desktop/Matematicke Financije - R")

###INTC
podaci1<-read.csv('INTC_zadnje2.csv',header=TRUE)
attach(podaci1)

### Graficki prikazi
# Cijene
v1<-Adj.Close
summary(v1)
podaci1[Adj.Close<=41.64,] #2019-06-03
podaci1[Adj.Close>=66.4,]  #2020-01-24
##medijan i mean su pribl jednaki sto sugerira da distr cijena treba modelirati nekom simetricnom distribucijom
par(mfrow=c(1,2))
plot(v1,type="l",col="red",main='Intel Corporation',xlab="vrijeme",ylab="cijene u trenutku zatvaranja")
par(mfrow=c(1,2))
hist(v1,freq=F,main = NULL)
boxplot(v1) 

# Log povrati za zadnje dnevne cijene
return1<-diff(log(v1))  
summary(return1)
par(mfrow=c(1,2))
plot(return1,type="l",col="blue",main='Intel Corporation',xlab="vrijeme",ylab="log povrat")
legend('bottomleft', c("Procijenjeno oèekivanje log-povrata", "Vremenski ovisan prosjek log-povrata"),
       col=c("red", "green"),lty=1:2, cex=0.8,
       box.lty=0)
#Dodajmo i prosjek log-povrata
mean(return1)
lines(seq(1,length(return1)),rep(mean(return1),length(return1)),col="red")
e<-c()
for(i in 1:length(return1)){e<-c(e,sum(return1[1:i])/i)}
lines(seq(1,length(e)),e,col="green")

par(mfrow=c(1,3))
hist(return1,breaks=100,freq=F,main='Intel Corporation',ylab = NULL,xlab='log-povrati')
curve(dnorm(x,mean(return1),sd(return1)),col="red",add=T)
##histogram sugerira da distr log povrata ima znantno veci maksimum od ove norm distr, ima i teze repove 
## dakle nije oprvdano pretpostaviti da su log povrati normalno distribuirani
boxplot(return1)
boxplot(return1,range=0)
##vidimo da nema outliera

# QQ-plot
par(mfrow=c(1,1))
qqPlot(return1*100,envelope=F,main='Intel Corporation',xlab="kvantili normalne distribucije",ylab="kvantili empirijske distribucije log-povrata")

# Testiranje hipoteze o normalnosti
kurtosis(return1)
#koeficijent spljostenosti je znacajno veci od tri sto nam sugerira da distribuciju log povrata treba traziti
##medju distribucijama s teskim repovima
jarque.bera.test(return1) #p-value < 2.2e-16
##distrib log povrata nema smisla modelirati normalnom distrib
shapiro.test(return1) #p-value < 2.2e-16

###Ericsson
detach(podaci1)
podaci2<-read.csv2('XZAG_zadnje2.csv',header=TRUE, sep = ';')
attach(podaci2)

### Graficki prikazi
# Cijene
v2<-as.numeric(Last.Price)
summary(v2)
podaci2[Last.Price==992,]
podaci2[Last.Price==1530,]
##medijan i mean su pribl jednaki sto sugerira da distr cijena treba modelirati nekom simetricnom distribucijom
par(mfrow=c(1,2))
plot(v2,type="l",col="red",main='Ericsson Nikola Tesla',xlab="vrijeme",ylab="cijene u trenutku zatvaranja")
par(mfrow=c(1,2))
hist(v2,freq=F,main='Ericsson Nikola Tesla')
boxplot(v2) 

# Log povrati za zadnje dnevne cijene
return2<-diff(log(v2))  
summary(return2)
par(mfrow=c(1,2))
plot(return2,type="l",col="blue",main='Ericsson Nikola Tesla',xlab="vrijeme",ylab="log povrat")
legend("bottomleft",c("Procijenjeno oèekivanje log-povrata", "Vremenski ovisan prosjek log-povrata"), col=c("red","green"),lwd=2, cex=0.4, inset = c(0.003,0),box.lty=0)

#Dodajmo i prosjek log-povrata
mean(return2)
lines(seq(1,length(return2)),rep(mean(return2),length(return2)),col="red")
e<-c()
for(i in 1:length(return2)){e<-c(e,sum(return2[1:i])/i)}
lines(seq(1,length(e)),e,col="green")

par(mfrow=c(1,2))
hist(return2,breaks=100,freq=F,main = 'Ericsson Nikola Tesla',xlab = 'log-povrati',ylab =NULL)
curve(dnorm(x,mean(return2),sd(return2)),col="red",add=T)
legend("topleft",c("teorijska funkcija gustoæe
normalne distribucije s parametrima procjenjenog oèekivanja i varijance log-povrata"), col=c("red"),lwd=2, cex=0.6)

##histogram sugerira da distr log povrata ima znantno veci maksimum od ove norm distr, ima i teze repove 
## dakle nije oprvdano pretpostaviti da su log povrati normalno distribuirani
boxplot(return2)
boxplot(return2,range=0)
##vidimo da nema outliera

# QQ-plot
par(mfrow=c(1,2))
qqPlot(return2*100,envelope=F,main='Ericsson Nikola Tesla',xlab="kvantili normalne distribucije",ylab="kvantili empirijske distribucije log povrata")

# Testiranje hipoteze o normalnosti
kurtosis(return2)  #13.03497
#koeficijent spljostenosti je znacajno veci od tri sto nam sugerira da distribuciju log povrata treba traziti
##medju distribucijama s teskim repovima
jarque.bera.test(return2) #p-value < 2.2e-16
##distrib log povrata nema smisla modelirati normalnom distrib
##zakljucujemo da ima smisla traziti distribuciju medju distrib s teskim repovima
shapiro.test(return2) #p-value < 2.2e-16

### Traženje normalnosti i nekoreliranosti log povrata
detach(podaci2)
podaci<-merge(podaci1,podaci2,by=c('Date'))
attach(podaci)
podaci<-podaci[c('Date', 'Last.Price', 'Adj.Close')]
v1<-as.numeric(Adj.Close)
return1<-diff(log(v1)) 
v2<-as.numeric(Last.Price)
return2<-diff(log(v2)) 
#2019-07-09 (96) do 2019-10-23 (164)
return1 <- return1[96:164]
return2 <- return2[96:164]
jarque.bera.test(return1) #p-value = 0.5961
jarque.bera.test(return2) #p-value = 0.7936
shapiro.test(return1) #p-value = 0.6115
shapiro.test(return2) #p-value = 0.41

n1<-length(return1)
box<-numeric(n1)
for(i in 1:n1){
  test<-Box.test(return1,lag=i,type="Ljung-Box")
  box[i]<-test$p.value
}
par(mfrow=c(1,2))
plot(box,ylim=c(0,1),col="blue")
abline(h=0.05,v=0,col="red",lty=2)
n2<-length(return2)
box<-numeric(n2)
for(i in 1:n2){
  test<-Box.test(return2,lag=i,type="Ljung-Box")
  box[i]<-test$p.value
}
plot(box,ylim=c(0,1),col="blue")
abline(h=0.05,v=0,col="red",lty=2)


### Portfelj
#pocetni kapital 100000
podaci$Adj.Close<-6.27*podaci$Adj.Close
podaci$portfelj_RFI<-70*podaci$Adj.Close+50*podaci$Last.Price #rizicni dio portfelja
podaci<-podaci[c(96:164),]
detach(podaci)
attach(podaci)
100000-portfelj_RFI[1] #24171.19
#svaka kosta kolko je prva cijena kad tek kupimo
podaci$portfelj_NFI<-24171.19*(1+0.001)^(1:69)
podaci$portfelj<-podaci$portfelj_NFI+podaci$portfelj_RFI
detach(podaci)
attach(podaci)
summary(portfelj)
sd(portfelj)   #2277.669

boxplot(portfelj,main="",ylab="Vrijednost portfelja u HRK",col="blue")
hist(portfelj,col="blue",main="",xlab="Vrijednost portfelja u HRK",ylab = "Frekvencije" )
par(mfrow=c(1,1))
plot(portfelj,type="l",col="red",main=NULL,xlab="Vrijeme",ylab="Vrijednost", pch=10)
par(mfrow=c(1,2))
plot(Adj.Close,type="l",col="red",main="Intel Corporation",xlab="Vrijeme",ylab="Cijene", pch=16)
plot(Last.Price,type="l",col="red",main="Ericsson Nikola Tesla",xlab="Vrijeme",ylab="Cijene", pch=16)

#log povrati
return<-diff(log(portfelj))
return<-na.omit(return)
summary(return)
sd(return)  #0.007401693
par(mfrow=c(1,1))
plot(return,col="blue",type="p",xlab="Vrijeme",ylab="Log-povrati",lwd=1.5)
lines(return,lwd=1.5)
mean(return)
abline(h=mean(return),col="red")
par(mfrow=c(1,2))
hist(return,breaks=20,probability=T, xlab = "log-povrati", ylab = "frekvencije",main='')
curve(dnorm(x,mean(return),sd(return)),col="red", add=T)
qqnorm(return, col="blue",xlab="Teorijski kvantili",ylab="Kvantili empirijske distribucije", pch=16,main = '')
qqline(return, col="red",lwd=2)


### Pretpostavke GBG
#normalnost
jarque.bera.test(return)       #p-value = 0.809
shapiro.test(return)           #p-value = 0.1746

#koreliranost
par(mfrow=c(1,1))
n<-length(return)
q1<-acf(return,lag=n,type="correlation",plot=T, main = '')
q2<-acf(return^2,lag=n,type="correlation",plot=T)
##oba graficka prikaza sugeriraju da ima smisla log povrate modelirati kao nezavisne realizacije
box<-numeric(n)
for(i in 1:n){
  test<-Box.test(return,lag=i,type="Ljung-Box")
  box[i]<-test$p.value
}
par(mfrow=c(1,1))
plot(box,ylim=c(0,1),col="blue")
abline(h=0.05,v=0,col="red",lty=2)
#p vrij za svaki lag veca od 0.05 sto znaci da na razini znac 0.05 ne odbacujemo 
#hipotezu o nekoreliranosti log povrata
#=>ne mozemo tvrditi da su log povrati normalni i ne mozemo tvrditi da su koreliirani
#nastavljamo s njima raditi kao s realizacijom jsu iz normalne distr

## Procjena parametara GBG
(sigma.hat<-sqrt(var(return))) #stand devijacija log povr u jedinicnom vrem int je par sigma
#pa ga procjenjujemo kao uzoracku stand devijaciju vektora log povrata
(mu.hat<-mean(return)) #mi je ocekivanje log povrata, procjenjujemo ga prosjekom
(alpha.hat<-mu.hat+0.5*sigma.hat^2) #alfa je ocekivana stopa log povrata
#s gbg tj procjenili smo parametre tog procesa
(alpha.hat<-mu.hat+0.5*sigma.hat^2)-0.5*(sigma.hat<-sqrt(var(return)))^2 #0.001186484


# OÈEKIVANE VRIJEDNOSTI PORTFELJA
portfelj[1] #prva realizirana(stvarna) vrijednost portfelja
Eprice<-function(t){portfelj[1]*exp(mu.hat*t+0.5*t*sigma.hat^2)} #ocekivana cijena(vrijednost u tren t)
Eprices<-numeric(length(portfelj)) #vektor u koji cemo spremit ocekivane cijene
for(t in 1:length(portfelj)){Eprices[t]<-Eprice(t)}
plot(portfelj,type="l",ylim=c(min(portfelj),max(Eprices)),xlab="vrijeme",ylab="cijena",main="Stvarne cijene")
lines(Eprices,col="red")


# Modelirane cijene (koristimo GBG s procijenjenim parametrima)
#simuliramo trajektorije kako bi dobili predikcije cijena u sljedecih 50 dana
length(portfelj)
n<-50
set.seed(123)
SGBG1<-GBM(x=portfelj[1],r=alpha.hat,sigma=sigma.hat,T=length(portfelj)+n,N=length(portfelj)+n)
SGBG2<-GBM(x=portfelj[1],r=alpha.hat,sigma=sigma.hat,T=length(portfelj)+n,N=length(portfelj)+n)
SGBG3<-GBM(x=portfelj[1],r=alpha.hat,sigma=sigma.hat,T=length(portfelj)+n,N=length(portfelj)+n)
SGBG4<-GBM(x=portfelj[1],r=alpha.hat,sigma=sigma.hat,T=length(portfelj)+n,N=length(portfelj)+n)
SGBG5<-GBM(x=portfelj[1],r=alpha.hat,sigma=sigma.hat,T=length(portfelj)+n,N=length(portfelj)+n)
plot(portfelj,type="l",xlim=c(0,length(portfelj)+n),ylim=c(min(portfelj)-5000,max(portfelj)+20000),xlab="vrijeme",ylab="cijena",main="Trajektorija cijena")
##ovo su realizacije cijena koje imamo
lines(SGBG1,col="red")
lines(SGBG2,col="blue")
lines(SGBG3,col="violet")
lines(SGBG4,col="orange")
lines(SGBG5,col="green")

set.seed(123)
SGBG1<-sde.sim(X=portfelj[1],model="BS",theta=c(mu.hat,sigma.hat),T=length(portfelj)+50,N=length(portfelj)+50,M=1)
SGBG2<-sde.sim(X=portfelj[1],model="BS",theta=c(mu.hat,sigma.hat),T=length(portfelj)+50,N=length(portfelj)+50,M=1)
SGBG3<-sde.sim(X=portfelj[1],model="BS",theta=c(mu.hat,sigma.hat),T=length(portfelj)+50,N=length(portfelj)+50,M=1)
SGBG4<-sde.sim(X=portfelj[1],model="BS",theta=c(mu.hat,sigma.hat),T=length(portfelj)+50,N=length(portfelj)+50,M=1)

par(mfrow=c(2,2))
plot(portfelj,type="l",ylim=c(min(portfelj),max(portfelj)),xlab="Vrijeme",ylab="Vrijednost u kunama")
curve(Eprice(x),add=T,col="red",lwd=2)
lines(SGBG1, col="blue")
legend("bottomright",c("Oèekivana vrijednost portfelja", "Simulirana trajektorija GBG", "stvarna vrijednost portfelja"), col=c("red","blue","black"),lwd=2, cex=0.48)

plot(portfelj,type="l",ylim=c(min(portfelj),max(portfelj)),xlab="Vrijeme",ylab="Vrijednost u kunama")
curve(Eprice(x),col="red",add=T,lwd=2)
lines(SGBG2,col="blue")
legend("bottomright",c("Oèekivana vrijednost portfelja", "Simulirana trajektorija GBG", "stvarna vrijednost portfelja"), col=c("red","blue","black"),lwd=2, cex=0.48)

plot(portfelj,type="l",ylim=c(min(portfelj),max(portfelj)),xlab="Vrijeme",ylab="Vrijednost u kunama")
curve(Eprice(x),col="red",add=T,lwd=2)
lines(SGBG3,col="blue")
legend("bottomright",c("Oèekivana vrijednost portfelja", "Simulirana trajektorija GBG", "stvarna vrijednost portfelja"), col=c("red","blue","black"),lwd=2, cex=0.48)

plot(portfelj,type="l",ylim=c(min(portfelj),max(portfelj)+1000),xlab="Vrijeme",ylab="Vrijednost u kunama")
curve(Eprice(x),col="red",add=T,lwd=2)
lines(SGBG4,col="blue")
legend("bottomright",c("Oèekivana vrijednost portfelja", "Simulirana trajektorija GBG", "stvarna vrijednost portfelja"), col=c("red","blue","black"),lwd=2, cex=0.48)



SGBG<-sde.sim(X0=portfelj[1],model="BS",theta=c(mu.hat,sigma.hat),T=length(portfelj)+50,N=length(portfelj)+50,M=200)
# svaka simulirana trajektorija je jedan stupac ovog data frame-a
estSt<-function(t)
{
  estPrices<-SGBG[t, ]  # dohvaæa t-ti redak data frame-a SGBG
  St<-mean(estPrices)
  St
}

# Usporedba stvarnih vrijednosti i prosjeka simuliranih vrijednosti u t
EstPrices<-c()
for(t in 0:(length(portfelj))){EstPrices<-c(EstPrices, estSt(t))}
length(EstPrices)
par(mfrow=c(1,1))
plot(portfelj,type="l",xlim=c(0,length(portfelj)),ylim=c(min(portfelj),max(portfelj)),xlab="Vrijeme",ylab="Vrijednost u kunama",main="",lwd=2)
lines(EstPrices,col="red",lwd=2) #prosjeci simuliranih cijena
lines(Eprices,col="darkblue",lwd=2) #ocekivane cijene (vidimo da prosjecima sim jako dobro pogadjamo ocekivane)
legend("bottomright",c("Oèekivana vrijednost portfelja","Prosjek simuliranih vrijednosti","Stvarna vrijednost portfelja"),col=c("blue","red","black"),lwd=2,cex=0.5)


# Razlike stvarnih cijena i prosjeka simuliranih cijena u t
# t=1 tj prvi dan
portfelj[1]-estSt(1) #mora bit 0 jer je to isto
# 50 dana prije T
portfelj[length(portfelj)-50]-estSt(length(portfelj)-50) #odstupanje stvarne vrijednosti od simuliranih u kunama
# 20 dana prije T
portfelj[length(portfelj)-20]-estSt(length(portfelj)-20)
# 10 dana prije T
portfelj[length(portfelj)-10]-estSt(length(portfelj)-10) # prosjeci sim cijena "precjenjuju" stvarne cijene

#iz svega vidimo da i dalje prosjeci simuliranih vrijednosti precjenjuju stvarnu vrijednost portfelja


### Mjere rizika
# Neparametarska procjena VaR-a - procjena kvantila
returnSort<-sort(return) #sortiramo od najmanjeg prema najvecem
quantile(returnSort,0.05) #VaR - kvantil reda 0.05 tako sortiranog niza, tj. 5% vrijednost niza podataka
#-0.005464426
portfelj[1]*(quantile(returnSort,0.05)) #-591.7885
#tako dobijemo iznos vaR-a u novèanim jedinicama - VaR ovdje predstavlja gubitak od
#tolko n.j. 
#procjenjujemo da je vjerojatnost da se realizira dnevni gubitak >= od 591.7885 jednaka 0.05

# Parametarska procjena VaR-a - pretpostavljena normalna distribucija log-povrata
mean(return) #procjena ocekivanja
sd(return) #procjena st dev
#ako zelimo procijeniti var moramo procijeniti kvantil normalne distribucije
(ksn<-qnorm(0.05,mean=0,sd=1)) #kvantil reda 0.05 standardne normalne distribucije
(VaR<-mean(return)+ksn*sd(return))
(VaR<-qnorm(0.05,mean=mean(return),sd=sd(return))) #-851.8342
#5% vrijednost normalne distrib s oèek i st dev koje smo procijenili kao uzoracko ocek i uzoracku st dev log povrata (dobije se isti broj)
portfelj[1]*(VaR) #var izrazen u novcanim jedinicama


##### Vrijednost portfelja ukljucivanjem opcija
#Vrijednost našeg portfelja na poèetku 
portfelj[1] #100024.2
#na kraju razdoblja 
portfelj[69] #108542
108542-100024.2
#što znaèi da smo na dobitku 8517.8 kn. Želimo poveæati dobitak.

### Bull-Call opcija
plot(Last.Price,type="p",main="",xlab="vrijeme",ylab="vrijednost u kunama", xlim=c(1, 69), xaxt='n')
axis(side = 1, at=1:69)
lines(Last.Price,col="red")

#uzlazna putanja od 41 do 53
podaci[41:53,] # rast od 11.9.2019. do 27.9.2019.
Last.Price[41:53]
podaci[47,] #2019-09-19 
Last.Price[53]
53-47
#pratimo cijene, uoèimo rast i 19.9. odluèimo kupiti ECO s datumom izvrsenja 27.8
#vrijeme izvrsenja je 6 dana

plot(Last.Price,type="l",col="red",main="Cijene ",xlab="Vrijeme",ylab="Cijene", lwd=2)
y<-Last.Price[1:47]
r1<-c( )  
for(i in 1:length(y)+1){
  r1[i]<-(y[i+1]-y[i])/y[i] 
}

r1<-na.omit(r1)
(a<-mean(r1[r1<0])) #-0.008805639
(b<-mean(r1[r1>0])) #0.008280117

r<-0.001

S01<-Last.Price[47]
S01 #1125

S11a<-S01*(1+a)
S11a #1115.094
S11b<-S01*(1+b)
S11b #1134.315

#ECO
crrECO<-function(T,t,x,S01,S11a,S11b,K,r)
{
  a<-(S11a-S01)/S01
  b<-(S11b-S01)/S01
  p<-(r-a)/(b-a)
  f1<-function(i)
  {choose(T-t,i)*((1-p)^i)*(p^(T-t-i))*max(S01*((1+a)^(t-x))*((1+b)^x)*((1+a)^i)*((1+b)^(T-t-i))-K, 0)}
  vecop<-c()
  for(i in 0:(T-t)){vecop <- c(vecop,f1(i))}
  op<-((1+r)^(t-T))*sum(vecop)
  print(op)
}
#kupljena
crrECO(T=6,t=0,x=0,S01=1125,S11a=1115.094,S11b=1134.315,K=1120,r=0.001)
#premija = 16.28116

#prodana
crrECO(T=6,t=0,x=0,S01=1125,S11a=1115.094,S11b=1134.315,K=1125,r=0.001)
#premija = 12.9772
16.28116-12.9772 #3.30396


#BSM
call.price<-function(S0,t,T,r,sigma,K)
{
  d2<-(log(S0/K)+(r-0.5*sigma^2)*(T-t))/sigma
  d1<-d2+sigma*(T-t)
  S0*pnorm(d1/sqrt(T-t))-K*exp(-r*(T-t))*pnorm(d2/sqrt(T-t))
}

S0<-1125
K<-1120
r<-log(1+0.001)
r #0.0009995003
T<-6
z<-Last.Price[47:53]
zz<-diff(log(z))
shapiro.test(zz) #0.4152
normalTest(zz,method="jb")
maxlag<-length(zz)-1
q1<-acf(zz,maxlag,type="correlation",plot=T)
(Box.test(zz,lag=maxlag,type="Ljung-Box")) #0.709
sigma<-sd(zz) 
sigma #0.01040282

#kupljena
ECO<-call.price(S0=S0,t=0,T=T,r=r,K=K,sigma=sigma)
ECO #18.16857

#prodana
C<-call.price(S0=S0,t=0,T=T,r=r,K=1125,sigma=sigma)
C #15.07941
18.16857-15.07941 #3.08916

# maksimalni gubitak smanjili na 3.08916 kn umjesto 18.16857 kn koliko iznosi premija za 
#kupnju ECO. Na taj naèin smo se i ogranièili od rizika, ali i smanjili dobit sa 
#1195-1120 =75 kn na 1125-1120=5 kn. Kako je razlika izmeðu plaæene i primljene premije 
#3.08916 kn bilježimo zaradu od
5-3.08916 #1.91084 kn
#u danu 47 na raspolaganju imamo:
portfelj_NFI[47] #25333.76
#koliko puta mozemo provesti strategiju:
portfelj_NFI[47]/3.08916 #8200.858
#uz provedbu ove strategije 8200 puta neto zarada iznosi
8200*1.91084 #=15668.89

########################################################################################
##EPO

#plot(Adj.Close,type="o",col="red",main="Cijene Intel",xlab="Vrijeme",ylab="Cijene", lwd=2)
plot(Adj.Close,type="p",main="",xlab="vrijeme",ylab="vrijednost u kunama", xlim=c(1, 69), xaxt='n')
axis(side = 1, at=1:69)
lines(Adj.Close,col="red")
podaci[11:22,] #pad od 11. do 22. dana (24.7.2019. do 12.8.2019)
podaci[15,] #kupuje 31.7. do 12.8.
22-15 #vrijeme izvršenja 7 dana

y2<-Adj.Close[1:15]
r2<-c() 
for(i in 1:length(y2)+1){
  r2[i]<-(y2[i+1]-y2[i])/y2[i] 
}

r2<-na.omit(r2)
(a<-mean(r2[r2<0])) #-0.01774633
(b<-mean(r2[r2>0])) #0.01244197

r<-0.001
S01<-Adj.Close[15]
S01 #303.6601

S11a<-S01*(1+a)
S11a #298.2713
S11b<-S01*(1+b)
S11b #307.4382

crrEPO<-function(T,t,x,S01,S11a,S11b,K,r)
{
  a<-(S11a-S01)/S01
  b<-(S11b-S01)/S01
  p<-(r-a)/(b-a)
  f1<-function(i)
  {choose(T-t,i)*((1-p)^i)*(p^(T-t-i))*max(K-S01*((1+a)^(t-x))*((1+b)^x)*((1+a)^i)*((1+b)^(T-t-i)), 0)}
  vecop<-c()
  for(i in 0:(T-t)){vecop<-c(vecop, f1(i))}
  op<-((1+r)^(t-T))*sum(vecop)
  print(op)
}

#kupljena opcija
crrEPO(T=7, t=0, x=0 ,S01=303.6601, S11a=S11a , S11b=S11b , K=320, r=0.001)
#14.68949

#prodana put opcija
crrEPO(T=7, t=0, x=0 ,S01=303.6601, S11a=S11a , S11b=S11b, K=303.6601, r=0.001)
#3.723685
14.68949-3.723685 #=10.9658

#BSM EPO
S0<-303.6601
K<-320
(r<-log(1+0.001))
r #0.0009995003
T<-7
z2<-Adj.Close[15:22] 
zz2<-diff(log(z2))
shapiro.test(zz2) #0.7471
normalTest(zz2,method="jb")
maxlag<-length(zz2)-1
q1<-acf(zz2,maxlag,type="correlation",plot=T,main="Funkcija autokovarijanci log-povrata")
(Box.test(zz2,lag=maxlag,type="Ljung-Box")) #0.8976
sigma<-sd(zz2) 
sigma #0.01343464

#EPO
put.price<-function(S0,t,T,r,sigma,K)
{
  d2<-(log(S0/K)+(r-0.5*sigma^2)*(T-t))/sigma
  d1<-d2+sigma*(T-t)
  K*exp(-r*(T-t))*pnorm(-d2/sqrt(T-t))-S0*pnorm(-d1/sqrt(T-t))
}

#kupljena
P<-put.price(S0=S0,t=0,T=T,r=r,K=K,sigma=sigma)
P #14.63578

#prodana
P<-put.price(S0=S0,t=0,T=T,r=r,K=303.6601,sigma=sigma)
P #3.315025
14.63578-3.315025 #=11.32076

#cijena dionice na 22.dan
Adj.Close[22] #275.7743 
# maksimalni gubitak smanjili na 11.32076 kn umjesto 14.63578 kn koliko iznosi premija za kupnju ECO.
#Na taj naèin smo se i ogranièili od rizika, ali i smanjili dobit sa
320-275.7743#=44.2257 kn na 
320-303.6601 #=16.3399 kn. Kako je razlika izmeðu plaæene i primljene premije 11.32076 kn bilježimo zaradu od
16.3399-11.32076 #5.01914 kn
# u danu 15 imamo na raspolaganju:
portfelj_NFI[15] #24536.31
#mozemo provesti strategiju ovoliko puta:
portfelj_NFI[15]/11.32076 #2167.373
#uz provedbu ove strategije 2167 puta neto zarada iznosi
2167*5.01914 #=10876.48