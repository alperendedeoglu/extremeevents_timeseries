rm(list=ls())
library(eva)
library(data.table)
library(ggplot2)
options(scipen=999)

getwd()
data<-read.csv(file="sinus.csv",  sep = "," , dec="." , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
data = setDT(data)


#co2
setDT(data)[, lag1 := shift(co2, 1L, type="lag")]
setDT(data)[, diff := co2-lag1]
setDT(data)[, diff2 := diff];setDT(data)[is.na(diff2), diff2 := 0]
data = data[1:round((0.7)*nrow(data)),];nrow(data)
dat = data$diff[!is.na(data$diff)];length(dat);tail(dat)
dat2 = -dat

#sinus
data<-read.csv(file="sinus.csv",  sep = "." , dec="," , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
data$co2 = data$sin
data = setDT(data)
setDT(data)[, diff := sin]
setDT(data)[, diff2 := diff];setDT(data)[is.na(diff2), diff2 := 0]
data = data[1:round((0.7)*nrow(data)),];nrow(data)
dat = data$diff[!is.na(data$diff)];length(dat);tail(dat)
dat2 = -dat

#bitcoin
data<-read.csv(file="bitcoin_all.csv",  sep = "." , dec="," , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
data$co2 = data$bitcoin
data = setDT(data)
setDT(data)[, diff := bitcoin]
setDT(data)[, diff2 := diff];setDT(data)[is.na(diff2), diff2 := 0]
data = data[1:round((0.7)*nrow(data)),];nrow(data)
dat = data$diff[!is.na(data$diff)];length(dat);tail(dat)
dat2 = -dat

#2. SIHS
library(extremefit)
library(eBsc)
data_sorted = sort(dat, decreasing = T)
data_sorted2 = sort(dat2, decreasing = T)

hill_estimator<-function(data_sorted){
  
    Y = c(rep(0, length(data_sorted)))
    
    for(i in 1:length(data_sorted)){
      
        for (j in 1:i){
            
            v = 0
            
            v = (1/i)* log(data_sorted[j] / data_sorted[i+1])
            Y[i] = Y[i] + v 
        }
      
    }
    
    return(Y)
  
}

Y_2 = hill_estimator(data_sorted2)


Y2 = Y[1:(which.min(!is.na(Y))-1)]
Y3 = Y_2[1:(which.min(!is.na(Y_2))-1)]
IHS<-function(Y2){
    
    ihs = c()
    for(i in 1:length(Y2)){
      
        ihs[i] = (4-i)/ (2*Y2[i]*i)

    }
    
    return(ihs)
}

plot(ihs)
min(ihs)
which.min(ihs)

ihs_sm <- eBsc(ihs)
plot(ihs_sm, full = FALSE)
min(ihs_sm$f.hat)
1 - (which.min(ihs_sm$f.hat)/length(dat))
Y[which.min(ihs_sm$f.hat)]
#co2
#(-2.45) 0.19 , 95.5% & -2.66, 0.18, 99%

#sin
#99% , -4.58  | 98.5% , -4.63

#bit
#99.5% , -10.53  | 99.5% , -13.88 

ihsl = IHS(Y3)
plot(ihsl)
min(ihsl)
1 - (which.min(ihsl)/length(dat2))

ihs_sml <- eBsc(ihsl)
plot(ihs_sml, full = FALSE)
min(ihs_sml$f.hat)
1 - (which.min(ihs_sml$f.hat)/length(dat2))
Y3[which.min(ihs_sml$f.hat)]

weights <- rep(1, length(dat))
wh <- hill(dat, w = weights)

#3. SAMSEE
YY = c(rep(0, length(data_sorted)))
for(i in 1:length(data_sorted)){
  
  for (j in 1:i){
    
    YY[i] = YY[i] + log(data_sorted[j] / data_sorted[i+1])
  }
  
}  

YY = YY[1:(which.min(!is.na(YY))-2)]
  
M2 = c()
k = 0
Y2_square = YY**2
Y2_ave = c()

k = 0
for(i in 1:length(Y2_square)){
      
      k = k + 1
      
      M2[k] = sum(Y2_square[1:k])/k
      
      Y2_ave[k] = sum(Y2[1:k])/k
  
}

k = 0
EK2 = c()
for(i in 1:length(Y2_square)){
  
    k = k + 1
    EK2[i]
    
    Y2_ave[k]
  
}

Vk2 = M2/2/Y2
GJ2 = 2*Vk2 - Y2

for(i in 3:(length(Y2_square)-2)){
  
    Vk2  
    
    
  
  
}

Y2_up = list()

k = 0
for(K in 1:length(Y2)){
  k = k + 1
  for(i in k:K){
    
    Y2_up[i][K] = sum(Y2[k:K])/(K - k + 1)
    
  }
  k = 0
}

Y2_up = c()
K = 500
k = 0
for(i in 1:K){
  
  k = k + 1
  
  Y2_up[k] = sum(Y2[1:k])/(K - k + 1)
  
}



b_up2 = Y2_up - Y2_ave[K]



plot(Y2_ave, type="l")
plot(Y2_up, type="l")

k = 0
#K = 500
SAMSEE2 = c()
for(i in 1:K){
  
  k = k + 1
  
  SAMSEE2[k] = ((GJ2[K]**2)/k) + 4*((b_up2)**2)[k]
  
}
plot(SAMSEE2[1:30], type="l")
min(SAMSEE2)

#1. GOF
tail(dat)
length(dat)

p1 = 0.10;p2=0.9

n1 =round(length(dat)*p1);n1
n2 =round(length(dat)*p2);n2
u1 = sort(dat,decreasing = T)[n1];u1
u2 = sort(dat,decreasing = T)[n2];u2

setDT(data)[, above := round(diff2 - u1, 2)]
setDT(data)[, below := round(u2 - diff2, 2)]

data_above = data[above>0, above];length(data_above)
data_below = data[below>0, below];length(data_below)

ggplot()+geom_line(data = data , aes(1:nrow(data),diff2), colour="blue", size = 1.0) + geom_hline(yintercept=u1,colour="red", size = 0.8) + geom_hline(yintercept=u2,colour="green", size = 0.8) 

gpdAd(data_above, bootstrap = T, 100)

gpdAd(
  data_above,
  bootstrap = T, 2,
  bootnum = NULL,
  allowParallel = FALSE,
  numCores = 1
)

p1 = 0.76
p2 = 0.24
p = c();s = c()
for(i in 1:45){
  
  p1 = round(p1 - 0.01 , 2)
  #p2 = round(p2 + 0.01 , 2)
  
  print(paste0("p1: ",1-p1))
  #print(paste0("p2: ",1-p2))
  
  n1 =round(length(dat)*p1);n1
  n2 =round(length(dat)*p2);n2
  u1 = sort(dat,decreasing = T)[n1];u1
  u2 = sort(dat,decreasing = T)[n2];u2
  
  setDT(data)[, above := round(diff2 - u1, 2)]
  setDT(data)[, below := round(u2 - diff2, 2)]
  
  data_above = data[above>0, above];print(length(data_above))
  #data_below = data[below>0, below];print(length(data_below))
  
  p[i] = gpdAd(data_above, bootstrap = F, 1000)$p.value
  #s[i] = gpdAd(data_below, bootstrap = T, 100)$p.value
  
}

gof<-function(p1, p2, n, r, version){
  
  s1 = c()
  s2 = c()
  k1 = c()
  k2 = c()
  r1 = 0
  r2 = 0
  if(version==1){
        
      for(i in 1:n){
        
        print(paste0("p1: ",1 - round(p1 - r*i , 2)))
        n1 =round(length(dat)*round(p1 - r*i , 2));n1
        u1 = sort(dat,decreasing = T)[n1];u1
        setDT(data)[, above := round(diff2 - u1, 2)]
        data_above = data[above>0, above];print(length(data_above))
        s1[i] = gpdAd(data_above, bootstrap = T, 100)$p.value
        
      }
      
      print(s1)
      
      for(i in 1:length(s1)){
        
        r1 = log(1 - s1[i]) + r1
        k1[i] = -r1/i
        
        if(k1[i]> 0.05){
          print(paste0("p1: ",1 - round(p1 - 0.01*(i-1) , 2)))
          break
        }
        
      }
    
  }else if(version==2){
      
      for(i in 1:n){
        
        print(paste0("p2: ", round(p2 + r*i , 2)))
        
        n2 =round(length(dat)*round(p2 + r*i , 2));n2
        u2 = sort(dat,decreasing = T)[n2];u2
        setDT(data)[, below := round(u2 - diff2, 2)]
        data_below = data[below>0, below];print(length(data_below))
        s2[i] = gpdAd(data_below, bootstrap = T, 100)$p.value
        
      }
    
      print(s2)
      
      for(i in 1:length(s2)){
        
        r2 = log(1 - s2[i]) + r2
        
        k2[i] = -r2/i
        
        if(k2[i]> 0.05){
          print(paste0("p2: ", round(p2 + 0.01*(i-1) , 2)))
          break
        }
        
      }
    
  }else{
  
      for(i in 1:n){
        
        print(paste0("p1: ",1 - round(p1 - r*i , 2)))
        print(paste0("p2: ",1 - round(p2 + r*i , 2)))
        
        n1 =round(length(dat)*round(p1 - r*i , 2));n1
        n2 =round(length(dat)*round(p2 + r*i , 2));n2
        u1 = sort(dat,decreasing = T)[n1];u1
        u2 = sort(dat,decreasing = T)[n2];u2
        
        setDT(data)[, above := round(diff2 - u1, 2)]
        setDT(data)[, below := round(u2 - diff2, 2)]
        
        data_above = data[above>0, above];print(length(data_above))
        data_below = data[below>0, below];print(length(data_below))
        
        s1[i] = gpdAd(data_above, bootstrap = T, 100)$p.value
        s2[i] = gpdAd(data_below, bootstrap = T, 100)$p.value
        
      }
      
      print(s1);print(s2)
      
      for(i in 1:length(s1)){
        
        r1 = log(1 - s1[i]) + r1
        k1[i] = -r1/i
        
        if(k1[i]> 0.05){
          print(paste0("p1: ",1 - round(p1 - 0.01*(i-1) , 2)))
          p1 = round(p1 - 0.01*(i-1) , 2)
          break
        }
        
        
      }
      for(i in 1:length(s2)){
        
        r2 = log(1 - s2[i]) + r2
        
        k2[i] = -r2/i
        
        if(k2[i]> 0.05){
          print(paste0("p2: ", round(p2 + 0.01*(i-1) , 2)))
          p2 = round(p2 + 0.01*(i-1) , 2)
          break
        }
      }
  }
  
  return(list(k1, k2, p1, p2))
}

t = gof(0.09, 0.91, 8, 0.01, 3)
#t = gof(0.76, 0.24, 75, 0.01, 3)

p1 = t[[3]];p2 = t[[4]];
p1;p2
n1 =round(length(dat)*p1);n1
n2 =round(length(dat)*p2);n2
u1 = sort(dat,decreasing = T)[n1];u1
u2 = sort(dat,decreasing = T)[n2];u2

setDT(data)[, above := round(diff2 - u1, 2)]
setDT(data)[, below := round(u2 - diff2, 2)]

data_above = data[above>0, above];length(data_above)
data_below = data[below>0, below];length(data_below)
gpdAd(data_above)
#gpdAd(data_below)
gpdAd(data_below, bootstrap = T, 1)

ggplot()+geom_line(data = data , aes(1:nrow(data),diff2), colour="blue", size = 1.0) + geom_hline(yintercept=u1,colour="red", size = 0.8) + geom_hline(yintercept=u2,colour="green", size = 0.8) 



k1 = c()
k2 = c()
r1 = 0
r2 = 0
p1 = 0.46
p2 = 0.54

for(i in 1:length(p)){
  
  r1 = log(1 - p[i]) + r1
  k1[i] = -r1/i
  
  p1 = round(p1 - 0.01 , 2)
  
  if(k1[i]> 0.05){
    print(paste0("p1: ", p1-0.01))
    break
  }
  
}

for(i in 1:length(s)){
  
  #r1 = log(1 - p[i]) + r1
  #k1[i] = -r1/i
  
  r2 = log(1 - s[i]) + r2
  k2[i] = -r2/i
  
  p2 = round(p2 + 0.01 , 2)
  
  if(k2[i]> 0.05){
     print(paste0("p2: ",1-p2+0.01))
     break
  }
  
}
k2




log(1 - s[i])


gpdAd(
  c(3,4,2),
  bootstrap = FALSE,
  bootnum = NULL,
  allowParallel = FALSE,
  numCores = 1
)

x <- rgpd(200, loc = 0, scale = 1, shape = 0.2)
gpdAd(data_below)


#Decomposition Plots
library(ggplot2)
library(knitr)
#library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
#library(TTR)
getwd()
setwd("C:/Users/HanWIS/Desktop/Master Thesis/Datasets/")
data<-read.csv(file="CO2.csv",  sep = "," , dec="." , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
data = setDT(data)

plot(data[, co2])
time <- ts(data[, co2], frequency=52)
dtime <- decompose(time)
plot(dtime)

fit <- stl(time, s.window = "periodic")

library(ggfortify)
autoplot(fit, ts.colour = 'blue')

co2 = as.data.frame(data[, co2])
library(ggplot2)
co2 %>%
  decompose() %>%
  autoplot()
                        
data<-read.csv(file="sinus.csv",  sep = "." , dec="," , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
#data = setDT(data)
data = as.data.frame(data)
head(data)
plot(data[, "sin"], type="l")
#ggplot()+geom_line(data = data , aes(1:nrow(data),diff2), colour="blue", size = 1.0)
time <- ts(c(data$sin), frequency= 500)
fit <- stl(time, s.window = "periodic")
autoplot(fit, ts.colour = 'blue')
library(ggfortify)

acf(data, lag=100)

data<-read.csv(file="bitcoin_all.csv",  sep = "." , dec="," , header=T, stringsAsFactors=FALSE,check.names=F)  #Eng Hepsi
#data = setDT(data)
data = as.data.frame(data)
head(data)
plot(data[, "bitcoin"], type="l")
#ggplot()+geom_line(data = data , aes(1:nrow(data),diff2), colour="blue", size = 1.0)
time <- ts(c(data$bitcoin), frequency= 4*365)
fit <- stl(time, s.window = "periodic")
autoplot(fit, ts.colour = 'blue' ,
         cex.lab = 14)
library(ggfortify)

