setwd("C:/Users/RayCheng/Dropbox/R 戳ソ厨")
rm(list=ls())
library(readxl)

data=read_excel("awesome2.xlsx",2)
attach(data)
head(data)
y=Nikkei_exp3

#First Model
reg1=lm(y~log(GDP)+CPI+Interest+CC+Unemploy+Exchange+t)
summary(reg1)
AIC(reg1)

#First model mean=64.53, max=2031
k=c()
for(j in 65:1400){
  Buy=(predict(reg1))>j

  Success=((Buy+rise)==2)  
  Fail=((Buy-rise)==1) #も(1,1)=0,(1,0)=1 ⊿Τも(0,1)=0,(0,0)=0
  if(sum(Buy,na.rm = T)>64){
  k[j]=sum(Success)/sum(Success+Fail)
  }
}
max(k,na.rm = T)
Buy=(predict(reg1))>which.max(k)
#璸衡Ω计
times=sum(Buy,na.rm = T)

#Second Model
reg2=lm(y~log(GDP)+Exchange+Unemploy_fall2+CC_rise2+Interest_fall2+CPI_rise2+CPI_fall2+t)
summary(reg2)
AIC(reg2)






#Third (Step) Model
rdc.model=lm(y~0)
fwd.model=step(rdc.model,direction = "both",scope=(~(log(GDP)+Exchange+Unemploy+CC+Interest+CPI+Unemploy_fall2+CC_rise2+Interest_fall2+CPI_rise2+CPI_fall2)^5))

rdc.model=lm(y~0)
fwd.model=step(rdc.model,direction = "both",scope=(~(log(GDP)+exchange+Unemploy_rise+CI_rise+interest_rise)^5))
summary(fwd.model)





#Third model mean=63.1, max=1382
k=c()
for(j in 60:1400){
  Buy=(predict(fwd.model))>j

  Success=((Buy+rise)==2)  
  Fail=((Buy-rise)==1) #も(1,1)=0,(1,0)=1 ⊿Τも(0,1)=0,(0,0)=0
  if(sum(Buy,na.rm = T)>64){
  k[j]=sum(Success)/sum(Success+Fail)
  }
}
max(k,na.rm = T)
Buy=(predict(fwd.model))>which.max(k)
#璸衡Ω计
times=sum(Buy,na.rm = T)






###############
t=seq(1,128,1)
reg1=lm(y~GDP+基计+瞯+禣獺み+ア穨瞯+蹲瞯+t)
############
reg=lm(y~GDP+CPI_rise+interest_rise+CI_rise+Unemploy_rise+蹲瞯)
#############
reg3=lm(nikkei~nikkei_t.m1+GDP+基计+瞯+禣獺み+ア穨瞯+蹲瞯+t)
##############
reg2=lm(y~GDP+基计+瞯+禣獺み+ア穨瞯+蹲瞯)
############
reg1=lm(y~GDP+基计+瞯+禣獺み+ア穨瞯+蹲瞯+t)
###########
reg=lm(y~GDP+CPI_rise+interest_rise+CI_rise+Unemploy_rise+蹲瞯)
############
reg=lm(y~log(GDP)+CPI_rise+interest_rise+CI_rise+Unemploy_rise+蹲瞯)

