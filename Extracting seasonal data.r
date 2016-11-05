library(readxl)
data=read_excel("awesome2.xlsx",1)
attach(data)

Code=as.Date(Code,format="%Y%m%d") #把亂亂的格式轉變一下
season=as.Date("1979-12-31") #set any date that let season become a time variable
for(i in 1:35){
a=Code[grep(eval(1979+i),Code)] #select first quarter of years
b=a[grep(eval("-03-"),a)]
season=c(season,max(b)) #max(b) will find the last date of March

a=Code[grep(eval(1979+i),Code)] #select second quarter of years
b=a[grep(eval("-06-"),a)]
season=c(season,max(b))

a=Code[grep(eval(1979+i),Code)] #select third quarter of years
b=a[grep(eval("-09-"),a)]
season=c(season,max(b))

a=Code[grep(eval(1979+i),Code)] #select forth quarter of years
b=a[grep(eval("-12-"),a)]
season=c(season,max(b))
}

season=season[2:length(season)] #eliminate first element "1979-12-31" from our data

#Get the location of our quarterly exchange rate
address=c()
for(i in 1:length(season)){
	address[i]=which(Code==season[i])
}

exchange=data$匯率
exchange=exchange[address]
seasonal.data=data.frame(season,exchange) #The final result