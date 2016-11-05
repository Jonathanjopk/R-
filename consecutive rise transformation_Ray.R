library(readxl)
data=read_excel("awesome2.xlsx")
attach(data)
CI=消費者信心
Unemploy=失業率
interest=利率
CPI=物價指數
ex=匯率

#Two consecutive rise, binary start from 1980 Q3
transform=function(index){
	bi=c()
	for(i in 1:(length(index)-2)){
		if((index[i+2]>index[i+1])&&(index[i+1]>index[i])){
			bi[i]=1}
		else{
			bi[i]=0}
	}
	return(bi)
}

#Since there are NA in CI, we need to remove NA firest to perform the transformation
#However, the binary after the transformation will start from 1982 Q4
CI=CI[!is.na(CI)]
