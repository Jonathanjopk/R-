
#下季是否有漲的向量
rise=c()
for(i in 1:(length(日經)-1)){
  if(日經[i+1]-日經[i]>0){
    rise[i]=1
  }else{
    rise[i]=0
  }
}


#轉換日經指數成未來3期漲幅平均

JP_index=data$日經
result=c()
for(i in 1:length(JP_index)-3){
	result[i]=((JP_index[i+1]-JP_index[i])+(JP_index[i+2]-JP_index[i+1])+(JP_index[i+3]-JP_index[i+2]))/3
}


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