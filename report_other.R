
#�U�u�O�_�������V�q
rise=c()
for(i in 1:(length(��g)-1)){
  if(��g[i+1]-��g[i]>0){
    rise[i]=1
  }else{
    rise[i]=0
  }
}


#�ഫ��g���Ʀ�����3�����T����

JP_index=data$��g
result=c()
for(i in 1:length(JP_index)-3){
	result[i]=((JP_index[i+1]-JP_index[i])+(JP_index[i+2]-JP_index[i+1])+(JP_index[i+3]-JP_index[i+2]))/3
}


library(readxl)
data=read_excel("awesome2.xlsx")
attach(data)
CI=���O�̫H��
Unemploy=���~�v
interest=�Q�v
CPI=��������
ex=�ײv

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