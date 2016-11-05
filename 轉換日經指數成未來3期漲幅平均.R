#轉換日經指數成未來3期漲幅平均

JP_index=data$日經
result=c()
for(i in 1:length(JP_index)-3){
	result[i]=((JP_index[i+1]-JP_index[i])+(JP_index[i+2]-JP_index[i+1])+(JP_index[i+3]-JP_index[i+2]))/3
}

