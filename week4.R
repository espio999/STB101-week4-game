library(e1071)

mySummary=function(x) {
  myVal = data.frame(
    "Mean"=mean(x),
    "Median"=median(x),
    "Mode"=names(which.max(table(x))),
    "Standard deviation"=sd(x),
    "Standard error"=sd(x)/sqrt(length(x)),
    "Variance"=var(x),
    "Kurtosis"=kurtosis(x),
    "Skewness"=skewness(x),
    "Min"=min(x),
    "Max"=max(x),
    "Total"=sum(x),
    "Length"=length(x)
  )
  
  print(myVal)
  
  myMin=myVal$Mean-1.96*myVal$Standard.deviation/sqrt(myVal$Length)
  myMax=myVal$Mean+1.96*myVal$Standard.deviation/sqrt(myVal$Length)
  print(myMin)
  print(myMax)
  print(seekPrice(myMin, myMax, FALSE))
  print(seekPrice(myMin, myMax, TRUE))

  temp=t.test(x)
  print(temp)
  myMin=temp$conf.int[1]
  myMax=temp$conf.int[2]
  print(seekPrice(myMin, myMax, FALSE))
  print(seekPrice(myMin, myMax, TRUE))
}

seekPrice=function(myMin, myMax, flg){
  fte=15
  material=500
  logistic=100
  others=300
  discount=0.3
  
  if (flg==FALSE){
    return=c(
      fte*myMin+material+logistic+others,
      fte*myMax+material+logistic+others
    )
  }
  else{
    return=c(
      (fte*myMin+material+logistic+others)*(1-discount),
      (fte*myMax+material+logistic+others)*(1-discount)
    )
  }
}

drawChart=function(val, myStr){
  par(mfcol=c(4,1))

  #Hist=hist(val, breaks="Scott", main=myStr)
  myHist=hist(val, breaks="Sturges", main=myStr)
  par(new=TRUE)
  curve(
    dnorm(x, mean(val), sd(val)),
    xlim=c(min(myHist$breaks), max(myHist$breaks)),
    xlab="", ylab="",
    xaxt="n", yaxt="n",
    bty="n"
  )

  boxplot(val, horizontal=T)
  par(new=T)
  stripchart(val, method="stack")
}

myHour = week4$`ASM hours (Min)`
mySummary(myHour)
drawChart(myHour, "ASM hours")