selectData=function(){

  name=file.choose()
  hib=read_ib(name)


  plot(hib$Hours,hib$Temp,type="l",xlab="Hours",ylab="Body Temperature C",main="Mark beginning and end (two points) with mouse",cex.main=2)

  sel=identify(hib$Hours,hib$Temp,n=2,pos=T)

  first=sel$ind[1]
  last=sel$ind [2]

  abline(v=hib$Hours[first],col="red")
  abline(v=hib$Hours[last],col="red")
  Sys.sleep(.5)

  hib=hib[first:last,]
  plot(hib$Hours,hib$Temp,type="l",xlab="Hours",ylab="Body Temperature C")
  selout=list(data=hib,name=name)
  return(selout)
}
