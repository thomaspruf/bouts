selectData=function () 
{
  name = file.choose()
  hib = read_butt(name)
  
  plot(hib$Hours, hib$Temp, type = "l", xlab = "Hours", ylab = "Body Temperature C", 
       main = "Mark beginning and end (two points) with left mouse clicks", 
       cex.main = 1.5)
  sel = identify(hib$Hours, hib$Temp, n = 2, pos = T)
  first = sel$ind[1]
  last = sel$ind[2]
  
  abline(v = hib$Hours[first], col = "red")
  abline(v = hib$Hours[last], col = "red")
  Sys.sleep(0.5)
  hib = hib[first:last, ]
  
  yy=substr(hib$tme[1],1,4)
 
  print(paste(yy,"-01-01 00:00:00",sep=""))
  z1=as.POSIXct(paste(yy,"-01-01 00:00:00",sep=""),tz="CET")         
  z2=as.POSIXct(hib$tme, tz="CET")
  hib$Hours=difftime(z2,z1,units="hours")
  
  plot(hib$Hours, hib$Temp, type = "l", xlab = "Hours", ylab = "Body Temperature C")
  selout = list(data = hib, name = name)
  return(selout)
}
