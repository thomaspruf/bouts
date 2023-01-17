read_butt=function(name) {
library(lubridate)


df=read.csv(name)


 nn=names(df)
 if ((any(nn=="Date")==TRUE) & (any(nn=="Time")==FALSE)){
   df$Time="00:00:00"
 }

 nn=names(df)
 if (any(nn=="Date") & any(nn=="Time")){
   gsub("Date","Date.Time",nn)
   df$Date.Time=paste(df$Date,df$Time)
 }

 nn=names(df)
 nn=gsub ("Temperature","Temp",nn)
 nn=gsub("TemperatureC","Temp",nn)
 nn=gsub("Value","Temp",nn)
 nn=gsub("Date.Time","tme",nn)
 names(df)=nn


 df$tme=trimws(df$tme)

 if (nchar(df$tme[1])<=17) df$tme=paste(df$tme,":00",sep="")

 str1=df$tme[1]

 suppressWarnings({
 f=ff=NULL
 yy=ymd_hms (str1); if (!is.na(yy)) f=1;ff=c(ff,f)
 yy=dmy_hms (str1); if (!is.na(yy)) f=2;ff=c(ff,f)
 yy=mdy_hms (str1); if (!is.na(yy)) f=3;ff=c(ff,f)
 yy=ydm_hms (str1);  if (!is.na(yy)) f=4;ff=c(ff,f)

 })

 ff=unique(ff)

 dff=df



 for (f in ff){
 if (f==1) dff$tme=ymd_hms(df$tme)
 if (f==2) dff$tme=dmy_hms(df$tme)
 if (f==3) dff$tme=mdy_hms(df$tme)
 if (f==4) dff$tme=ydm_hms (df$tme)


 dff$Hours=difftime(dff$tme, dff$tme[1],units="hours")
 mx=max(diff(diff(dff$Hours)),na.rm=T)

 if (mx<0.05) break
 }

 invisible(dff)

}
