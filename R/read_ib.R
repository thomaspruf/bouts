
read_ib=function(name) {
    skp=0
    ff=file(name)
    ll=readLines(ff,n=50)
    close(ff)

for (i in 1:50){
      if (substring(ll[i],1,4)=="Date" | substring(ll[i],1,4)=="Datu" ){
        skp=i
        break()
      }
    }

    if (skp==0 ){
      #Gerhard Fluch
      prel=read.csv(name)
        nm=names(prel)
        mx=max(grepl("Raw",nm))
        if (mx>0){
         tme=as.POSIXct(prel[,2],format="%Y-%m-%d %H:%M:%S",origin="CEST")
         h=difftime(tme,tme[1],units="hours")
          if (dim(prel)[2]==4){
             dframe=data.frame(Date=prel$Date, Temp=prel$Temperature, Hours=as.numeric(h))
             return(dframe)
          }
         if (dim(prel)[2]==6){
           dframe=data.frame(Date=prel$Date, Temp=prel$Temperature, Act=prel$Activity, Hours=as.numeric(h))
           return(dframe)
         }

        }
    }



    dat=read.csv(name,skip=skp)

    cols=dim(dat)[2]
    rows=dim(dat)[1]
    for (cc in 2 :cols){
      if (is.integer(dat[,cc]) & is.integer(dat[,cc-1]) )   temp=dat[,cc-1]+dat[,cc]/1000
      if (is.numeric(dat[,cc])) temp=dat[,cc]
    }

    if (is.character(dat[,2]) ) dat[,1]=paste(dat[,1],dat[,2])


    dat[,1]=  gsub("/",".",dat[,1])
    dat[,1]=  gsub("-",".",dat[,1])
    dat[,1]=  gsub(".20",".",dat[,1],fixed=T)
    dat[,1]=  gsub("  "," ",dat[,1])

    if (substr(dat[1,1],4,4)==" ") dat[,1]=substring(dat[,1],5)
    if (substr(dat[1,1],3,3)==" ") dat[,1]=substring(dat[,1],4)
    if (is.character(dat[,1]))  tme=as.POSIXct(dat[,1],format="%d.%m.%y %H:%M:%S")
    if (substring(dat[1,1],1,2)=="20")  tme=as.POSIXct(dat[,1],format="%Y.%m.%d %H:%M:%S")

    h=difftime(tme,tme[1],units="hours")
    df=data.frame(Date=tme,Temp=temp,Hours=h)
    return (df)
}
