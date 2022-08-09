bcounts=function(data, name="exm",thresh=28, min_torpid=NULL, min_wake=NULL,t_res=5){

hib=data

fac=mean(diff(hib$Hours),na.rm=T)*60/t_res #t_res is temporal resolution in minutes
nh=length(hib$Hours)
ap=approx(hib$Hours,hib$Temp,n=nh*fac)
dtm=ap$x
Temp=ap$y
torpor=ifelse (Temp<thresh,0,1)
n=length(torpor)
delta=diff(dtm)[1]


if (!is.null (min_torpid)){
  ntorp=floor(min_torpid/delta)        # At least 24 h torpid
  # hibernation Onset
  for (i in 1:(n-ntorp-1)) {
    if (sum(torpor[i:(i+ntorp)])==0) break
  }
 wsa=i
}else{
 wsa=1
}

hibstart=dtm[wsa]
#abline(v=hibstart,col="blue")


if (!is.null (min_wake)){
  nwake=floor(min_wake/delta)        #at least 24 h awake
  for (i in wsa:(n-nwake-1)) {
    if (sum(torpor[i:(i+nwake)])==nwake) break
  }
  if (i==(n-nwake-1)) wse=n-1 else wse=i+1
}else{
  wse=length(ap$x)-1
}
hibend=dtm[wse]
#abline(v=hibend,col="blue")


hibdur=hibend-hibstart


acti=grep("Act",names(hib))
if(length(acti)>0){
  apact=approx(hib$Hours,hib$Act,n=nh*fac)
  dtm=apact$x
  Act=apact$y
} else{
  dtm=ap$x
  Act=rep(NA,length(dtm))
}


plot(dtm, Temp,col="red",cex=0.2)

plot(Temp~dtm,type="o",pch=".",ylim=c(-5,40),col="grey40",xlab="Hours",ylab="Body Temperature C",cex.lab=1)

points(Act/20~dtm,type="o",pch=".",col="blue")
mtext(side=4,line=1,"Activity",adj=0.15,col="blue")
abline(h=thresh,lty=3)

# Number Arousals & Torporbouts
hibern=torpor[wsa:wse]
ar=sum(diff(hibern)==1)
to=sum(diff(hibern)==-1)
change=diff(torpor)

#Length of arousals
ar_actv<-tme_at_tb_max<-ar_len<-ar_onset<-tb_max<-rep(0,ar)
ar_no=0
ar_start=wsa
for (i in wsa:wse) {
  if (change[i]==1){
    ar_no=ar_no+1
    ar_start=i
    ar_onset[ar_no]=dtm[i]
    points(dtm[i],Temp[i],col="red",bg="red",pch=24)
  }

  if (change[i]==-1) {
    ar_end=i
    points(dtm[i],Temp[i],col="blue",pch=25,bg="blue")
    ar_len[ar_no]=dtm[ar_end]-dtm[ar_start]
    tb_max[ar_no]=max(Temp[ar_start:ar_end])
    now=which(Temp[ar_start:ar_end]==tb_max[ar_no])[1]
    tme_at_tb_max[ar_no]=dtm[ar_start+now]
    ar_actv[ar_no]=mean(Act[ar_start:ar_end],na.rm=T)
  }


}
points(tme_at_tb_max,tb_max,col="orange",pch=19)


#Length of torporbouts
to_actv<-to_len<-to_onset<-tb_min<-tme_at_tb_min<-rep(0,ar)
to_no=0
to_start=0
for (i in wsa:(wse-1)) {
  if (change[i]==-1){
    to_no=to_no+1
    to_start=i
    to_onset[to_no]=dtm[i]
  }
  if (change[i]==1) {
    to_end=i
    to_len[to_no]=dtm[to_end]-dtm[to_start]
    tb_min[to_no]=min(Temp[to_start:to_end])
    now=which(Temp[to_start:to_end]==tb_min[to_no])[1]
    tme_at_tb_min[to_no]=dtm[to_start+now]
    to_actv[to_no]=mean(Act[to_start:to_end],na.rm=T)
  }
}
points(tme_at_tb_min,tb_min,col="salmon",pch=19)

# cut off last arousal
nb=to_no
ar_len[nb]=NA
tb_max[nb]=NA
to_actv[to_no]=NA
index=1:nb

to_onset=round(to_onset[1:nb],2)
to_len=round(to_len[1:nb],2)
tb_min=round(tb_min[1:nb],2)
to_actv=round(to_actv[1:nb],2)
ar_onset=round(ar_onset[1:nb],2)
ar_len=round(ar_len[1:nb],2)
tb_max=round(tb_max[1:nb],2)
ar_actv=round(ar_actv[1:nb],2)

sname="Example"

ix=max(gregexpr("/",name)[[1]])
if (ix>-1) {
  sname=substring(name,ix)
  sname=sub("/","..",sname)
}
results=data.frame(rep(sname,nb),index,to_onset,to_len,tb_min,to_actv,ar_onset,ar_len,tb_max,ar_actv)
names(results)[1]="file"

mto=mean(results$to_len, na.rm=T)
mar=mean(results$ar_len,na.rm=T)
mtomin=mean(results$tb_min,na.rm=T)
mtomax=mean(results$tb_max,na.rm=T)
sto=sum(results$to_len,na.rm=T)
sar=sum(results$ar_len,na.rm=T)
mto=round(mto,2)
mar=round(mar,2)
mtomin=round (mtomin,2)
mtomax=round (mtomax,2)
sto=round(sto,2)
sar=round(sar,2)
mtoactv=mean(results$to_actv,na.rm=T)
maractv=mean(results$ar_actv,na.rm=T)
mtoactv=round(mtoactv,2)
maractv=round(maractv,2)
res=rbind(results,c('Mean','--','--',mto,mtomin,mtoactv,'--',mar,mtomax,maractv))
res=rbind(res    ,c('Sum ','--','--',sto,'--','--','--',sar,'--','--'))
print (res)
return(res)
}
