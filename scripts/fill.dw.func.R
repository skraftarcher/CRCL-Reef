# function to fill missing dry weights
source("scripts/install_packages_function.R")
lp("tidyverse")


fill.dryweights<-function(dset,taxa,idcol){
  coln<-seq(1:ncol(dset))[colnames(dset)==idcol]
  t1<-dset[dset[,coln]==taxa,]
  t1.wd<-filter(t1,!is.na(dry.weight))
  t1.wd<-filter(t1.wd,voucher!="yes")
  t1.wd<-filter(t1.wd,bmass>0)
  t1.wod<-anti_join(t1,t1.wd)
  avg.bpi<-t1.wd$bmass/t1.wd$abundance
  if(nrow(t1.wd!=0) & nrow(t1.wod!=0)){
    for(i in 1:nrow(t1.wod)){
      if(is.na(t1.wod$dry.weight[i])|t1.wod$bmass[i]<0){
        dws<-sample(size=t1.wod$abundance[i],x=avg.bpi,replace=T)
        t1.wod$bmass[i]<-sum(dws)
      }
      if(!is.na(t1.wod$dry.weight[i])&t1.wod$voucher[i]=="yes"&t1.wod$abundance[i]>1){
       dws<-sample(size=t1.wod$abundance[i]-t1.wod$voucher.count[i],x=avg.bpi,replace=T)
       t1.wod$bmass[i]<-t1.wod$bmass[i]+sum(dws)
      }
      if(t1.wod$voucher[i]=="yes"&t1.wod$abundance[i]==1){
        t1.wod$bmass[i]<-sample(size=1,x=avg.bpi,replace=T)
      }
    }
  }
  rpl.data<-bind_rows(t1.wd,t1.wod)
  t2<-anti_join(dset,t1)
  return(bind_rows(t2,rpl.data))
}
