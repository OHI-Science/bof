setwd("C:\\Users\\Sylvie\\Documents\\BOFEP")
filename <- "StockScores.csv"
outfile  <- "Indices.csv"

cdat=data.frame(32,20,0)
xscores=data.frame(1,10,0)
cdat <- read.csv(filename, header=T)
#slist <- unique(as.character(cdat$stock)) 
nr=nrow(cdat)

alpha=0.5
beta=0.25
thres= 1.5  #T
lthres=0.95 #(LT)
totC=sum(cdat$meanC)


SSFmsy<- function(FFmsy, BBmsy)
   #decision rules from Halpern et al 2014 but the maximum has been raise to 3.1 from 2.5 to account for tha range in the region
{
  if (BBmsy<0.8){
    Fpi<- ifelse(FFmsy>(BBmsy+1.5),0,ifelse(FFmsy<(BBmsy-0.2),FFmsy/(BBmsy-0.2),ifelse(((BBmsy+0.2)<FFmsy) & (FFmsy<(BBmsy+1.5)),(BBmsy+1.5-FFmsy)/1.5,ifelse((BBmsy-0.2<FFmsy) & (FFmsy<BBmsy+0.2),1,NA))))
  } else {
    Fpi <- ifelse(FFmsy<0.8, FFmsy/0.8,ifelse(0.8<=FFmsy & FFmsy<1.2,1,ifelse(FFmsy>=1.2,(3.1-FFmsy)/1.3,NA)))
  }
  
  return (Fpi)
}

## initial scores  
for (i in 1:nr){
  a<-0
  ah<-0
  al<-0
  if (!is.na(cdat$iBB1[i])){
      cdat$SS1[i] <- ifelse(cdat$iBB1[i]>=lthres,(ifelse(cdat$iBB1[i]<=thres,1,max(1-alpha*(cdat$iBB1[i]-thres),beta))),cdat$iBB1[i])
      if (!is.na(cdat$iFF1[i])){
        cdat$Fp[i]<-  SSFmsy(FFmsy=cdat$iFF1[i],BBmsy=cdat$iBB1[i])
        cdat$SS2[i]<- mean(c(cdat$SS1[i], cdat$Fp[i]))
      } else {
        cdat$SS2[i]=cdat$Fp[i]<- NA
      }
      
      } else {
      if (!is.na(cdat$iBB2[i])){
        cdat$SS1[i] <- ifelse(cdat$iBB2[i]>=lthres,(ifelse(cdat$iBB2[i]<=thres,1,max(1-alpha*(cdat$iBB2[i]-thres),beta))),cdat$iBB2[i])
        cdat$SS2[i]=cdat$Fp[i]<- NA
      } else {
      cdat$SS1[i] <- ifelse(cdat$iBB3[i]>=lthres,(ifelse(cdat$iBB3[i]<=thres,1,max(1-alpha*(cdat$iBB3[i]-thres),beta))),cdat$iBB3[i])
      cdat$SS2[i]=cdat$Fp[i]<- NA
    }
  }
 # (0.58)[cdat$stock[i]=='tuna']->cdat$SS1[i]
  if(cdat$stock[i]=='tuna'){
    cdat$SS1[i]<-0.58
  } 
 a<- c(cdat$iBB1[i],cdat$iBB2[i],cdat$iBB3[i])
 ah<- max(a, na.rm=T)
 al<- min(a, na.rm=T)
 cdat$SSH[i] <- max(ifelse(ah>=lthres,(ifelse(ah<=thres,1,max(1-alpha*(ah-thres),beta))),ah),cdat$SS2[i], na.rm=T)
 cdat$SSL[i] <- min(ifelse(al>=lthres,(ifelse(al<=thres,1,max(1-alpha*(al-thres),beta))),al),cdat$SS2[i], na.rm=T)
 

} #end i
windows(record=T)
hist(cdat$SS1,breaks=seq(0,1,0.2), main='', xlab='Stock Score (SS1)')

xscores$X_SS1=sum(cdat$SS1*cdat$meanC/totC)
for (i in 1:nr) {cdat$SSe[i] <- ifelse (!is.na(cdat$SS2[i]),cdat$SS2[i],cdat$SS1[i])}
xscores$X_SS2=sum(cdat$SSe*cdat$meanC/totC)
xscores$X_SSH=sum(cdat$SSH*cdat$meanC/totC)
xscores$X_SSL=sum(cdat$SSL*cdat$meanC/totC)

hdat<- subset(cdat,cdat$stock!="herring")
xscores$woherr=sum(hdat$SS1*hdat$meanC/sum(hdat$meanC))
hdat<- subset(cdat,hdat$stock!="cod")
xscores$wocod=sum(hdat$SS1*hdat$meanC/sum(hdat$meanC))
hdat<- subset(cdat,hdat$stock!="haddock")
xscores$wohadd=sum(hdat$SS1*hdat$meanC/sum(hdat$meanC))
hdat<- subset(cdat,hdat$stock!="pollock")
xscores$wopoll=sum(hdat$SS1*hdat$meanC/sum(hdat$meanC))


n=length(xscores)  # n cells in xscores
nn=ncol(cdat)
scen_lthres=c(0.95, 0.95,0.95,0.95,0.85)
scen_thres=c(1.5,1.05,1.8,1.5,1.5)
scen_alpha=c(0.5, 0.5, 0.5, 0.2, 0.5)
jj=length(scen_alpha)
for (j in 1:jj){
  lthresh=scen_lthres[j]
  thresh=scen_thres[j]
  alpha=scen_alpha[j]
  for (i in 1:nr){
    if (!is.na(cdat$iBB1[i])){
      cdat[i,nn+j] <- ifelse(cdat$iBB1[i]>=lthres,(ifelse(cdat$iBB1[i]<=thres,1,max(1-alpha*(cdat$iBB1[i]-thres),beta))),cdat$iBB1[i])
      
    } else {
      if (!is.na(cdat$iBB2[i])){
        cdat[i,nn+j] <- ifelse(cdat$iBB2[i]>=lthres,(ifelse(cdat$iBB2[i]<=thres,1,max(1-alpha*(cdat$iBB2[i]-thres),beta))),cdat$iBB2[i])
      } else {
        cdat[i,nn+j] <- ifelse(cdat$iBB3[i]>=lthres,(ifelse(cdat$iBB3[i]<=thres,1,max(1-alpha*(cdat$iBB3[i]-thres),beta))),cdat$iBB3[i])
      }
    }
    # (0.58)[cdat$stock[i]=='tuna']->cdat$SS1[i]
    if(cdat$stock[i]=='tuna'){
      cdat$SS1[i]<-0.58
    } 
    
    xscores[n+j]=sum(cdat[,nn+j]*cdat$meanC/totC)
  } #end i
} # end of j

herss<- c(0.1, 0.4, 0.6)
hdat<-cdat
n=length(xscores)  # n cells in xscores
for (i in 1:3){
  hdat$SS1[which(hdat$stock=='herring')]<-herss[i]
  xscores[n+i]=sum(hdat$SS1*hdat$meanC/sum(hdat$meanC))
}

write.csv(xscores,file= "scores results.csv")
write.csv(cdat,file= "calculations.csv")         
