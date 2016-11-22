#----------------------------loding library......
rm(list=ls())
library(R2OpenBUGS)
library(coda)

mywd<-c('C:\\Users\\user\\Desktop\\SolomonMScwork\\')
setwd(mywd)

# =====================================  Code I used for Data Prepration.....
Infant_data<-read.csv("C:\\Users\\user\\Desktop\\SolomonMScwork\\Data and code\\infant data1.csv",header=T)
head(Infant_data)
table(Infant_data$Residence)
table(Infant_data$Child.sex)

table(Infant_data$mother.educa,Infant_data$mother.educa.primary)
table(Infant_data$mother.educa)

Infant_data$mother.educa.primary <- ifelse(with(Infant_data, mother.educa==1),1 , 0)
Infant_data$mother.educa.Seconday <- ifelse(with(Infant_data, mother.educa==2),1 , 0)


Infant_data$Event_time <- ifelse(with(Infant_data, Status==0),NA , Infant_data$Time)
Infant_data$Cen_time <- ifelse(with(Infant_data, Status==1), 0, Infant_data$Time)
Infant_data$Init_time_for_censored <- ifelse(with(Infant_data, Status==1),NA , (Infant_data$Time)+1)

#=============================================================================================================
#================================    models formulation ==========================================================================
#================================    Univariate weibull regression ==========================================================================

  cat(" model{
        for (i in 1:N){
        t[i] ~ dweib(rho,lambda[i])I(tcen[i],)
        lambda[i] <- exp(beta0+b1*X1[i]+b2*X2[i]+b3*X3[i]+b4*X4[i])
        median[i] <- pow((log(2) /lambda[i]), 1/rho)
        log(surv[i]) <- -lambda[i]*pow(t[i],rho)
    }

   beta0 ~dnorm(0,0.0001)
   b1  ~ dnorm(0,0.0001)
   b2  ~ dnorm(0,0.0001)
   b3  ~ dnorm(0,0.0001)
   b4  ~ dnorm(0,0.0001)

   rho ~ dgamma(1.0,0.0001)
   }",file=paste(mywd,"Univweibul.txt",sep=''));

# ------------------------------------ Load data-----------------------------------
# ----------------------------------- data for winbugs
Univweibuldata<-list(t=Infant_data$Event_time, tcen=Infant_data$Cen_time, N=3156, X1=Infant_data$Child.sex,X2=Infant_data$Residence,X3=Infant_data$mother.educa.primary,X4=Infant_data$mother.educa.Seconday)


# =================================== parameters to save and intials=================================================================
Univweibulinit11<-list(t=Infant_data$Init_time_for_censored,rho=0.1,beta0=0.1,b1=0.1,b2=0.1,b3=0.1,b4=0.1)
Univweibulinit12<-list(t=Infant_data$Init_time_for_censored,rho=0.5,beta0=0.5,b1=0.5,b2=0.5,b3=0.5,b4=0.5)
Univweibulinit13<-list(t=Infant_data$Init_time_for_censored,rho=0.5,beta0=0,b1=0,b2=0,b3=0,b4=0)

Univweibulinit<-list(Univweibulinit11,Univweibulinit12,Univweibulinit13)

parameters.to.save.Univweibul<-c('beta0','rho','b1','b2','b3','b4')

# ======================================running the model===============================================================================

model.coda.Univweibul<-bugs(data= Univweibuldata, parameters.to.save=parameters.to.save.Univweibul,inits=Univweibulinit, model.file =paste(mywd,"Univweibul.txt",sep=''),debug=T , n.chains = 3,n.iter =1000, n.burnin = 500,n.thin=1,codaPkg=T,working.directory=paste(mywd,sep=''))
coda.out.Univweibul<-read.bugs(model.coda.Univweibul)
 
#================================    models formulation ==========================================================================
#================================    Shared fraility weibull regression ==========================================================================

  cat(" model{
        for (i in 1:N){
        t[i] ~ dweib(rho,lambda[i])I(tcen[i],)
        lambda[i] <- exp(beta0+b1*X1[i]+b2*X2[i]+v[id2[i]])
        median[i] <- pow((log(2) /lambda[i]), 1/rho)
        log(surv[i]) <- -lambda[i]*pow(t[i],rho)
    }
 for(k in 1:n){ 
    v[k]  ~ dnorm(0,tauv)
   }

   beta0 ~dnorm(0,0.0001)
   b1  ~ dnorm(0,0.0001)
   b2  ~ dnorm(0,0.0001)
   rho ~ dgamma(1.0,0.0001)
   tauv <- pow(sdv,-2)
   sdv ~ dunif(0,10)
   }",file=paste(mywd,"ShrFrailityWeibull.txt",sep=''));

# ------------------------------------ Load data-----------------------------------
id<-Infant_data$Region
id2<-rep(0,length(id));

for(i in 1:length(sort(unique(id)))){id2<-ifelse(id==sort(unique(id))[i],i,id2)}
id2  # cluster identification number
# ------------------------------------- Number of Observations ---------------------

N<-length(id2)
N
# ------------------------------------- Number of Clusters --------------------------
n<-length(sort(unique(id2)));
n

# ----------------------------------- data for winbugs

ShrFrailityWeibulldata<-list(t=Infant_data$Event_time, tcen=Infant_data$Cen_time, N=3156,n=11,id2=id2,X1=Infant_data$Child.sex,X2=Infant_data$Residence)


# =================================== parameters to save and intials=================================================================
v1<-c(rep(0,11))
ShrFrailityWeibullinit1<-list(v=v1,t=Infant_data$Init_time_for_censored, sdv=0.1,rho=0.1,beta0=0.1,b1=0.1,b2=0.1)
ShrFrailityWeibullinit<-list(ShrFrailityWeibullinit1)

parameters.to.save.ShrFrailityWeibull<-c('beta0','rho','sdv','b1','b2')

# ======================================running the model===============================================================================

model.coda.ShrFrailityWeibull<-bugs(data= ShrFrailityWeibulldata, parameters.to.save=parameters.to.save.ShrFrailityWeibull,inits=ShrFrailityWeibullinit, model.file =paste(mywd,"ShrFrailityWeibull.txt",sep=''),debug=T , n.chains = 1,n.iter =1000, n.burnin = 500,n.thin=1,codaPkg=T,working.directory=paste(mywd,sep=''))
coda.out.ShrFrailityWeibull<-read.bugs(model.coda.ShrFrailityWeibull)

modeldiag<-coda.out.ShrFrailityWeibull
#---------------------------------------------------------------------
#                      autocorrelation and cusum plots
#---------------------------------------------------------------------
modeldiag<-coda.out.ShrFrailityWeibull

par(mfrow=c(1,3))
autocorr.plot(modeldiag[,"deviance"], lag.max=60, auto.layout = FALSE,
              lwd=2,cex.lab=1.5,main="LOGPOS",cex.main=1.8,col="red")

autocorr.plot(modeldiag[,"b1"], lag.max=150, auto.layout = FALSE,
              lwd=2,cex.lab=1.5,main=expression(beta[1]),cex.main=1.8,col="red")

#----------------------------------------------------------------------
#             density plots
#----------------------------------------------------------------------
densplot(modeldiag[,"deviance"],show.obs=TRUE,lwd=3,main=expression(Dev),
         col="red")

densplot(modeldiag[,"b1"],show.obs=TRUE,lwd=3,main=expression(beta[1]),
         col="red")

#---------------------------------------------------------------------
#                     GELMAN RUBIN DIAGNOSTIC
#---------------------------------------------------------------------


gelman.diag(modeldiag, confidence = 0.95, transform=FALSE, 
            autoburnin=TRUE)
par(mfrow=c(1,3))
gelman.plot(modeldiag[,"b1"], bin.width = 10, max.bins = 50,
confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = F, main=expression(beta[1]))


#========================================= Extracting summaries===========================================================================
coda.outP1<-modeldiag
mynamesModelP1<- ((row.names(data.frame(summary(coda.outP1)[1]))))
outputmatModelP1<-as.data.frame(cbind(mynamesModelP1,data.frame(summary(coda.outP1)[1])) )
outputmat.confModelP1<-as.data.frame(cbind(mynamesModelP1,data.frame(summary(coda.outP1)[2])) )
parestsaveModelP1<-data.frame(cbind(outputmatModelP1[,1],round(outputmatModelP1[,c(2:3,5)],4),round(outputmat.confModelP1[,c(2,6)],4)))
names(parestsaveModelP1)<-c('Parameter','Estimate','SD',"MC error",'Lower cl','Upper cl')
xtable(parestsaveModelP1)
head(parestsaveModelP1)
tail(parestsaveModelP1)

#================================    models formulation ==========================================================================

#================================    Spitial fraility weibull regression ==========================================================================

  cat(" model{
        for (i in 1:N){
        t[i] ~ dweib(rho,lambda[i])I(tcen[i],)
        lambda[i] <- exp(beta0+b1*X1[i]+b2*X2[i]+W[Region[i]])

        median[i] <- pow((log(2) /lambda[i]), 1/rho)
       #log(surv[i]) <- -lambda[i]*pow(t[i],rho)
    }

for (j in 1:nsum) {weights[j] <- 1}
#W[1:11] ~ car.normal(adj[],weights[],num[],tau)
W[1:n] ~ car.l1(adj[], weights[], num[], tau)
W.mean <- mean(W[])
      rho ~ dgamma(0.1,0.01)	
	tau  ~ dgamma(0.1,0.01) # prior on precision
	sigma <- 1 / sqrt(tau)	# standard deviation			
      beta0 ~  dflat()    
      b1  ~ dflat() 
      b2  ~ dflat() 
   }",file=paste(mywd,"SpiWeibull.txt",sep=''));

# ------------------------------------ Load data-----------------------------------
id<-Infant_data$Region
id2<-rep(0,length(id));

for(i in 1:length(sort(unique(id)))){id2<-ifelse(id==sort(unique(id))[i],i,id2)}
id2  # cluster identification number
sort(unique(id2))
# ------------------------------------- Number of Observations ---------------------

N<-length(id2)
N
# ------------------------------------- Number of Clusters --------------------------
n<-length(sort(unique(id2)));
n
Infant_data$Child.sex1 <- ifelse(with(Infant_data, Child.sex==1),1 , 0)
Infant_data$Residence1 <- ifelse(with(Infant_data, Residence ==1),1 , 0)
head(Infant_data)
# ----------------------------------- data for winbugs

SpiWeibulldata<-list( N=3156,n=11,id2=id2,X1=Infant_data$Child.sex,X2=Infant_data$Residence,
SpiWeibulldata<-list(N=3156,n=n,Region=Infant_data$Region,
adj=c(1,2,3,
2,1,3,4,5,
3,1,2,4,6,
4,2,3,5,6,7,8,9,10,
5,2,4,11,
6,3,4,8,
7,4,8,
8,4,7,
9,4,
10,4,
11,4,5), num = c(3,5,5,9,4,4,3,3,2,2,3)
,nsum=43,t=Infant_data$Event_time, tcen=Infant_data$Cen_time,X1=Infant_data$Child.sex1
,X2=Infant_data$Residence1)


W1<-c(rep(0,11))

SpiWeibullinit1<-list(W=W1,t=Infant_data$Init_time_for_censored,rho=0.1,beta0=0,b1=0.1,b2=0.1,tau=0.1)
SpiWeibullinit1<-list(W=c(5,5,5,5,5,5,5,5,5,5,5),t=Infant_data$Init_time_for_censored,rho=0.1,beta0=0.1,b1=0.1,b2=0.1,tau=0.1)
SpiWeibullinit1<-list(t=Infant_data$Init_time_for_censored,rho=0.1,beta0=0.1,b1=0.1,b2=0.1,tau=0.1)

SpiWeibullinit<-list(SpiWeibullinit1)

parameters.to.save.SpiWeibull<-c('beta0','rho','b1','b2','sigma')

# ======================================running the model===============================================================================

model.coda.SpiWeibull<-bugs(data= SpiWeibulldata, parameters.to.save=parameters.to.save.SpiWeibull,inits=SpiWeibullinit, model.file =paste(mywd,"SpiWeibull.txt",sep=''),debug=T , n.chains = 1,n.iter =2000, n.burnin = 500,n.thin=1,codaPkg=T,working.directory=paste(mywd,sep=''))
coda.out.SpiWeibull<-read.bugs(model.coda.SpiWeibull)





