sim_ranking<-function(K=1000,fmla,ID,X,beta_hat,alpha,sigma_alpha){
  # Function for estimate power, FPR, PPV, and NPV
  # Takes the following arguments:
  # K: Number of simulations
  # fmla: Regression formula of the marginal model
  # ID: Hospital indicators
  # X: Covariates for risk adjustment 
  # beta_hat: Estimated regression parameters
  # alpha: Random effects mean
  # sigma_alpha: Random effects standard deviation
  
  N<-length(ID);m<-length(unique(ID))
  T_rank<-c();E_rank<-c()
  for (i in 1:K) {
    # Generate true ranks
    alphai<-rnorm(m,alpha,sigma_alpha)
    TR1<-ifelse(alphai>quantile(alphai,0.75),1,0)
    T_rank<-rbind(T_rank,TR1)
    
    # Simulate outcome y
    lr<-c();h<-1
    for (j in unique(ID)) {
      lrj<-alphai[h]+as.matrix(X)[ID==j,]%*%beta_hat
      lr<-c(lr,lrj)
      h<-h+1
    }
    p<-exp(lr)/(1+exp(lr))
    y<-rbinom(N,1,p)
    dats<-data.frame(y,ID,X)
    
    # Calculate SIRs and estimated ranks
    fit<-glm(fmla,data=dats,family=binomial)
    SIR<-c()
    for (i in unique(ID)) {
      dati<-dats[which(dats$ID==i),]
      O<-sum(dati$y)
      E<-sum(predict(fit,newdata=dati,type="response"))
      SIR<-c(SIR,O/E)
    }
    sir3<-quantile(SIR,0.75)
    PR<-ifelse(SIR>sir3,1,0)
    E_rank<-rbind(E_rank,PR)
  }
  
  # Calculate power, FPR, PPV, and NPV
  power<-c();FPR<-c();PPV<-c();NPV<-c()
  for (i in unique(ID)) {
    simi<-cbind(T_rank[,i],E_rank[,i])
    TP<-sum(ifelse(simi[,1]==1 & simi[,2]==1,1,0))
    FP<-sum(ifelse(simi[,1]==0 & simi[,2]==1,1,0))
    TN<-sum(ifelse(simi[,1]==0 & simi[,2]==0,1,0))
    FN<-sum(ifelse(simi[,1]==1 & simi[,2]==0,1,0))
    power<-c(power,TP/(TP+FN));FPR<-c(FPR,FP/(FP+TN))
    PPV<-c(PPV,TP/(TP+FP));NPV<-c(NPV,TN/(FN+TN))
  }
  cbind(unique(ID),power,FPR,PPV,NPV)
}
