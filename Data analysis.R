rm(list=ls())
library(lme4)
#############################################################
# Read bootstrap HCA data set
dat<-read.csv("Simulated Data.csv")
attach(dat)
ID<-ID
bmi<-ifelse(dat$bmi>=30,1,0)
Sex<-ifelse(dat$Sex=="M",1,0)
X_CMS<-data.frame(age,Sex,colo_asa,diabetes,bmi,closure)
y<-ifelse(colo_SSI=="Y",1,0)
dat_CMS<-data.frame(y,ID,X_CMS)

# Calculate surgical volume for each hospital
size<-c(table(ID))

# Fit GLMM to estimate random effects mean and standard deviation
fit_re<-glmer(y~age+Sex+colo_asa+diabetes+bmi+closure+(1|ID),data=dat_CMS,
              family=binomial)
alpha<-fixef(fit_re)
sigma_alpha<-as.numeric(sqrt(summary(fit_re)$varcor[[1]]))

# Fit GLM to estimate marginal model parameters
fit_fix<-glm(y~age+Sex+colo_asa+diabetes+bmi+closure,data=dat_CMS,
              family=binomial)
beta_hat<-coef(fit_fix)[-1]

# Calculate power, FPR, PPV, and NPV  
set.seed(1)
fmla<-"y~age+Sex+colo_asa+diabetes+bmi+closure"
out<-sim_ranking(1000,fmla,ID,X_CMS,beta_hat,alpha,sigma_alpha)
