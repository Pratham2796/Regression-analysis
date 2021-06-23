### Final Project ###
### Group 3 ###
### Dec 14, 2020 ###

#### clear data, setup ####
rm(list = ls())
par(mfrow=c(1,1))
shell("cls")

#### add libraries ####
library(leaps); library(HH); library(StepReg); library(olsrr);
library(EnvStats); library(car); library(corrplot); library(lmtest)

##### import dataset ####
loans <- read.csv("~/MT Y5/MA 4710/Final project/final-project-dataset.csv")
colnames(loans) <- c("y","x1","x2","x3","x4","x5","x6","x7")

##### categorical predictor conversion ####
# convert x6 and x7 to binary columns
{{loans$x6 = factor(loans$x6, ordered=FALSE); x6.mat=model.matrix(~loans$x6-1);   
c6.1 = x6.mat[,1]; c6.2 = x6.mat[,2]; c6.3 = x6.mat[,3]}
  {loans$x7 = factor(loans$x7, ordered=FALSE);  
    x7.mat=model.matrix(~loans$x7-1); c7.1=x7.mat[,1] ; c7.2=x7.mat[,2]}}

# adding class representation columns to dataset
{loans=cbind(loans,c6.1); loans=cbind(loans,c6.2); loans=cbind(loans,c6.3);
  loans = cbind(loans,c7.1); loans = cbind(loans,c7.2)}



##### transform, center, standardize the numerical predictors #####
loansNm=loans[,c(1,2,3,4,5,6)] #view only numericals

cor(loansNm); #view cor before edits
pairs (loansNm) #scatter plot matrix before edits

#### center the predictor variables ####
{loans$x1 <- (loans$x1 - mean(loans$x1))
loans$x2 <- (loans$x2 - mean(loans$x2))
loans$x3 <- (loans$x3 - mean(loans$x3))
loans$x4 <- (loans$x4 - mean(loans$x4))
loans$x5 <- (loans$x5 - mean(loans$x5))}

loansNm=loans[,c(1,2,3,4,5,6)] #view only numericals

cor(loansNm); #view cor after centering

#### scale the predictor variables ####
{loans$x1 <- loans$x1/sd(loans$x1)
loans$x2 <- loans$x2/sd(loans$x2)
loans$x3 <- loans$x3/sd(loans$x3)
loans$x4 <- loans$x4/sd(loans$x4)
loans$x5 <- loans$x5/sd(loans$x5)}

loansNm=loans[,c(1,2,3,4,5,6)] #view only numericals

cor(loansNm); #view cor after centering

#### adding interaction terms to the dataset ####
{n <- nrow(loans)
output = matrix(, nrow = n, ncol = 25) #empty matrix
xn=loans[,c(2,3,4,5,6)]
xc=loans[,c(9,10,11,12,13)]
colnames(output)=c(
  "x1:c6.1","x1:c6.2","x1:c6.3","x1:c7.1","x1:c7.2",
  "x2:c6.1","x2:c6.2","x2:c6.3","x2:c7.1","x2:c7.2",
  "x3:c6.1","x3:c6.2","x3:c6.3","x3:c7.1","x3:c7.2",
  "x4:c6.1","x4:c6.2","x4:c6.3","x4:c7.1","x4:c7.2",
  "x5:c6.1","x5:c6.2","x5:c6.3","x5:c7.1","x5:c7.2"
)}
{q=1 #counter for placement
  for(i in 1:5){
    for(j in 1:5){
      output[,q]= xn[,i]*xc[,j]
      loans=cbind(loans,output[,q])
      q=q+1;
    }
  }}
colnames(loans)=c("y","x1","x2","x3","x4","x5","x6","x7",
                  "c6.1","c6.2","c6.3","c7.1","c7.2",
                  "x1..c6.1","x1..c6.2","x1..c6.3","x1..c7.1","x1..c7.2",
                  "x2..c6.1","x2..c6.2","x2..c6.3","x2..c7.1","x2..c7.2",
                  "x3..c6.1","x3..c6.2","x3..c6.3","x3..c7.1","x3..c7.2",
                  "x4..c6.1","x4..c6.2","x4..c6.3","x4..c7.1","x4..c7.2",
                  "x5..c6.1","x5..c6.2","x5..c6.3","x5..c7.1","x5..c7.2")

loansNm=loans[,c(1,2,3,4,5,6)] #view only numericals
loans.first.order=loans[,-c(7,8)] #exclude original categoricals 

pairs(loansNm); cor(loansNm) #outputs after edits

#### adding square terms to the dataset ####
{x1sq=loans$x1^2; x2sq=loans$x2^2; x3sq=loans$x3^2; 
x4sq=loans$x4^2; x5sq=loans$x5^2;}

loansNm2=cbind(loansNm,x1sq,x2sq,x3sq,x4sq,x5sq)
loans=cbind(loans,x1sq,x2sq,x3sq,x4sq,x5sq) #loans contains all information
loans.second.order=cbind(loansNm,x1sq,x2sq,x3sq,x4sq,x5sq,c6.1,c6.2,c6.3,c7.1,c7.2,output)

pairs(loansNm2); cor(loansNm2) #outputs w/ square terms

##Correlation Plot##
{a <- cor(loansNm2)
  corrplot(a)} ##correlation plot


#### histograms to visualize numerical data ####
{par(mfrow=c(2,2)); hist(loans$x1,breaks=15); hist(loans$x2,breaks=30); 
  hist(loans$x3,breaks=15);   hist(loans$x4,breaks=15);} #first four histograms
hist(loans$x5,breaks=15); hist(loans$y,breaks=15) #last two



#### regression fit (Simple) ####
simple.fit = lm(y ~ x1+x2+x3+x4+x5+c6.1+c6.2+c6.3+c7.1+c7.2, data=loans)
summary(simple.fit)

#### added variable plots to analyze residuals ####
avPlots(simple.fit)


###### fitting first order regression function w/ all interaction terms ######
summary(lm(y ~ (x1+x2+x3+x4+x5)*(c6.1+c6.2+c6.3+c7.1+c7.2)
           , data=loans))



######################### Include interaction? ###############################

######################### Interaction #################################

int.lmfit <- lm(y~(x1+x2+x3+x4+x5)*(c6.1+c6.2+c6.3+c7.1+c7.2), data=loans)
# full.lmfit
# anova(full.lmfit)

######################### no Interaction ##############################

noInt.lmfit <- lm(y~(x1+x2+x3+x4+x5)+(c6.1+c6.2+c6.3+c7.1+c7.2), data=loans)
# reduced.lmfit
# anova(reduced.lmfit)

####################### Lack-of-Fit Test ######################

anova(noInt.lmfit,int.lmfit)


######################### Include second order? ###############################

######################### Second order w/ interaction #########################

so.lmfit <- lm(y~(x1+x2+x3+x4+x5)*(c6.1+c6.2+c6.3+c7.1+c7.2)+
                 x1sq+x2sq+x3sq+x4sq+x5sq, data=loans)

######################### First order w/interaction ##########################

fo.lmfit <- lm(y~(x1+x2+x3+x4+x5)*(c6.1+c6.2+c6.3+c7.1+c7.2), data=loans)

####################### Lack-of-Fit Test ######################

anova(fo.lmfit,so.lmfit)
#p-val less than 0.05


######################### Include second order? ###############################

######################### Second order w/ int #################################

soWi.lmfit <- lm(y~(x1+x2+x3+x4+x5)*(c6.1+c6.2+c6.3+c7.1+c7.2)+
                   x1sq+x2sq+x3sq+x4sq+x5sq, data=loans)

######################### Second order w/o int ##############################

soWoi.lmfit <- lm(y~(x1+x2+x3+x4+x5)+(c6.1+c6.2+c6.3+c7.1+c7.2)+
                    x1sq+x2sq+x3sq+x4sq+x5sq, data=loans)

####################### Lack-of-Fit Test ######################

anova(soWoi.lmfit,soWi.lmfit)
#p-val less than 



######################### MODEL SELECTION ################################

###### stepwise selection methods comparison ######
# including second order and interaction:
stepwise(data=loans.second.order,y="y",select="adjRsq")
stepwise(data=loans.second.order,y="y",select="CP")
stepwise(data=loans.second.order,y="y",select="AIC")
stepwise(data=loans.second.order,y="y",select="BIC")

summary(lm(y ~ x5sq + x5 + x5..c7.1 + c7.1 + x5..c6.1 + 
             x4..c6.2 + c6.1 + x4..c7.2 + c7.2 + x1..c6.1 + x3..c6.1 +
             x2..c6.2 + x4sq + x1..c6.3 + c6.2 + x1..c7.1 + x1
           ,data=loans)) # output model from adjRsq

summary(lm(y ~ x5sq + x5 + x5..c7.1 + c7.1 + x5..c6.1 + 
             x4..c6.2 + c6.1 + x4..c7.2 + c7.2 + x1..c6.1 + x3..c6.1 +
             x2..c6.2 + x4sq + x1..c6.3
           ,data=loans)) # output model from CP

summary(lm(y ~ x5sq + x5 + x5:c7.1 + c7.1 + x5:c6.1 + 
             x4:c6.2 + c6.1 + x4:c7.2 + c7.2 + x1:c6.1 + x3:c6.1 +
             x2:c6.2 + x4sq + x1:c6.3
           ,data=loans)) # output model from AIC (same as CP)

summary(lm(y ~ x5sq + x5 + x5:c7.1 + c7.1 + x5:c6.1 + 
             x4:c6.2 + c6.1 + x4:c7.2 + c7.2 + x1:c6.1 + x3:c6.1 +
             x2:c6.2 + x4sq + x1:c6.3
           ,data=loans)) # output model from BIC (same as CP)

# *** the model from CP method may be best. every term is significant. 
#      ^ it has the same model as BIC and AIC.
# adjRsq has 3 more terms, but it has 5 insignificant terms w/ alpha = 0.05.


######################### ENTER CHOSEN MODEL ###########################

reg.loans.chosen=lm(y ~ x5sq + x5 + x5..c7.1 + c7.1 + x5..c6.1 + 
                      x4..c6.2 + c6.1 + x4..c7.2 + c7.2 + x1..c6.1 + x3..c6.1 +
                      x2..c6.2 + x4sq + x1..c6.3
                    ,data=loans)
# ^enter the chosen model, best from previous results









####################### Check the linearity ###########################
### Lack of Fit Test ###

full.lmfit <- lm(y ~ factor(x5sq) + factor(x5) + factor(x5*c7.1) +
                   factor(c7.1) + factor(x5*c6.1) +factor(x4*c6.2) +
                   factor(c6.1) + factor(x4*c7.2) + factor(c7.2) +
                   factor(x1*c6.1) + factor(x3*c6.1) + factor(x2*c6.2) +
                   factor(x4sq) + factor(x1*c6.3), data=loans)

reduced.lmfit <- reg.loans.chosen

anova(reduced.lmfit, full.lmfit)
#if p-val is greater than 0.05, the model satisfies linearity

#### residual plot setup ####
# get residuals and fitted values #
res <- rstudent(reg.loans.chosen); fit.Y <- fitted(reg.loans.chosen)

#### Residual Plots ####

{par(mfrow=c(2,2))
  plot(res ~ fit.Y, xlab="Fitted Values (Y.hat)",ylab="Residuals", 
       main="Residuals vs. Fitted Y"); abline(h=0, lwd=2,col="red")
  #res vs x_i
  plot(res ~ loans$x5sq, xlab="x5sq", ylab="Residuals", main="Residuals vs. x5^2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5, xlab="x5", ylab="Residuals", main="Residuals vs. x5")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5..c7.1, xlab="x5*c7.1", ylab="Residuals", main="Residuals vs. x5*c7.1")
  abline(h=0, lwd=2,col="red") 
}#end of first 4 res plots
{plot(res ~ loans$c7.1, xlab="c7.1", ylab="Residuals", main="Residuals vs. c7.1")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5..c6.1, xlab="x5*c6.1", ylab="Residuals",
       main="Residuals vs. x5*c6.1");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x4..c6.2, xlab="x4*c6.2", ylab="Residuals",
       main="Residuals vs. x4*c6.2");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$c6.1, xlab="c6.1", ylab="Residuals", 
       main="Residuals vs. c6.1");    
  abline(h=0, lwd=2,col="red")
}#end of second four
{plot(res ~ loans$x4..c7.2, xlab="x4*c7.2", ylab="Residuals", main="Residuals vs. x4*c7.2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$c7.2, xlab="c7.2", ylab="Residuals", 
       main="Residuals vs. c7.2");    
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x1..c6.1, xlab="x1*c6.1", ylab="Residuals",
       main="Residuals vs. x1*c6.1");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x3..c6.1, xlab="x3*c6.1", ylab="Residuals",
       main="Residuals vs. x3*c6.1");
  abline(h=0, lwd=2,col="red")
}#end of third four
{plot(res ~ loans$x2..c6.2, xlab="x2*c6.2", ylab="Residuals", main="Residuals vs. x2*c6.2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x4sq, xlab="x4sq", ylab="Residuals", 
       main="Residuals vs. x4sq");    
  abline(h=0, lwd=2,col="red")
}#end of fourth set




####################### Check the normality ###########################
qqnorm(res); qqline(res); shapiro.test(res); hist(res,breaks=20)
# if p val is greater than 0.05, normality is true (SW test)




################## Homoscedacity (error variance) ######################
#res vs time series, detects nonindependance of error terms
{par(mfrow=c(1,1)); plot(res, xlab="Time",ylab="Residuals", main="Residuals vs. Time")
  abline(h=0, lwd=2,col="red")}

bptest(reg.loans.chosen)#for constant variance check

#refer back to residual plots again (vs fit.y and x_i)



####################### Influential Points ############################

ols_plot_cooksd_chart(reg.loans.chosen)
ols_plot_dffits(reg.loans.chosen)
ols_plot_dfbetas(reg.loans.chosen)


####################### Multicolinearity ##############################
vif(reg.loans.chosen)
# x var are not correlated if VIF values are near 1



################### Remove high leverage observations ########################

# loans.second.order=loans.second.order[-c(37,47,57,83,124,300,301,309,364,470,482,510,588,619,660,661,715),]
# loans=loans[-c(83,309,510,715),]

loans=loans[-c(57,83,124,139,186,300,301,309,326,365,387,407,445,446,452,
               471,482,510,535,571,586,604,619,660,661,704,715,708,719),]

loans.second.order=loans.second.order[-c(57,83,124,139,186,300,301,309,326,
                                         365,387,407,445,446,452,
                                         471,482,510,535,571,586,604,619,660,661,704,715,708,719),]

####################### Remove more points ##############################
loans=loans[-c(13,50,82,96,155,156,501,508,305,355,382,458,459,508,570,721),]
loans.second.order=loans.second.order[-c(13,50,82,96,155,156,501,508,305,355,
                                         382,458,459,508,570,721),]

loans=loans[-c(36,250,312,307,623,607,632,579),]
loans.second.order=loans.second.order[-c(36,250,312,307,623,607,632,579),]



#resave the fit function
reg.loans.chosen=lm(y ~ x5sq + x5 + x5..c7.1 + c7.1 + x5..c6.1 + 
                      x4..c6.2 + c6.1 + x4..c7.2 + c7.2 + x1..c6.1 
                    + x3..c6.1 + x2..c6.2 + x4sq + x1..c6.3
                    ,data=loans)


######################### Transformation #############################

{boxcox.summary = boxcox(reg.loans.chosen, optimize=TRUE)
lambda = boxcox.summary$lambda; 
trans.Y = loans$y^lambda;
loans = cbind(loans,trans.Y)
loans.second.order = cbind(loans,trans.Y)}



######## Re-fitting a model using the transformed response variable. ########

{boxcox.lmfit = lm(trans.Y ~ x5sq + x5 + x5..c7.1 + c7.1 + x5..c6.1 + 
                     x4..c6.2 + c6.1 + x4..c7.2 + c7.2 + x1..c6.1 
                   + x3..c6.1 + x2..c6.2 + x4sq + x1..c6.3, data=loans); 
boxcox.res <- rstudent(boxcox.lmfit)
boxcox.fitted.y <- fitted(boxcox.lmfit)}
summary(boxcox.lmfit)






#### then run assumption checks again



####################### Check the linearity ###########################
### lack of fit test ###
{full.lmfit.trans <- lm(trans.Y ~ factor(x5sq) + factor(x5) + factor(x5*c7.1) +
                          factor(c7.1) + factor(x5*c6.1) +factor(x4*c6.2) +
                          factor(c6.1) + factor(x4*c7.2) + factor(c7.2) +
                          factor(x1*c6.1) + factor(x3*c6.1) + factor(x2*c6.2) +
                          factor(x4sq) + factor(x1*c6.3), data=loans)

reduced.lmfit.trans <- boxcox.lmfit

anova(reduced.lmfit.trans, full.lmfit.trans)}
#if p-val is greater than 0.05, the model satisfies linearity

#### residual plot setup ####
# get residuals and fitted values #
res = rstudent(boxcox.lmfit); fit.Y = fitted(boxcox.lmfit)

#### Residual Plots ####
{par(mfrow=c(2,2))
  plot(res ~ fit.Y, xlab="Fitted Values (Y.hat)",ylab="Residuals", 
       main="Residuals vs. Fitted Y"); abline(h=0, lwd=2,col="red")
  #res vs x_i
  plot(res ~ loans$x5sq, xlab="x5sq", ylab="Residuals", main="Residuals vs. x5^2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5, xlab="x5", ylab="Residuals", main="Residuals vs. x5")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5..c7.1, xlab="x5*c7.1", ylab="Residuals", main="Residuals vs. x5*c7.1")
  abline(h=0, lwd=2,col="red") 
}#end of first 4 res plots
{plot(res ~ loans$c7.1, xlab="c7.1", ylab="Residuals", main="Residuals vs. c7.1")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x5..c6.1, xlab="x5*c6.1", ylab="Residuals",
       main="Residuals vs. x5*c6.1");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x4..c6.2, xlab="x4*c6.2", ylab="Residuals",
       main="Residuals vs. x4*c6.2");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$c6.1, xlab="c6.1", ylab="Residuals", 
       main="Residuals vs. c6.1");    
  abline(h=0, lwd=2,col="red")
}#end of second four
{plot(res ~ loans$x4..c7.2, xlab="x4*c7.2", ylab="Residuals", main="Residuals vs. x4*c7.2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$c7.2, xlab="c7.2", ylab="Residuals", 
       main="Residuals vs. c7.2");    
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x1..c6.1, xlab="x1*c6.1", ylab="Residuals",
       main="Residuals vs. x1*c6.1");
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x3..c6.1, xlab="x3*c6.1", ylab="Residuals",
       main="Residuals vs. x3*c6.1");
  abline(h=0, lwd=2,col="red")
}#end of third four
{plot(res ~ loans$x2..c6.2, xlab="x2*c6.2", ylab="Residuals", main="Residuals vs. x2*c6.2")
  abline(h=0, lwd=2,col="red")
  plot(res ~ loans$x4sq, xlab="x4sq", ylab="Residuals", 
       main="Residuals vs. x4sq");    
  abline(h=0, lwd=2,col="red")
}#end of fourth set


####################### Check the normality ###########################
qqnorm(res); qqline(res); shapiro.test(res); hist(res,breaks=20)
# if p val is greater than 0.05, normality is true (SW test)


################## Homoscedacity (error variance) ######################
#res vs time series, detects nonindependance of error terms
{par(mfrow=c(1,1)); plot(res, xlab="Time",ylab="Residuals", main="Residuals vs. Time")
  abline(h=0, lwd=2,col="red")}

bptest(boxcox.lmfit) #for constant variance check

#refer back to residual plots again (vs fit.y and x_i)



####################### Influential Points ############################
ols_plot_cooksd_chart(reg.loans.chosen)
ols_plot_dffits(reg.loans.chosen)
ols_plot_dfbetas(reg.loans.chosen)



####################### Multicolinearity ##############################
vif(reg.loans.chosen)
# x var are not correlated if VIF values under 10


