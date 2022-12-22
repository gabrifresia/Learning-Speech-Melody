#Packages
library(glmmTMB)
library(MASS)
library(car)
library(pROC)
library(multcomp)
library(ggplot2)

#Read the data, but omit the unaltered prototypes to keep the data balanced
data=read.csv('preprocessed_data.csv', header=T)
data$Intensity=factor(data$Intensity)
data$answer=relevel(factor(data$answer),'Others')
data=data[data$Intensity!=0,]

############################
#                          #
#         Inference        #
#                          #
############################

#Fit the full model, but considering that the first research questions didn't 
#include some of the interactions
model_full=glmmTMB(Correct~answer+Intensity+LocationDychotomous+Nback.MemoryScore+
                     Learning.Ability+(answer|Participant.Private.ID), data=data, 
                 family=binomial(link='logit'))

#Test for significance of fixed effects with Anova(), which performs a Wald's test
Anova(model_full)
summary(model_full)

#Check if a random effect on answer is significant with BIC and AIC
model_noRandomAnsw=glmmTMB(Correct~answer+Intensity*LocationDychotomous+Nback.MemoryScore+Learning.Ability
                    +(1|Participant.Private.ID), data=data, 
                    family=binomial(link='logit'))
summary_noRandomAnsw=summary(model_noRandomAnsw)


#Using random intercepts for answer really seems to help reduce the BIC and AIC 
#so the chosen model is:
model=model_full
#Whose p-values, once again, are:
summary(model)


############################
#                          #
#Compute coefficient impact#
#                          #
############################
#Make a dataframe with the confidence intervals
ConfInts_raw=confint(object=model)
ConfInts=as.data.frame(exp(ConfInts_raw))
ConfInts$Var=rownames(ConfInts_raw)
ConfInts[2:9,]$Var=c('Melody Biff vs. Others','Melody Mup vs. Others','Melody Wob vs. Others',
                     'Intensity 3ST vs 1ST','Intensity 5ST vs 1ST','Location: End',
                     'Memory Score (10%)','Learning Ability (10%)')
names(ConfInts)=c('Min','Max','Estimate','Var')
rownames(ConfInts)=1:19

# ggplot2 plot with confidence intervals
ggplot(ConfInts[2:9,],aes(Estimate,Var)) +        
  geom_point() +
  geom_errorbar(aes(xmin = Min, xmax = Max)) + xlim(0,13) +
  geom_vline(xintercept=1, linetype="dashed",col='red',size=0.7) +
  xlab('Confidence Interval: effect on odds of correctly identifying the pattern') + ylab('Predictor')




############################
#                          #
#  Check Model Assumptions #
#                          #
############################

#Check the Deviance for overdispersion
print(summary(model)$AICtab)
#The ratio of deviance and approx. df is close to 1, which is good:
summary(model)$AICtab[4]/summary(model)$AICtab[5]

#Check normality of errors and coefficients
#errors
hist(coef(model)$cond$Participant.Private.ID$'(Intercept)',50,probability = T)

qqnorm(coef(model)$cond$Participant.Private.ID$'(Intercept)')
qqline(coef(model)$cond$Participant.Private.ID$'(Intercept)', col = "steelblue", lwd = 2)

#coefficients
hist(coef(model)$cond$Participant.Private.ID$answerBiff,50,probability = T)

qqnorm(coef(model)$cond$Participant.Private.ID$answerBiff)
qqline(coef(model)$cond$Participant.Private.ID$answerBiff, col = "steelblue", lwd = 2)

hist(coef(model)$cond$Participant.Private.ID$answerMup,50,probability = T)

qqnorm(coef(model)$cond$Participant.Private.ID$answerMup)
qqline(coef(model)$cond$Participant.Private.ID$answerMup, col = "steelblue", lwd = 2)

hist(coef(model)$cond$Participant.Private.ID$answerWob,50,probability = T)

qqnorm(coef(model)$cond$Participant.Private.ID$answerWob)
qqline(coef(model)$cond$Participant.Private.ID$answerWob, col = "steelblue", lwd = 2)


############################
#                          #
#    Goodness of Fit       #
#                          #
############################

#Use cross-validation on the participants
Participants=sample(unique(data$Participant.Private.ID))
n=length(Participants)
folds=rep_len(1:10,length.out=n)
performance=rep(NA,10)
brier_score=rep(NA,10)
AUC=rep(NA,10)
conf_matrix=matrix(0,2,2)
for(k in 1:10){
  #Train model
  IDs=Participants[folds==k]
  test_indices=data$Participant.Private.ID %in% IDs
  data_train=data[-test_indices,]
  model_folded=glmmTMB(Correct~answer+Intensity*LocationDychotomous+Nback.MemoryScore+Learning.Ability
                    +(answer|Participant.Private.ID), data=data_train, 
                    family=binomial(link='logit'))
  #Validate model
  true_class=data[test_indices,'Correct']
  base_rate=mean(true_class)
  p_hat=predict(model_folded,data[test_indices,], type = 'response')
  y_hat=(p_hat>=base_rate)*1
  
  performance[k]=mean(y_hat == true_class)
  brier_score[k]=mean((true_class-p_hat)^2)
  AUC[k]=roc(true_class,p_hat,levels = c(0,1), direction = "<")$auc
  conf_matrix=conf_matrix+table(true_class,y_hat)
}

#Show goodness of fit measures:
print(paste('Overall performance:',mean(performance)))
print(paste('Brier score:',mean(brier_score)))
print(paste('Area under the curve:',mean(AUC)))
print('Average Confusion matrix:')
print(conf_matrix/10)
