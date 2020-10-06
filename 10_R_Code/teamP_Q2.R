library(knitr)
library(ggplot2)
library(skimr)
library(xtable)
library(rms) #for VIF
library(MASS)
library(pander)
library(arm)
library(pROC)
library(e1071)
library(caret)
require(gridExtra)

################################################################################
######################## Exploratory data analysis #############################
################################################################################
wages <- read.delim('./lalondedata.txt', sep=',')
head(wages)
dim(wages)
summary(wages)

wages$re78_zero <- wages$re78
wages$re78_zero[wages$re78_zero == 0] <- 0
wages$re78_zero[wages$re78_zero != 0] <- 1

wages$re78_fac <- wages$re78
wages$re78_fac[wages$re78_fac == 0] <- 0
wages$re78_fac[wages$re78_fac != 0] <- 1
wages$re78_fac <-as.factor(wages$re78_fac)

wages$treat_fac <- factor(wages$treat)
wages$black_fac <- as.factor(wages$black)
wages$married_fac <- as.factor(wages$married)
wages$hispan_fac <- as.factor(wages$hispan)
wages$nodegree_fac <- as.factor(wages$nodegree)

wages$edu_fac <- wages$educ
wages$edu_fac[wages$edu_fac <= 9] <- 0
wages$edu_fac[wages$edu_fac > 9 & wages$edu_fac <= 12] <- 1
wages$edu_fac[wages$edu_fac > 12] <- 2
wages$edu_fac <- as.factor(wages$edu_fac)
wages$age_cen <- wages$age - mean(wages$age)

# convert education
#Education
#0~9 Below High School
#10~12 High School
#13~18 High School Graduate Above

# treat_fac

table(wages[,c("re78_fac","treat_fac")])
table(wages[,c("re78_fac","treat_fac")])/sum(table(wages[,c("re78_fac","treat_fac")]))
apply(table(wages[,c("re78_fac","treat_fac")])/sum(table(wages[,c("re78_fac","treat_fac")])),
      2,function(x) x/sum(x))
tapply(wages$re78_fac, wages$treat_fac, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","treat_fac")]))
# 0.7686


# edu_fac
table(wages[,c("re78_fac","edu_fac")])
table(wages[,c("re78_fac","edu_fac")])/sum(table(wages[,c("re78_fac","edu_fac")]))
apply(table(wages[,c("re78_fac","edu_fac")])/sum(table(wages[,c("re78_fac","edu_fac")])),
      2,function(x) x/sum(x)) 
tapply(wages$re78_fac, wages$edu_fac, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","edu_fac")]))
# 0.2472


# black
table(wages[,c("re78_fac","black")])
table(wages[,c("re78_fac","black")])/sum(table(wages[,c("re78_fac","black")]))
apply(table(wages[,c("re78_fac","black")])/sum(table(wages[,c("re78_fac","black")])),
      2,function(x) x/sum(x)) 
tapply(wages$re78_fac, wages$black, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","black")]))
# 0.0201

# hispan
table(wages[,c("re78_fac","hispan")])
table(wages[,c("re78_fac","hispan")])/sum(table(wages[,c("re78_fac","hispan")]))
apply(table(wages[,c("re78_fac","hispan")])/sum(table(wages[,c("re78_fac","hispan")])),
      2,function(x) x/sum(x)) 
tapply(wages$re78_fac, wages$hispan, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","hispan")]))
# p-value = 0.2052


# married
table(wages[,c("re78_fac","married")])
table(wages[,c("re78_fac","married")])/sum(table(wages[,c("re78_fac","married")]))
apply(table(wages[,c("re78_fac","married")])/sum(table(wages[,c("re78_fac","married")])),
      2,function(x) x/sum(x)) 
tapply(wages$re78_fac, wages$married, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","married")]))
# p-value = 0.5756

# nodegree
table(wages[,c("re78_fac","nodegree")])
table(wages[,c("re78_fac","nodegree")])/sum(table(wages[,c("re78_fac","nodegree")]))
apply(table(wages[,c("re78_fac","nodegree")])/sum(table(wages[,c("re78_fac","nodegree")])),
      2,function(x) x/sum(x)) 
tapply(wages$re78_fac, wages$nodegree, function(x) table(x)/sum(table(x)))
chisq.test(table(wages[,c("re78_fac","nodegree")]))
# p-value = 0.5053


ggplot(wages,aes(x=re78_fac, y=age, fill=re78_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="age",
       x="re78_fac",y="age") +
  theme_classic() + theme(legend.position="none") #+


binnedplot(y=wages$re78_zero,wages$age,xlab="age",col.pts="navy",
           ylab ="re78_zero?",main="Binned age and re78_zero",
           col.int="white")


################################################################################
################################### Model ######################################
################################################################################
model_origin <- glm(re78_fac ~ treat_fac + edu_fac + age_cen + black_fac + hispan_fac + married_fac
                    + nodegree_fac
                    , data = wages, family = binomial)
summary(model_origin)

rawresid_origin <- residuals(model_origin,"resp")

#binned residual plots
binnedplot(x=fitted(model_origin),y=rawresid_origin,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# 0.55-0.65 not enough data ,but random, 3 potential outliers


binnedplot(wages$age_cen,rawresid_origin,xlab="age_cen",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# the trend goes up and down, we may need some transformation


###### Model validation

#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_origin) >= 0.5, "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_origin) >= mean(wages$re78_zero), "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

roc(wages$re78_zero,fitted(model_origin),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

##################################### age2 #####################################
wages$age_sq2 <- wages$age_cen^2
binnedplot(y=wages$re78_zero,wages$age_sq2,xlab="age_sq2",col.pts="navy",
           ylab ="re78_zero?",main="Binned age_sq2 and re78_zero",
           col.int="white")

model_age2 <- glm(re78_fac ~ treat_fac + edu_fac + age_cen + age_sq2 + black_fac + hispan_fac + married_fac
                    + nodegree_fac
                    , data = wages, family = binomial)
summary(model_age2)

rawresid_age2 <- residuals(model_age2,"resp")

#binned residual plots
binnedplot(x=fitted(model_age2),y=rawresid_age2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# a litter better, outliers to 1, extreme still exist

binnedplot(wages$age_sq2,rawresid_age2,xlab="age_sq2",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# no obvious trend


#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_age2) >= 0.5, "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_age2) >= mean(wages$re78_zero), "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

roc(wages$re78_zero,fitted(model_origin),plot=T,legacy.axes=T,print.auc =T,col="red3")
roc(wages$re78_zero,fitted(model_age2),plot=T,legacy.axes=T,col="blue3",add=T)
legend('bottomright', c('model1','model2'),lty=c(1,1),
       lwd=c(2,2),col=c('red3','blue3'))


################################################################################
################################# interaction ##################################
################################################################################
#-----------------treat vs all-------------------------#

# edu~ black  black~ marriage black~nodegree age~edu age~treat  treat vs all
# treat ~ edu
re78_nontreat=wages$re78_fac[wages$treat==0]
edu_treat=wages$edu_fac[wages$treat==0]
table(re78_nontreat,edu_treat)/sum(table(re78_nontreat, edu_treat))
chisq.test(table(re78_nontreat,edu_treat))
# 0.08759

re78_nontreat=wages$re78_fac[wages$treat==1]
edu_treat=wages$edu_fac[wages$treat==1]
table(re78_nontreat,edu_treat)/sum(table(re78_nontreat, edu_treat))
chisq.test(table(re78_nontreat,edu_treat))
# 0.8751

# treat ~ black
re78_nontreat=wages$re78_fac[wages$treat==0]
black_treat=wages$black_fac[wages$treat==0]
table(re78_nontreat,black_treat)/sum(table(re78_nontreat, black_treat))
chisq.test(table(re78_nontreat,black_treat))
# 0.08759

re78_nontreat=wages$re78_fac[wages$treat==1]
edu_treat=wages$edu_fac[wages$treat==1]
table(re78_nontreat,edu_treat)/sum(table(re78_nontreat, edu_treat))
chisq.test(table(re78_nontreat,edu_treat))
# 0.1076


# treat ~ marry
re78_nontreat=wages$re78_fac[wages$treat==0]
marry_treat=wages$married_fac[wages$treat==0]
table(re78_nontreat,marry_treat)/sum(table(re78_nontreat, marry_treat))
chisq.test(table(re78_nontreat,marry_treat))
# 0.9553

re78_nontreat=wages$re78_fac[wages$treat==1]
marry_treat=wages$married_fac[wages$treat==1]
table(re78_nontreat,marry_treat)/sum(table(re78_nontreat, marry_treat))
chisq.test(table(re78_nontreat,marry_treat))
# 0.1873


# treat ~ hispan
re78_nontreat=wages$re78_fac[wages$treat==0]
hispan_treat=wages$hispan_fac[wages$treat==0]
table(re78_nontreat,hispan_treat)/sum(table(re78_nontreat, hispan_treat))
chisq.test(table(re78_nontreat,hispan_treat))
# 0.6366

re78_nontreat=wages$re78_fac[wages$treat==1]
hispan_treat=wages$hispan_fac[wages$treat==1]
table(re78_nontreat,hispan_treat)/sum(table(re78_nontreat, hispan_treat))
chisq.test(table(re78_nontreat,hispan_treat))
# 0.1149


# treat ~ nodegree
re78_nontreat=wages$re78_fac[wages$treat==0]
nodegree_treat=wages$nodegree_fac[wages$treat==0]
table(re78_nontreat,nodegree_treat)/sum(table(re78_nontreat, nodegree_treat))
chisq.test(table(re78_nontreat,nodegree_treat))
# 0.811

re78_nontreat=wages$re78_fac[wages$treat==1]
nodegree_treat=wages$nodegree_fac[wages$treat==1]
table(re78_nontreat,nodegree_treat)/sum(table(re78_nontreat, nodegree_treat))
chisq.test(table(re78_nontreat,nodegree_treat))
# 0.5377


# treat~ age

#lets set up the graphics device to show two plots side by side 
par(mfcol=c(2,1))

#first plot for educnew = 0
binnedplot(wages$age[wages$treat_fac==0], y=wages$re78_zero[wages$treat_fac==0], 
           xlab = "age", ylab = "Switch cases", main = "Binned age and re78 cases (treat=0)") 

#next the plot for educnew = 1
binnedplot(wages$age[wages$treat_fac==1], y=wages$re78_zero[wages$treat_fac==1], 
           xlab = "age", ylab = "Switch cases", main = "Binned age and re78 cases (treat=1)") 

# not obvious

# edu~ black  black~ marriage black~nodegree age~edu

# black ~ edu
re78_black=wages$re78_fac[wages$black_fac==0]
edu_black =wages$edu_fac[wages$black_fac==0]
table(re78_black,edu_black)/sum(table(re78_black, edu_black))
chisq.test(table(re78_black,edu_black))
# 0.03548

re78_black=wages$re78_fac[wages$black_fac==1]
edu_black=wages$edu_fac[wages$black_fac==1]
table(re78_black,edu_black)/sum(table(re78_black, edu_black))
chisq.test(table(re78_black,edu_black))
# 0.324


# black~ marriage
re78_black=wages$re78_fac[wages$black_fac==0]
marry_black =wages$married_fac[wages$black_fac==0]
table(re78_black,marry_black)/sum(table(re78_black, marry_black))
chisq.test(table(re78_black,marry_black))
# 0.53

re78_black=wages$re78_fac[wages$black_fac==1]
marry_black=wages$married_fac[wages$black_fac==1]
table(re78_black,marry_black)/sum(table(re78_black, marry_black))
chisq.test(table(re78_black,marry_black))
# 0.5304


# black~nodegree
re78_black=wages$re78_fac[wages$black_fac==0]
nodegree_black =wages$nodegree_fac[wages$black_fac==0]
table(re78_black,nodegree_black)/sum(table(re78_black, nodegree_black))
chisq.test(table(re78_black,nodegree_black))
# 0.53

re78_black=wages$re78_fac[wages$black_fac==1]
nodegree_black=wages$nodegree_fac[wages$black_fac==1]
table(re78_black,nodegree_black)/sum(table(re78_black, nodegree_black))
chisq.test(table(re78_black,nodegree_black))
# 0.8742

#age ~ edu
#lets set up the graphics device to show two plots side by side 
par(mfcol=c(3,1))

#first plot for educnew = 0
binnedplot(wages$age[wages$edu_fac==0], y=wages$re78_zero[wages$edu_fac==0], 
           xlab = "age", ylab = "Switch cases", main = "Binned age and re78 cases (edu=0)") 

#next the plot for educnew = 1
binnedplot(wages$age[wages$edu_fac==1], y=wages$re78_zero[wages$edu_fac==1], 
           xlab = "age", ylab = "Switch cases", main = "Binned age and re78 cases (edu=1)") 

#next the plot for educnew = 2
binnedplot(wages$age[wages$edu_fac==2], y=wages$re78_zero[wages$edu_fac==2], 
           xlab = "age", ylab = "Switch cases", main = "Binned age and re78 cases (edu=2)") 


################################################################################
############################### Stepwise  Model ################################
################################################################################
NullModel <- glm(re78_fac ~ 1, data = wages, family = binomial)
FullModel <- glm(re78_fac ~ treat_fac + edu_fac + age_cen + black_fac + hispan_fac
                 + married_fac + nodegree_fac
                 + treat_fac*(edu_fac + age_cen + black_fac + hispan_fac + married_fac + nodegree_fac)
                 + edu_fac:black_fac + black_fac:married_fac + black_fac:nodegree_fac
                 + age_cen*edu_fac
                  , data = wages, family = binomial)
model_step <- step(NullModel, scope=formula(FullModel), direction="both", trace=0)
summary(model_step)
# re78_fac ~ age_cen + black_fac

########## anova #############
model_1_1 <- glm(re78_fac ~ age_cen + black_fac + treat_fac, data = wages, family = binomial)
anova(model_1_1, model_step, test='Chisq')
# 0.2581

# treat~age
model_1_2 <- glm(re78_fac ~ age_cen + black_fac + treat_fac + treat_fac:age_cen, data = wages, family = binomial)
anova(model_1_2, model_step, test='Chisq')
# 0.03817  keep treat

# treat~ black
model_1_3 <- glm(re78_fac ~ age_cen + black_fac + treat_fac + treat_fac:age_cen + treat_fac:black_fac, data = wages, family = binomial)
anova(model_1_3, model_1_2, test='Chisq')
# 0.1389

# age2
model_1_4 <- glm(re78_fac ~ age_cen + black_fac + treat_fac + age_sq2 + treat_fac:age_cen, data = wages, family = binomial)
anova(model_1_2, model_1_4, test='Chisq')
# 0.04316

# age ~ edu
model_1_5 <- glm(re78_fac ~ age_cen + black_fac + treat_fac + age_sq2 + treat_fac:age_cen + edu_fac:age_cen,
                 data = wages, family = binomial)
anova(model_1_2, model_1_5, test='Chisq')
# 0.04387 

##############################################################################
################################ final model #################################
final_model <- glm(re78_fac ~ age_cen + black_fac + treat_fac + age_sq2 + treat_fac:age_cen + edu_fac:age_cen,
                   data = wages, family = binomial)
summary(final_model)
vif(final_model)
# all are less than 10

rawresid_final <- residuals(final_model,"resp")

#binned residual plots
binnedplot(x=fitted(final_model),y=rawresid_final,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(wages$age_cen,rawresid_final,xlab="age_cen",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# looks good

###### Model validation

#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= 0.5, "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= mean(wages$re78_zero), "1","0")),
                            wages$re78_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

roc(wages$re78_zero,fitted(final_model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
