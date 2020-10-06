setwd("E:\duke\20 fall\702\team projects\team project 1")


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

wages <- read.delim('C:/Users/Dean Huang/Documents/Duke University (MIDS 2022)/Fall 2020/IDS 702/Dataset/lalondedata.txt', sep=',')
head(wages)
dim(wages)
summary(wages)
wages$diff <- wages$re78-wages$re74
wages$log_diff <- log(wages$diff)

wages$treat_fac <- factor(wages$treat)
wages$black_fac <- as.factor(wages$black)
wages$married_fac <- as.factor(wages$married)
wages$hispan_fac <- as.factor(wages$hispan)
wages$nodegree_fac <- as.factor(wages$nodegree)
wages$age_cen <- wages$age - mean(wages$age)
wages$age2<-wages$age_cen^2


# convert education
#Education
#0~9 Below High School
#10~12 High School
#13~18 High School Graduate Above
ggplot(wages,aes(x=educ, y=diff)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(col="red3", method='lm') + theme_classic() +
  labs(title="diff vs educ",x="educ",y="diff")

wages$edu_fac <- wages$educ
wages$edu_fac[wages$edu_fac <= 8] <- 0
wages$edu_fac[wages$edu_fac > 8 & wages$edu_fac <= 11] <- 1
wages$edu_fac[wages$edu_fac > 11] <- 2
wages$edu_fac <- as.factor(wages$edu_fac)
table(wages$edu_fac)


############################  normality ###############################
hist(wages$diff)
qqnorm(wages$diff, pch = 1, frame = FALSE)
qqline(wages$diff, col = "steelblue", lwd = 2)

############################  numerical ###############################
# educ & re 74&78
# table(wages$educ)
# hist(wages$educ)
# 
# ggplot(wages,aes(x=educ, y=re74)) +
#   geom_point() + #coord_flip()# +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Income 74 vs Education",
#        x="Education",y='Income 74') + 
#   geom_smooth(col="red3", method="lm") +
#   theme_classic() + theme(legend.position="none")+
#   theme(plot.title = element_text(hjust = 0.5))
# # some trend
# ggplot(wages,aes(x=educ, y=re78)) +
#   geom_point() + #coord_flip()# +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Income 78 vs Education",
#        x="Education",y='Income 78') + 
#   geom_smooth(col="red3", method="lm") +
#   theme_classic() + theme(legend.position="none")+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # age
# ggplot(wages,aes(x=age, y=re74)) +
#   geom_point() + #coord_flip()# +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Income 74 vs age",
#        x="age",y='Income 74') + 
#   geom_smooth(col="red3", method="lm") +
#   theme_classic() + theme(legend.position="none")+
#   theme(plot.title = element_text(hjust = 0.5))
# # some trend
# ggplot(wages,aes(x=age, y=re78)) +
#   geom_point() + #coord_flip()# +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Income 78 vs age",
#        x="age",y='Income 78') + 
#   geom_smooth(col="red3", method="lm") +
#   theme_classic() + theme(legend.position="none")+
#   theme(plot.title = element_text(hjust = 0.5))


########################### diff between 74 and 78 #############################

###-------------------------------- numerical -------------------------------###
ggplot(wages,aes(x=educ, y=diff)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Income diff vs Education",
       x="Education",y='Income diff') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Income diff vs Age",
       x="age",y='Income diff') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))


###-------------------------------- categorical------------------------------###

# treat 
ggplot(wages,aes(x=treat_fac, y=diff, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="treat vs salary",
       x="treat",y="diff") +
  theme_classic() + theme(legend.position="none")

binnedplot(y=wages$treat,wages$diff,xlab="treat",col.pts="navy",
           ylab ="diff",main="Binned diff and treat",
           col.int="white")

# black
ggplot(wages,aes(x=black_fac, y=diff, fill=black_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="black vs salary",
       x="black",y="diff") +
  theme_classic() + theme(legend.position="none")

# married
ggplot(wages,aes(x=married_fac, y=diff, fill=married_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="married_fac vs diff",
       x="married_fac",y="diff") +
  theme_classic() + theme(legend.position="none")

# hispan
ggplot(wages,aes(x=hispan_fac, y=diff, fill=hispan_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="hispan vs diff",
       x="hispan",y="diff") +
  theme_classic() + theme(legend.position="none")

# degree
ggplot(wages,aes(x=nodegree_fac, y=diff, fill=nodegree_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") + labs(title="nodegree vs salary", x="nodegree",y="diff") +
  theme_classic() + theme(legend.position="none", axis.title.x =element_text(size=15),
  axis.title.y=element_text(size=15), axis.text = element_text(size=14), 
  plot.title = element_text(size = rel(1.5), lineheight = .9, family = "Times", face = "bold.italic"))



################################  interaction  #################################
###---------------- treat_fac vs all categorical predictors------------------###

## interaction between edu_fac & treat_fac
## diff vs edu_fac by treat_fac
ggplot(wages,aes(x=edu_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs edu_fac by treat_fac",x="edu_fac",y="diff") +
  facet_wrap( ~ treat_fac,ncol=3)
# some interaction

# interaction between married_fac & treat_fac
#diff vs married_fac by treat_fac
ggplot(wages,aes(x=married_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs married_fac by treat_fac",x="married_fac",y="diff") +
  facet_wrap( ~ treat_fac,ncol=3)
# some interaction


# interaction between black_fac & treat_fac
#diff vs black_fac by treat_fac
ggplot(wages,aes(x=black_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs black_fac by treat_fac",x="black_fac",y="diff") +
  facet_wrap( ~ treat_fac,ncol=3)
# no interaction


# interaction between hispan_fac & treat_fac
#diff vs hispan_fac by treat_fac
ggplot(wages,aes(x=hispan_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs hispan_fac by treat_fac",x="hispan_fac",y="diff") +
  facet_wrap( ~ treat_fac,ncol=3)
# no interaction


# interaction between nodegree_fac & treat_fac
#diff vs nodegree_fac by treat_fac
ggplot(wages,aes(x=nodegree_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs nodegree_fac by treat_fac",x="nodegree_fac",y="diff") +
  facet_wrap( ~ treat_fac,ncol=3)
# some interaction


###-------------------  age vs all categorical predictors -------------------###
## interaction between age & edu_fac
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age by edu_fac",x="treat_fac",y="diff") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ edu_fac,ncol=3)
# no interaction


# interaction between age & treat_fac
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age by treat",x="treat_fac",y="diff") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ treat_fac,ncol=3)
# some interaction


# interaction between age & black
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age by black_fac",x="treat_fac",y="diff") +
  facet_wrap( ~ black_fac,ncol=3)
# no interaction


# interaction between age & hispan
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age by hispan_fac",x="treat_fac",y="diff") +
  facet_wrap( ~ hispan_fac,ncol=3)
# no interaction


# interaction between age & married
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age by married_fac",x="treat_fac",y="diff") +
  facet_wrap( ~ married_fac,ncol=3)
# no interaction

# interaction between age & nodegree
ggplot(wages,aes(x=age, y=diff)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs age nodegree",x="treat_fac",y="diff") +
  facet_wrap( ~ nodegree_fac,ncol=3)
# no interaction


###------------------- diff vs all categorical predictors -------------------###
#diff vs treat_fac by edu_fac
ggplot(wages,aes(x=treat_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs treat_fac by edu_fac",x="treat_fac",y="diff") +
  facet_wrap( ~ edu_fac,ncol=3)
# no interaction

#diff vs treat_fac by black
ggplot(wages,aes(x=treat_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs treat_fac by black",x="treat_fac",y="diff") +
  facet_wrap( ~ black,ncol=2)
# no interaction

#diff vs treat_fac by hispan
ggplot(wages,aes(x=treat_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs treat_fac by hispan",x="treat_fac",y="diff") +
  facet_wrap( ~ hispan,ncol=2)
# no interaction

#diff vs treat_fac by married
ggplot(wages,aes(x=treat_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs treat_fac by married",x="treat_fac",y="diff") +
  facet_wrap( ~ married,ncol=2)
# no interaction

#diff vs treat_fac by nodegree
ggplot(wages,aes(x=treat_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs treat_fac by nodegree",x="treat_fac",y="diff") +
  facet_wrap( ~ nodegree,ncol=2)
# no interaction


###------------------- edu_fac vs all categorical predictors -------------------###
#diff vs edu_fac by black
ggplot(wages,aes(x=edu_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs edu_fac by black",x="edu_fac",y="diff") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ black,ncol=3)
# no interaction 


# diff vs edu_fac by hispan
ggplot(wages,aes(x=edu_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs edu_fac by hispan",x="edu_fac",y="diff") +
  facet_wrap( ~ hispan,ncol=3)
# some interaction

# diff vs edu_fac by marriage
ggplot(wages,aes(x=edu_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs edu_fac by married",x="edu_fac",y="diff") +
  facet_wrap( ~ married,ncol=3)
# no interaction

# diff vs edu_fac by nodegree
ggplot(wages,aes(x=edu_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs edu_fac by nodegree",x="edu_fac",y="diff") +
  facet_wrap( ~ nodegree,ncol=3)
#no interaction


###----------------- black_fac vs all categorical predictors ----------------###
#diff vs black by married
ggplot(wages,aes(x=black_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs black by married",x="black",y="diff") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ married,ncol=2)
# some interaction

#diff vs black by nodegree
ggplot(wages,aes(x=black_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs black by nodegree",x="black",y="diff") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ nodegree,ncol=2)
#some interaction


###------------------ hispan vs all categorical predictors ------------------###
#diff vs hispan by married
ggplot(wages,aes(x=hispan_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs hispan by married",x="hispan",y="diff") +
  facet_wrap( ~ married,ncol=2)
#no interaction

#diff vs hispan by nodegree
ggplot(wages,aes(x=hispan_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs hispan by nodegree",x="hispan",y="diff") +
  facet_wrap( ~ nodegree,ncol=2)
#no interaction


###------------------ nodegree vs all categorical predictors -----------------###
#diff vs married by nodegree
ggplot(wages,aes(x=married_fac, y=diff)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="diff vs married by nodegree",x="married",y="diff") +
  facet_wrap( ~ nodegree,ncol=2)
#no interaction



################################################################################
################################### Model ######################################
################################################################################
# original model with main effects
model_0  <- lm(diff ~ treat_fac + age_cen + edu_fac + married_fac + hispan_fac 
               + black_fac + nodegree_fac, data = wages)
summary(model_0)

plot(model_0, which=1, col=c('blue4'))

ggplot(model_0,aes(x=age_cen, y=model_0$residual)) + 
  geom_point(alpha = .7, col='brown') +  
  geom_hline(yintercept=0,col="black") + theme_classic() +
  labs(title="Residuals vs Age",x="Age",y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

plot(model_0, which=2, col=c('blue4'))

################################# square of age ################################
model_age2  <- lm(diff ~ treat_fac + age_cen + age2 + edu_fac + married_fac + hispan_fac 
                  + black_fac + nodegree_fac, data = wages)
summary(model_age2)
ggplot(model_age2,aes(x=age2, y=model_0$residual)) + 
  geom_point(alpha = .7, col='brown') +  
  geom_hline(yintercept=0,col="black") + theme_classic() +
  labs(title="Residuals vs square of Aage",x="square of age",y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

############################# interaction stepwise ############################# 
NullModel <- lm(diff~1, data = wages)
FullModel <- lm(diff ~ treat_fac + age_cen + edu_fac + married_fac + hispan_fac+ black_fac + nodegree_fac 
                + treat_fac:age_cen + treat_fac:edu_fac + treat_fac:married_fac + treat_fac:hispan_fac 
                + treat_fac:black_fac + treat_fac:nodegree_fac + hispan_fac:edu_fac
                + black_fac:married_fac + black_fac:nodegree_fac
                 , data = wages)
model_step <- step(NullModel, scope=formula(FullModel), direction="both", trace=0)
# diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen

summary(model_step)


#################################  anova #######################################
##--------------------------------- edu ------------------------------------## 

# eilminate age*edu_fac  0.5044
model_1_1 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + age_cen*edu_fac, 
                data = wages)
anova(model_step, model_1_1)


# edu vs nodegree 0.783
model_1_2 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + edu_fac + treat_fac:age_cen + nodegree_fac:edu_fac, 
                data = wages)
anova(model_step, model_1_2)


# edu vs black  0.7566
model_1_3 <- lm(diff ~ married_fac + treat_fac + age_cen + black_fac + edu_fac + treat_fac:age_cen + black_fac:edu_fac
                , data = wages)
anova(model_step, model_1_3)


##--------------------------------- treat ------------------------------------## 
# eilminate treat:black_fac  0.4422 
model_2_1 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + treat_fac:black_fac, 
                data = wages)
anova(model_step, model_2_1)

# eilminate treat:edu_fac  0.7138 
model_2_2 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + edu_fac + treat_fac:age_cen + treat_fac:edu_fac, 
                data = wages)
anova(model_step, model_2_2)

# eilminate treat:married_fac  0.6318
model_2_3 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + treat_fac:married_fac, 
                data = wages)
anova(model_step, model_2_3)

# eilminate treat:hispan_fac  0.705
model_2_4 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + treat_fac:hispan_fac, 
                data = wages)
anova(model_step, model_2_4)

# eilminate treat:nodegree_fac 0.8247
model_2_5 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + nodegree_fac + treat_fac:age_cen + treat:nodegree_fac, 
                data = wages)
anova(model_step, model_2_5)


##--------------------------------- black ------------------------------------##

# eilminate black_fac*married_fac  0.5844
model_3_1 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + black_fac*married_fac, 
                data = wages)
anova(model_step, model_3_1)

# eilminate black_fac*nodegree_fac  0.6151
model_3_2 <- lm(formula = diff ~ married_fac + treat_fac + age_cen + treat_fac:age_cen + black_fac*nodegree_fac, 
                data = wages)
anova(model_step, model_3_2)



############################# test for no degree ###############################

# final model with centered age
model_drop_degree <- lm(diff ~ married_fac + age_cen + edu_fac + treat_fac + treat_fac:age_cen 
                   ,data = wages)

model_add_degree <- lm(diff ~ married_fac  + age_cen + edu_fac + treat_fac + treat_fac:age_cen + nodegree_fac 
                   , data = wages)


anova(model_drop_degree, model_add_degree)    # no p-value
summary(model_drop_degree)
summary(model_add_degree)



################################ Final Model ###################################

model_final <- lm(diff ~ married_fac  + age_cen + treat_fac + treat_fac:age_cen 
                 , data = wages)

summary(model_final)
confint(model_final)
# mean(wages$age_cen)



################################################################################
############################ Checking Assumptinos ##############################
################################################################################

## linearity
ggplot(wages,aes(x=age_cen,y=model_final$residual)) + geom_point(alpha=.7) + 
  geom_hline(yintercept=0, col='red3') + theme_classic() +
  labs(title = 'Residuals vs age', x='age',y="Residuals")

# Independence and Equal variance
plot(model_final, which=1, col=c('blue4'))

# Normality
plot(model_final, which=2, col=c('blue4'))


################################### Ourliers ###################################

# Ourliers
plot(model_final, which=2:5, col=c('blue4'))

# Leverage
n <- nrow(model.matrix(model_final)); p <- ncol(model.matrix(model_final))
lev_scores <- hatvalues(model_final) #can also use influence(regwagecsquares)$hat 
plot(lev_scores,col=ifelse(lev_scores > (2*p/n), 'red2', 'navy'),type="h",
     ylab="Leverage score",xlab="Index",main="Leverage Scores for All Observations")


############################## multicollinearity ###############################

model_vif <- lm(diff ~ married_fac + treat_fac:age_cen + nodegree_fac + edu_fac
                , data = wages)
vif(model_vif)

model_vif <- lm(diff ~ married_fac + treat_fac:age_cen + edu_fac
                , data = wages)
vif(model_vif)


y_predict_total <- predict(model_final, wages)
RSME_total <- (wages$diff - y_predict_total)^2
mean(RSME_total)




