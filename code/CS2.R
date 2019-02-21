library(dplyr)
library(ggplot2)

#loading the dataset into R and checking missing data (na)
attrition <- readxl::read_xlsx('CaseStudy2-data.xlsx')
sapply(attrition,function(x) sum(is.na(x)))

#another tool to check dataset for missing values

library(Amelia)
missmap(attrition, main = "Missing values vs observed")


#change string into factors for categorical variables (required for logistic regression)
attrition$BusinessTravel <- as.factor(attrition$BusinessTravel)
attrition$Attrition <- as.factor(attrition$Attrition)
attrition$Department <- as.factor(attrition$Department)
attrition$EducationField <- as.factor(attrition$EducationField)
attrition$Gender <- as.factor(attrition$Gender)
attrition$JobRole <- as.factor(attrition$JobRole)
attrition$MaritalStatus<- as.factor(attrition$MaritalStatus)
attrition$Over18 <- as.factor(attrition$Over18)
attrition$OverTime <- as.factor(attrition$OverTime)

#remove over18 column because it only has one level (all 'yes'), which won't work for logistic regression)
attrition$Over18 <- NULL  

#fit full logistic model 
fullmodel <- glm(Attrition~., family= binomial(link = "logit"), data = attrition)
summary(fullmodel)

#refine full model through forwrad, backward and stepwise methods
backward_model <- step(fullmodel)
forward_model <- step(fullmodel, direction = 'forward', k=2)
stepwise_model <- step(fullmodel, direction = 'both', k=2)

#checking several important parameters of prediction model

# 1. OverTime
attrition%>%ggplot() +
  geom_bar(aes(OverTime, fill= Attrition), position = "stack")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(label = 'The Effect of Overtime on Attrition')

attrition%>%ggplot(aes(OverTime, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Overtime on Attrition Rate', position= 'center')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

# 2. JobRole
attrition%>%ggplot() +
  geom_bar(aes(JobRole, fill= Attrition), position = "stack")+
  scale_fill_manual(values = c("Yes" = "red","No" = "dark green"))+
  ggtitle(label = 'The Effect of Job role on Attrition')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

attrition%>%ggplot(aes(JobRole, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Overtime on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  ylab('Percentages')

# 3. BusinessTravel
attrition%>%ggplot() +
  geom_bar(aes(BusinessTravel, fill= Attrition), position = "stack")+
  ggtitle(label = 'The Effect of BusinessTravel on Attrition')+
  theme(plot.title = element_text(hjust = 0.5))

attrition%>%ggplot(aes(BusinessTravel, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of BusinessTravel on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

# 4. MaritalStatus
attrition%>%ggplot() +
  geom_bar(aes(MaritalStatus, fill= Attrition), position = "stack")+
  ggtitle(label = 'The Effect of MaritalStatus on Attrition')+  
  theme(plot.title = element_text(hjust = 0.5))

attrition%>%ggplot(aes(MaritalStatus, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of MaritalStatus on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

# 5. JobSatisfaction
attrition%>%ggplot() +
  geom_bar(aes(JobSatisfaction, fill= Attrition), position = "stack")+
  ggtitle(label = 'The Effect of JobSatisfaction on Attrition')+
  theme(plot.title = element_text(hjust = 0.5))

attrition%>%ggplot(aes(JobSatisfaction, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of JobSatisfaction on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')


#############################################################################
#change several numerical into categorical variables per Travis's code  

attrition$Education <- as.factor(attrition$Education)
attrition$EnvironmentSatisfaction <- as.factor(attrition$EnvironmentSatisfaction)
attrition$JobInvolvement <- as.factor(attrition$JobInvolvement)
attrition$JobLevel <- as.factor(attrition$JobLevel)
attrition$JobSatisfaction <- as.factor(attrition$JobSatisfaction)
attrition$PerformanceRating <- as.factor(attrition$PerformanceRating)
attrition$RelationshipSatisfaction <- as.factor(attrition$RelationshipSatisfaction)
attrition$StockOptionLevel <- as.factor(attrition$StockOptionLevel)
attrition$WorkLifeBalance <- as.factor(attrition$WorkLifeBalance)

#refit full logistic model 
fullmodel <- glm(Attrition~., family= binomial(link = "logit"), data = attrition)
summary(fullmodel)

#refine model through forwrad, backward and stepwise methods
backward_model <- step(fullmodel)
forward_model <- step(fullmodel, direction = 'forward', k=2)
stepwise_model <- step(fullmodel, direction = 'both', k=2)






