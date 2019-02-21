install.packages("readxl")
library(knitr)
library(kableExtra)
library(readxl)
library(dplyr)
library(ggplot2)
CaseStudy2_data <- read_excel("../datasets/CaseStudy2-data.xlsx")

glimpse(CaseStudy2_data)
CaseStudy2_data$Age <- as.integer(CaseStudy2_data$Age)
CaseStudy2_data$Attrition <- as.factor(CaseStudy2_data$Attrition)
CaseStudy2_data$BusinessTravel <- as.factor(CaseStudy2_data$BusinessTravel)
CaseStudy2_data$DailyRate <- as.integer(CaseStudy2_data$DailyRate)
CaseStudy2_data$Department <- as.factor(CaseStudy2_data$Department)
CaseStudy2_data$DistanceFromHome <- as.integer(CaseStudy2_data$DistanceFromHome)
CaseStudy2_data$Education <- as.factor(CaseStudy2_data$Education)
CaseStudy2_data$EducationField <- as.factor(CaseStudy2_data$EducationField)
CaseStudy2_data$EmployeeCount <- NULL
CaseStudy2_data$EmployeeNumber <- as.integer(CaseStudy2_data$EmployeeNumber)
CaseStudy2_data$EnvironmentSatisfaction <- as.factor(CaseStudy2_data$EnvironmentSatisfaction)
CaseStudy2_data$Gender <- as.factor(CaseStudy2_data$Gender)
CaseStudy2_data$HourlyRate <- as.integer(CaseStudy2_data$Age)
CaseStudy2_data$JobInvolvement <- as.factor(CaseStudy2_data$JobInvolvement)
CaseStudy2_data$JobLevel <- as.factor(CaseStudy2_data$JobLevel)
CaseStudy2_data$JobRole <- as.factor(CaseStudy2_data$JobRole)
CaseStudy2_data$JobSatisfaction <- as.factor(CaseStudy2_data$JobSatisfaction)
CaseStudy2_data$MaritalStatus <- as.factor(CaseStudy2_data$MaritalStatus)
CaseStudy2_data$MonthlyIncome <- as.integer(CaseStudy2_data$MonthlyIncome)
CaseStudy2_data$MonthlyRate <- as.integer(CaseStudy2_data$MonthlyRate)
CaseStudy2_data$NumCompaniesWorked <- as.integer(CaseStudy2_data$NumCompaniesWorked)
CaseStudy2_data$Over18 <- as.factor(CaseStudy2_data$Over18)
CaseStudy2_data$OverTime <- as.factor(CaseStudy2_data$OverTime)
CaseStudy2_data$PercentSalaryHike <- as.integer(CaseStudy2_data$PercentSalaryHike)
CaseStudy2_data$PerformanceRating <- as.factor(CaseStudy2_data$PerformanceRating)
CaseStudy2_data$RelationshipSatisfaction <- as.factor(CaseStudy2_data$RelationshipSatisfaction)
CaseStudy2_data$StandardHours <- as.integer(CaseStudy2_data$StandardHours)
CaseStudy2_data$StockOptionLevel <- as.factor(CaseStudy2_data$StockOptionLevel)
CaseStudy2_data$TotalWorkingYears <- as.integer(CaseStudy2_data$TotalWorkingYears)
CaseStudy2_data$TrainingTimesLastYear <- as.integer(CaseStudy2_data$TrainingTimesLastYear)
CaseStudy2_data$WorkLifeBalance <- as.factor(CaseStudy2_data$WorkLifeBalance)
CaseStudy2_data$YearsAtCompany <- as.integer(CaseStudy2_data$YearsAtCompany)
CaseStudy2_data$YearsInCurrentRole <- as.integer(CaseStudy2_data$YearsInCurrentRole)
CaseStudy2_data$YearsSinceLastPromotion <- as.integer(CaseStudy2_data$YearsSinceLastPromotion)
CaseStudy2_data$YearsWithCurrManager <- as.integer(CaseStudy2_data$YearsWithCurrManager)
glimpse(CaseStudy2_data)

write.csv(CaseStudy2_data, file="../datasets/HRdata.csv", row.names = FALSE)

HRdf <- read.csv("../datasets/HRdata.csv")
glimpse(HRdf)

HRdf$Education <- as.factor(HRdf$Education)
HRdf$EnvironmentSatisfaction <- as.factor(HRdf$EnvironmentSatisfaction)
HRdf$JobInvolvement <- as.factor(HRdf$JobInvolvement)
HRdf$JobLevel <- as.factor(HRdf$JobLevel)
HRdf$JobSatisfaction <- as.factor(HRdf$JobSatisfaction)
HRdf$PerformanceRating <- as.factor(HRdf$PerformanceRating)
HRdf$RelationshipSatisfaction <- as.factor(HRdf$RelationshipSatisfaction)
HRdf$StockOptionLevel <- as.factor(HRdf$StockOptionLevel)
HRdf$WorkLifeBalance <- as.factor(HRdf$WorkLifeBalance)

glimpse(HRdf)


theme_set(theme_light())


## Histograms for attrition
ggplot(HRdf, aes(HRdf$JobLevel)) + 
  geom_bar(aes(fill=Attrition), width = 0.5) +
  labs(title = "Attrition by Job Level",
       subtitle = "Attrition by Specific Job Level",
       x = "Job Level",
       y = "Frequency") +
  coord_flip()

## Box Plots of Monthly Income by Job Role 
ggplot(HRdf, aes(x=JobRole, y=MonthlyIncome, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of Monthly Income by Job Involvment 
ggplot(HRdf, aes(x=JobInvolvement, y=MonthlyIncome, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of Age by Job Role
ggplot(HRdf, aes(x=JobRole, y=Age, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of YearsAtCompany by Job Role 
ggplot(HRdf, aes(x=JobRole, y=YearsAtCompany, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of YearsAtCompany by Job Role 
ggplot(HRdf, aes(x=JobRole, y=TotalWorkingYears, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of YearsSince Promotion by Job Role 
ggplot(HRdf, aes(x=JobRole, y=YearsSinceLastPromotion, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of YearswCurrent Manager by Job Role 
ggplot(HRdf, aes(x=JobRole, y=YearsWithCurrManager, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of TrainingTimes by Job Role 
ggplot(HRdf, aes(x=Department, y=TrainingTimesLastYear, fill=Attrition)) + 
  geom_boxplot()

## Box Plots of DistanceFromHome by MaritalStatus 
ggplot(HRdf, aes(x=MaritalStatus, y=DistanceFromHome, fill=Attrition)) + 
  geom_boxplot()

ggplot(HRdf, aes(BusinessTravel, Attrition)) + geom_jitter()

ggplot(HRdf, aes(OverTime, Attrition)) + geom_jitter()

ggplot(HRdf, aes(Education, Attrition)) + geom_jitter()

ggplot(HRdf, aes(OverTime, Attrition)) + geom_jitter()


install.packages("rpart", "rpart.plot")
library(rpart, rpart.plot) #for trees
tree1 <- rpart(Attrition ~ Age + Education + MonthlyIncome + JobLevel + StockOptionLevel + MaritalStatus +
                 Department + BusinessTravel + DistanceFromHome + EducationField + Gender + JobRole +
                 NumCompaniesWorked + OverTime + PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + PerformanceRating +
                 RelationshipSatisfaction + WorkLifeBalance,
                 data = HRdf,  method="class")

summary(tree1)
rpart.plot(tree1)

