library(readxl)
CaseStudy2_data <- read_excel("../datasets/CaseStudy2-data.xlsx")

## Gender [0 == Male] [1 == Female]
CaseStudy2_data$GenderVal <- ifelse(CaseStudy2_data$Gender=="Male", 0, 1)

## Turn Over [0 == No] [1 == Yes]
CaseStudy2_data$TurnOver <- ifelse(CaseStudy2_data$Attrition=="Yes", 1, 0)

## Buisiness Travel [0 == "Non-Travel] [1 == "Travel Rarely] [2 == "Travel Frequently]
CaseStudy2_data$BusTvlCode <- ifelse(CaseStudy2_data$BusinessTravel=="Travel_Rarely", 1, 
                                     ifelse(CaseStudy2_data$BusinessTravel=="Travel_Frequently", 
                                            2, 0))

## Departments [1 == "Sales"] [2 == "Research & Development"] [0 == "Human Resources"]
CaseStudy2_data$DeptCode <- ifelse(CaseStudy2_data$Department=="Sales", 1, 
                                     ifelse(CaseStudy2_data$Department=="Research & Development", 
                                            2, 0))
unique(CaseStudy2_data$EducationField)
## Departments [1 == "Life Sciences"] [2 == "Medical"] [3 == "Marketing"]
##             [4 == "Technical Degree] [5 == "Human Resources] [0 == "Other]
CaseStudy2_data$EduFieldCode <- ifelse(CaseStudy2_data$EducationField=="Other", 0, 
                                ifelse(CaseStudy2_data$EducationField=="Life Sciences", 1, 
                                ifelse(CaseStudy2_data$EducationField=="Medical", 2, 
                                ifelse(CaseStudy2_data$EducationField=="Marketing", 3,
                                ifelse(CaseStudy2_data$EducationField=="Technical Degree", 4,
                                ifelse(CaseStudy2_data$EducationField=="Human Resources", 5,
                                                                                    NA))))))
              
## Probably not valuable but wanted something to give a gross measure
CaseStudy2_data$HappyScore <- (CaseStudy2_data$EnvironmentSatisfaction + 
                              CaseStudy2_data$JobInvolvement +
                              CaseStudy2_data$JobSatisfaction +
                              CaseStudy2_data$RelationshipSatisfaction +
                              CaseStudy2_data$WorkLifeBalance)/5


sum(CaseStudy2_data$Attrition == "Yes")
sum(CaseStudy2_data$Attrition == "No")

## Subset Turnover Group
attrition <- subset(CaseStudy2_data, Attrition == "Yes")

## Subset Retained Employyes Group
happy <- subset(CaseStudy2_data, Attrition == "No")

## EDA
hist(attrition$Age, freq=F, breaks=12, main="Age Profile of Employees That Left Company",
     xlab="Age")
lines(density(attrition$Age), col="red")
lines(seq(10, 60, by=.5), dnorm(seq(10, 60, by=.5),
                                mean(attrition$Age), sd(attrition$Age)), col="blue")

hist(CaseStudy2_data$Age, freq=F, breaks=12, main="Age Profile of ALL Employees Current and Past",
     xlab="Age")
lines(density(CaseStudy2_data$Age), col="red")
lines(seq(10, 60, by=.5), dnorm(seq(10, 60, by=.5),
                                mean(CaseStudy2_data$Age), sd(CaseStudy2_data$Age)), col="blue")

hist(happy$Age, freq=F, breaks=12, main="Age Profile of Current Employees",
     xlab="Age")
lines(density(happy$Age), col="red")
lines(seq(10, 60, by=.5), dnorm(seq(10, 60, by=.5),
                                mean(happy$Age), sd(happy$Age)), col="blue")

hist(CaseStudy2_data$HappyScore, freq=F, breaks=10)
hist(attrition$HappyScore, freq=F, breaks=10)
hist(happy$HappyScore, freq=F, breaks=10)
mean(CaseStudy2_data$HappyScore)
mean(happy$HappyScore)
mean(attrition$HappyScore)
mean(happy$MonthlyIncome)
mean(attrition$MonthlyIncome)
mean(happy$TrainingTimesLastYear)
mean(attrition$TrainingTimesLastYear)


CaseStudy2.lm <- lm(MonthlyIncome ~ Education + JobLevel + PerformanceRating 
                    + TotalWorkingYears + YearsAtCompany, data=CaseStudy2_data)
summary(CaseStudy2.lm)

boxplot(MonthlyIncome~TurnOver,data=CaseStudy2_data, main="", 
        xlab="", ylab="")

boxplot(HappyScore~TurnOver,data=CaseStudy2_data, main="", 
        xlab="", ylab="")

boxplot(DistanceFromHome~TurnOver,data=CaseStudy2_data, main="", 
        xlab="", ylab="")

boxplot(JobSatisfaction~TurnOver,data=CaseStudy2_data, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Job Satisfaction")

boxplot(WorkLifeBalance~TurnOver,data=CaseStudy2_data, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Work Life Balance")

boxplot(JobInvolvement~TurnOver,data=CaseStudy2_data, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Job Involvement")

boxplot(EnvironmentSatisfaction~TurnOver,data=CaseStudy2_data, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Environment Satisfaction")

boxplot(Education~TurnOver,data=CaseStudy2_data, notch=TRUE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Education Level")

boxplot(JobLevel~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Job Level")

boxplot(NumCompaniesWorked~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Number of Companies Worked")

## Interesting ##
boxplot(BusTvlCode~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Business Travel (0=None, 1=Rare, 2=Frequent)")
hist(happy$BusTvlCode, freq=F, breaks=3)
hist(attrition$BusTvlCode, freq=F, breaks=3)
mean(CaseStudy2_data$BusTvlCode)
mean(attrition$BusTvlCode)
mean(happy$BusTvlCode)
##

boxplot(Age~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Employee Age")

boxplot(DeptCode~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Department (0=HR, 1=Sales, 2=R&D)")
hist(attrition$DeptCode, freq=T, breaks=3, main="Turn Over by Department", 
     ylab="Number of Employyes", xlab="Department (HR Sales R&D)")
TotalRD <- sum(CaseStudy2_data$DeptCode == 2)
TORD<- sum(CaseStudy2_data$DeptCode == 2 & CaseStudy2_data$Attrition == "Yes")

TotalHR <- sum(CaseStudy2_data$DeptCode == 0)
TOHR<- sum(CaseStudy2_data$DeptCode == 0 & CaseStudy2_data$Attrition == "Yes")

TotalSales <- sum(CaseStudy2_data$DeptCode == 1)
TOSales<- sum(CaseStudy2_data$DeptCode == 1 & CaseStudy2_data$Attrition == "Yes")

turnover.df <- data.frame(TotalHR, TOHR, TOHR/TotalHR)
turnover.df <- rbind(turnover.df, c(TotalSales, TOSales, TOSales/TotalSales))
turnover.df <- rbind(turnover.df, c(TotalRD, TORD, TORD/TotalRD))
colnames(turnover.df) <- c("TotalEmployees", "NumberTurnOver", "PercentTurnOver")                     
rownames(turnover.df) <- c("HR", "Sales", "R&D")

boxplot(EduFieldCode~TurnOver,data=CaseStudy2_data, notch=FALSE, main="", 
        xlab="Turn Over (0=No 1=Yes)", ylab="Education Field (0=HR, 1=Sales, 2=R&D)")
