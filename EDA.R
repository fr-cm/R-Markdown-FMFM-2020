#Exploratory analysis
# clean the workspace
rm(list=ls())
#Loading data

library(tidyverse)
library(DescTools)
library(gridExtra)
library(mice)
library(hexbin)
library(RColorBrewer)
library(ggplot2)
library(tibble)

data <- read.csv("C:/Users/fccom/Documents/R/FMFM 2020 - R Data story telling/Data_Seminar_2022/FMFM2019_seminar.csv")

frame <- data[ , c("Enterprise.Flag", "Purpose.of.Loan","Borrower.Gender", "Co.Borrower.Gender", "Age.of.Borrower", "Age.of.Co.Borrower", "Rate.Spread", "Interest.Rate.at.Origination", "Automated.Underwriting.System..AUS.","Credit.Score.Model...Borrower", "Debt.to.Income..DTI..Ratio", "Property.Value", "First.Time.Home.Buyer", "Acquisition.Unpaid.Principal.Balance..UPB.", "Federal.Guarantee", "Borrower.Income.Ratio", "Borrower.Race1", "High.Opportunity.Area", "Borrower.s..or.Borrowers...Annual.Income", "Loan.to.Value.Ratio..LTV.")] 
frame$Loan.to.Value.Ratio..LTV.[frame$Loan.to.Value.Ratio..LTV. == 999.00] <- NA
frame[frame$Purpose.of.Loan == 9] <- NA
frame$Borrower.Gender[frame$Borrower.Gender == 9] <- NA
frame$Borrower.Gender[frame$Borrower.Gender == 4] <- NA
frame$Borrower.Gender[frame$Borrower.Gender == 3] <- NA
frame$Co.Borrower.Gender[frame$Co.Borrower.Gender == 9] <- NA
frame$Co.Borrower.Gender[frame$Co.Borrower.Gender == 4] <- NA
frame$Age.of.Borrower[frame$Age.of.Borrower == 9] <- NA
frame$Age.of.Co.Borrower[frame$Age.of.Co.Borrower == 9] <- NA
frame$Rate.Spread[frame$Rate.Spread == 0] <- NA
frame$Interest.Rate.at.Origination[frame$Interest.Rate.at.Origination == 99.00] <- NA
frame$Automated.Underwriting.System..AUS.[frame$Automated.Underwriting.System..AUS. == 9] <- NA
frame$Credit.Score.Model...Borrower[frame$Credit.Score.Model...Borrower == 9] <- NA
frame$Credit.Score.Model...Borrower[frame$Credit.Score.Model...Borrower == 99] <- NA
frame$Debt.to.Income..DTI..Ratio[frame$Debt.to.Income..DTI..Ratio == 99] <- NA
frame$Property.Value[frame$Property.Value == 999999999] <- NA
frame$First.Time.Home.Buyer[frame$First.Time.Home.Buyer == 9] <- NA
frame$Acquisition.Unpaid.Principal.Balance..UPB.[frame$Acquisition.Unpaid.Principal.Balance..UPB. == 999999999] <- NA
frame$Borrower.Income.Ratio[frame$Borrower.Income.Ratio == 9999.000] <- NA
frame$Borrower.Race1[frame$Borrower.Race1 == 9] <- NA
frame$Borrower.Race1[frame$Borrower.Race1 == 7] <- NA
frame$Borrower.Race1[frame$Borrower.Race1 == 6] <- NA
frame$Borrower.s..or.Borrowers...Annual.Income[frame$Borrower.s..or.Borrowers...Annual.Income == 999999999] <- NA
frame$Rate.Spread[frame$Borrower.Race1 == 0] <- NA

Bg_1 <- frame[c(frame$Borrower.Gender ==1), ] 
Bg_2 <- frame[c(frame$Borrower.Gender ==2), ] 

Ent_1 <- frame[c(frame$Enterprise.Flag ==1), ] 
Ent_2 <- frame[c(frame$Enterprise.Flag ==2), ] 

Indian  <- frame[c(frame$Borrower.Race1 ==1), ] 
Asian <- frame[c(frame$Borrower.Race1 ==2), ] 
AfroAmerican <- frame[c(frame$Borrower.Race1 ==3), ] 
White  <- frame[c(frame$Borrower.Race1 ==5), ]

IndianM  <- Indian[c(Indian$Borrower.Gender==1), ]
AsianM <- Asian[c(Asian$Borrower.Gender==1), ]
AfroAmericanM <- AfroAmerican[c(AfroAmerican$Borrower.Gender==1), ]
WhiteM  <- White[c(White$Borrower.Gender==1), ]

IndianF  <- Indian[c(Indian$Borrower.Gender==2), ]
AsianF <- Asian[c(Asian$Borrower.Gender==2), ]
AfroAmericanF <- AfroAmerican[c(AfroAmerican$Borrower.Gender==2), ]
WhiteF  <- White[c(White$Borrower.Gender==2), ]

IndianM_A <- IndianM[c(IndianM$Enterprise.Flag ==1), ] 
IndianM_B <- IndianM[c(IndianM$Enterprise.Flag ==2), ]

IndianF_A <- IndianF[c(IndianF$Enterprise.Flag ==1), ] 
IndianF_B <- IndianF[c(IndianF$Enterprise.Flag ==2), ]

AsianM_A <- AsianM[c(AsianM$Enterprise.Flag ==1), ]
AsianM_B <- AsianM[c(AsianM$Enterprise.Flag ==2), ]

AsianF_A <- AsianF[c(AsianF$Enterprise.Flag ==1), ]
AsianF_B <- AsianF[c(AsianF$Enterprise.Flag ==2), ]

WhiteM_A <- WhiteM[c(WhiteM$Enterprise.Flag == 1), ] 
WhiteM_B <- WhiteM[c(WhiteM$Enterprise.Flag == 2), ] 

WhiteF_A <- WhiteF[c(WhiteF$Enterprise.Flag == 1), ] 
WhiteF_B <- WhiteF[c(WhiteF$Enterprise.Flag == 2), ]

AfroAmericanM_A <- AfroAmericanM[c(AfroAmericanM$Enterprise.Flag == 1), ]
AfroAmericanM_B <- AfroAmericanM[c(AfroAmericanM$Enterprise.Flag == 2), ]

AfroAmericanF_A <- AfroAmericanF[c(AfroAmericanF$Enterprise.Flag == 1), ]
AfroAmericanF_B <- AfroAmericanF[c(AfroAmericanF$Enterprise.Flag == 2), ]


#amalysus

Desc(Bg_1$Rate.Spread)
Desc(Bg_2$Rate.Spread)

#analysis - int.rate

#
Desc(IndianM_A$Interest.Rate.at.Origination)
Desc(IndianM_B$Interest.Rate.at.Origination)
Desc(IndianF_A$Interest.Rate.at.Origination) #difference 0,04
Desc(IndianF_B$Interest.Rate.at.Origination) #2nd enterprise lower
  
Desc(AsianM_A$Interest.Rate.at.Origination)
Desc(AsianM_B$Interest.Rate.at.Origination)
Desc(AsianF_A$Interest.Rate.at.Origination)  #female higher
Desc(AsianF_B$Interest.Rate.at.Origination)
  
Desc(WhiteM_A$Interest.Rate.at.Origination)
Desc(WhiteM_B$Interest.Rate.at.Origination)  
Desc(WhiteF_A$Interest.Rate.at.Origination)  #female higher
Desc(WhiteF_B$Interest.Rate.at.Origination)   #white higher than chinese


Desc(AfroAmericanM_A$Interest.Rate.at.Origination)    
Desc(AfroAmericanM_B$Interest.Rate.at.Origination)  
Desc(AfroAmericanF_A$Interest.Rate.at.Origination)  
Desc(AfroAmericanF_B$Interest.Rate.at.Origination)   #equals but high

Desc(AfroAmerican$Borrower.Gender)
Desc(White$Borrower.Gender)

#analysis - property value

Desc(IndianM_A$Property.Value)
Desc(IndianM_B$Property.Value)
Desc(IndianF_A$Property.Value)
Desc(IndianF_B$Property.Value)   #difference in B, and B higher value in general 

Desc(AsianM_A$Property.Value)
Desc(AsianM_B$Property.Value)
Desc(AsianF_A$Property.Value)
Desc(AsianF_B$Property.Value) #B lower, gender difference in both

Desc(WhiteM_A$Property.Value)
Desc(WhiteM_B$Property.Value)  
Desc(WhiteF_A$Property.Value)  
Desc(WhiteF_B$Property.Value) #greater gender difference, B lower values

Desc(AfroAmericanM_A$Property.Value)   
Desc(AfroAmericanM_B$Property.Value)
Desc(AfroAmericanF_A$Property.Value) 
Desc(AfroAmericanF_B$Property.Value)

############
Desc(data$High.Opportunity.Area+WhiteF_B$Property.Value, data=data)

Desc(WhiteM_A$High.Opportunity.Area)
Desc(WhiteF_A$High.Opportunity.Area)


###########
       
#analysis - age
       
Desc(IndianM_A$Age.of.Borrower)
Desc(IndianM_B$Age.of.Borrower)
Desc(IndianF_A$Age.of.Borrower)
Desc(IndianF_B$Age.of.Borrower)
                                 
Desc(ChineseM_A$Age.of.Borrower)
Desc(ChineseM_B$Age.of.Borrower)
Desc(ChineseF_A$Age.of.Borrower)
Desc(ChineseF_B$Age.of.Borrower)
                                                               
Desc(WhiteM_A$Age.of.Borrower)
Desc(WhiteM_B$Age.of.Borrower)  
Desc(WhiteF_A$Age.of.Borrower)  
Desc(WhiteF_B$Age.of.Borrower)
                                                                                           
Desc(AfroAmericanM_A$Age.of.Borrower)   
Desc(AfroAmericanM_B$Age.of.Borrower)
Desc(AfroAmericanF_A$Age.of.Borrower) 
Desc(AfroAmericanF_B$Age.of.Borrower)       

#analysis Rate.Spread
       
Desc(IndianM_A$Rate.Spread)
Desc(IndianM_B$Rate.Spread)
Desc(IndianF_A$Rate.Spread)
Desc(IndianF_B$Rate.Spread)

Desc(AsianM_A$Rate.Spread) 
Desc(AsianM_B$Rate.Spread)
Desc(AsianF_A$Rate.Spread)
Desc(AsianF_B$Rate.Spread)
#asian pay less but with gender gap 
                                   
Desc(WhiteM_A$Rate.Spread)
Desc(WhiteM_B$Rate.Spread)  
Desc(WhiteF_A$Rate.Spread)  
Desc(WhiteF_B$Rate.Spread)

#white pay more but with gender gap                              
                                                              
Desc(AfroAmericanM_A$Rate.Spread)   
Desc(AfroAmericanM_B$Rate.Spread)
Desc(AfroAmericanF_A$Rate.Spread) 
Desc(AfroAmericanF_B$Rate.Spread)     

#black pay more thean other but in equal way disparity is in the enterprice 2 where the man pay more the woman 
       
#analysis
       
Desc(IndianM_A$Borrower?..s..or.Borrowers?....Annual.Income) #103'867.20
Desc(IndianM_B$Borrower?..s..or.Borrowers?....Annual.Income) #114'645.47
Desc(IndianF_A$Borrower?..s..or.Borrowers?....Annual.Income) #93'309.86
Desc(IndianF_B$Borrower?..s..or.Borrowers?....Annual.Income) #99'181.91
                                   
Desc(AsianM_A$Borrower?..s..or.Borrowers?....Annual.Income) #214'325.907059
Desc(AsianM_B$Borrower?..s..or.Borrowers?....Annual.Income) #179'053.061316
Desc(AsianF_A$Borrower?..s..or.Borrowers?....Annual.Income) #120'930.78
Desc(AsianF_B$Borrower?..s..or.Borrowers?....Annual.Income) #117'527.14
                                                               
Desc(WhiteM_A$Borrower?..s..or.Borrowers?....Annual.Income) #122'400.52
Desc(WhiteM_B$Borrower?..s..or.Borrowers?....Annual.Income) #145'887.555095 
Desc(WhiteF_A$Borrower?..s..or.Borrowers?....Annual.Income) #105'985.232129 
Desc(WhiteF_B$Borrower?..s..or.Borrowers?....Annual.Income) #128'585.305265
                                                                                           
Desc(AfroAmericanM_A$Borrower?..s..or.Borrowers?....Annual.Income) #108'308.45  
Desc(AfroAmericanM_B$Borrower?..s..or.Borrowers?....Annual.Income) #226'317.678359
Desc(AfroAmericanF_A$Borrower?..s..or.Borrowers?....Annual.Income) #88'793.86
Desc(AfroAmericanF_B$Borrower?..s..or.Borrowers?....Annual.Income) #93'758.03    

#Boxplot race - int. rate
boxplot(Asian$Interest.Rate.at.Origination,White$Interest.Rate.at.Origination,Indian$Interest.Rate.at.Origination, Black$Interest.Rate.at.Origination, ylab= "Int. rate at origination", names = c("Asian","White","Indian","Black"))

#Boxplot race - int. rate
boxplot(Asian$Property.Value,White$Property.Value,Indian$Property.Value, Black$Property.Value, ylab= "Property value", names = c("Asian","White","Indian","Black"))

#Boxplot race - int. rate
boxplot(Bg_1$Property.Value,Bg_2$Property.Value, ylab= "Property value", names = c("Male","Female"))

Desc(frame$Borrower.Gender)

Gen <- 612838
Gen_2 <- 307552
barplot(Gen, Gen_2)


Desc(Bg_1$Property.Value)
Desc(Bg_2$Property.Value)
prop_gen <- c(379574.08,342510.19)
barplot(prop_gen, ylab= "Property value", names = c("Male","Female"))

install.packages("ggplot2")                                 
library("ggplot2")  

Desc(Indian$Interest.Rate.at.Origination)
Desc(Chinese$Interest.Rate.at.Origination)
Desc(AfroAmerican$Interest.Rate.at.Origination)
Desc(White$Interest.Rate.at.Origination)


Desc(frame$Acquisition.Unpaid.Principal.Balance..UPB.)

Desc(frame ~ frame$Rate.Spread)

Desc(Pur_1$Property.Value)
Desc(Pur_2$Property.Value)
Desc(Pur_4$Property.Value)
Desc(Pur_7$Property.Value)

boxplot(Bg_1$Property.Value, Bg_2$Property.Value)

Amale <- Ent_1[c(Ent_1$Borrower.Gender==1), ]
Afemale <- Ent_1[c(Ent_1$Borrower.Gender==2), ] 
Bmale <- Ent_2[c(Ent_1$Borrower.Gender==1), ]
Bfemale <- Ent_2[c(Ent_1$Borrower.Gender==2), ] 

boxplot(Amale$Interest.Rate.at.Origination, Afemale$Interest.Rate.at.Origination, Bmale$Interest.Rate.at.Origination, Bfemale$Interest.Rate.at.Origination)

#Int. rate

Desc(IndianM$Interest.Rate.at.Origination)
Desc(IndianF$Interest.Rate.at.Origination)

Desc(AsianM$Interest.Rate.at.Origination)
Desc(AsianF$Interest.Rate.at.Origination)

Desc(WhiteM$Interest.Rate.at.Origination)
Desc(WhiteF$Interest.Rate.at.Origination)

Desc(AfroAmericanM$Interest.Rate.at.Origination)
Desc(AfroAmericanF$Interest.Rate.at.Origination)



Desc(IndianM$Loan.to.Value.Ratio..LTV.)
Desc(IndianF$Loan.to.Value.Ratio..LTV.)

Desc(AsianM$Loan.to.Value.Ratio..LTV.)
Desc(AsianF$Loan.to.Value.Ratio..LTV.)

Desc(WhiteM$Loan.to.Value.Ratio..LTV.)
Desc(WhiteF$Loan.to.Value.Ratio..LTV.)

Desc(AfroAmericanM$Loan.to.Value.Ratio..LTV.)
Desc(AfroAmericanF$Loan.to.Value.Ratio..LTV.)

# Age borrower
  
Desc(IndianM$Age.of.Borrower)
Desc(IndianF$Age.of.Borrower)
                
Desc(ChineseM$Age.of.Borrower)
Desc(ChineseF$Age.of.Borrower)
                              
Desc(WhiteM$Age.of.Borrower)
Desc(WhiteF$Age.of.Borrower)
                                            
Desc(AfroAmericanM$Age.of.Borrower)
Desc(AfroAmericanF$Age.of.Borrower)
       
#AfroAmerican m b
1.5
18.3    
27.0  
23.6  
18.0   
9.0   
2.6  

#AfroAmerican f b
1.3
17.8   
23.9   
22.8   
19.9  
11.1   
3.2

under_25 <- c(1.5,1.3)
from_25_to_34 <-c(18.3,17.8)
from_35_to_44 <- c(27,23.9)
from_45_to_54<-c(23.6,22.8)
from_55_to_64 <-c(18,19.9)
from_65_to_74<- c(9,11,1)
over_74<-c(2.6,3.2)

tab_black_B <- cbind(under_25,
                      from_25_to_34,
                      from_35_to_44,
                      from_45_to_54,
                      from_55_to_64,
                      from_65_to_74,
                      over_74)
                     
view(tab_black_B)
barplot(tab_black_B)
tab_black_B[-c(3,1), ]


Desc(data$High.Opportunity.Area)

Desc(AfroAmerican$Borrower?..s..or.Borrowers?....Annual.Income)
Desc(White$Borrower?..s..or.Borrowers?....Annual.Income)
Desc(Chinese$Borrower?..s..or.Borrowers?....Annual.Income)
Desc(Indian$Borrower?..s..or.Borrowers?....Annual.Income)

Desc(AfroAmerican$First.Time.Home.Buyer)
Desc(White$First.Time.Home.Buyer)
Desc(Chinese$First.Time.Home.Buyer)
Desc(Indian$First.Time.Home.Buyer)

Desc(AfroAmericanM_A$Rate.Spread)
Desc(AfroAmericanF_A$Rate.Spread)

Desc(AfroAmericanM_B$Rate.Spread)
Desc(AfroAmericanF_B$Rate.Spread)

Desc(White$Rate.Spread)


view(data$Rate.Spread)

#loan to value ratio

Desc(IndianM_A$US.Postal.State.Code)
Desc(IndianM_B$US.Postal.State.Code)
Desc(IndianF_A$US.Postal.State.Code)
Desc(IndianF_B$US.Postal.State.Code)

Desc(AsianM_A$US.Postal.State.Code) 
Desc(AsianM_B$US.Postal.State.Code)
Desc(AsianF_A$US.Postal.State.Code)
Desc(AsianF_B$US.Postal.State.Code)

Desc(WhiteM_A$US.Postal.State.Code)
Desc(WhiteM_B$US.Postal.State.Code)  
Desc(WhiteF_A$US.Postal.State.Code)  
Desc(WhiteF_B$US.Postal.State.Code)

Desc(AfroAmericanM_A$US.Postal.State.Code)   
Desc(AfroAmericanM_B$US.Postal.State.Code)
Desc(AfroAmericanF_A$US.Postal.State.Code) 
Desc(AfroAmericanF_B$US.Postal.State.Code)

#loan int rate

Desc(IndianM_A$Interest.Rate.at.Origination)
Desc(IndianM_B$Interest.Rate.at.Origination)
Desc(IndianF_A$Interest.Rate.at.Origination)
Desc(IndianF_B$Interest.Rate.at.Origination)

Desc(AsianM_A$Interest.Rate.at.Origination) 
Desc(AsianM_B$Interest.Rate.at.Origination)
Desc(AsianF_A$Interest.Rate.at.Origination)
Desc(AsianF_B$Interest.Rate.at.Origination)

Desc(WhiteM_A$Interest.Rate.at.Origination)
Desc(WhiteM_B$Interest.Rate.at.Origination)  
Desc(WhiteF_A$Interest.Rate.at.Origination)  
Desc(WhiteF_B$Interest.Rate.at.Origination)

Desc(AfroAmericanM_A$Interest.Rate.at.Origination)   
Desc(AfroAmericanM_B$Interest.Rate.at.Origination)
Desc(AfroAmericanF_A$Interest.Rate.at.Origination) 
Desc(AfroAmericanF_B$Interest.Rate.at.Origination)

#Plots

#
AfroAmerican_rate <- c(4.46, 4.49, 4.42, 4.44)
white_rate <- c(4.24, 4.28, 4.23, 4.27)
asian_rate <- c(4.07,4.16, 4.09, 4.16)
indian_rate <- c(4.33, 4.3736, 4.26, 4.275)


boxplot(WhiteM_A$Interest.Rate.at.Origination, WhiteF_A$Interest.Rate.at.Origination,WhiteM_B$Interest.Rate.at.Origination, WhiteF_B$Interest.Rate.at.Origination)
boxplot(AfroAmericanM_A$Interest.Rate.at.Origination, AfroAmericanF_A$Interest.Rate.at.Origination,AfroAmericanM_B$Interest.Rate.at.Origination, AfroAmericanF_B$Interest.Rate.at.Origination)
boxplot(WhiteM_A$Interest.Rate.at.Origination, WhiteF_A$Interest.Rate.at.Origination,WhiteM_B$Interest.Rate.at.Origination, WhiteF_B$Interest.Rate.at.Origination)
boxplot(WhiteM_A$Interest.Rate.at.Origination, WhiteF_A$Interest.Rate.at.Origination,WhiteM_B$Interest.Rate.at.Origination, WhiteF_B$Interest.Rate.at.Origination)

Desc(WhiteM_A$Property.Value ~ WhiteM_A$Interest.Rate.at.Origination)

WhiteM %>%
  ggplot(aes(x=Interest.Rate.at.Origination,y=Borrower?..s..or.Borrowers?....Annual.Income)) +
  geom_point(alpha=0.5) +
  labs(x= "Rate", y="Income")+
  ylim(0, 600000)


Desc(WhiteM$Borrower?..s..or.Borrowers?....Annual.Income)

Whitt <- 

#annual income

AfroAmerican_male_A <- 108308.45 
AfroAmerican_male_B <- 226317.678359
AfroAmerican_female_A <- 88793.86
AfroAmerican_female_B <- 93758.03 
  
White_male_A <- 122400.52
White_male_B <- 145887.555095
White_female_A <- 105985.232129 
White_female_B <- 128585.305265
  
Chinese_male_A <- 214325.907059
Chinese_male_B <- 179053.061316
Chinese_female_A <- 120930.78
Chinese_female_B <- 117527.14
  
Indian_male_A <-  103867.20
Indian_male_B <- 114645.47
Indian_female_A <- 93309.86 
Indian_female_B <- 99181.91


   
AfroAmerican_income <- c(108308.45,88793.86,226317.678359,93758.03)
White_income <- c(122400.52,105985.232129, 145887.555095,128585.305265)
Asian_income <- c(214325.907059,120930.78,179053.061316,117527.14)
Indian_income <- c(103867.20,93309.86,114645.47,99181.91)

par(mfrow=c(2,2))
barplot(Black_income, axes=FALSE, col= c("blue", "red","blue", "red"),main="Black",names.arg= "Freddie MAE                                                     Freddie MAC")
barplot(White_income, axes=FALSE, col= c("blue", "red","blue", "red"),main="White",names.arg= "Freddie MAE                                                     Freddie MAC")
barplot(Asian_income, axes=FALSE, col= c("blue", "red","blue", "red"),main="Asian",names.arg= "Freddie MAE                                                     Freddie MAC")
barplot(Indian_income, axes=FALSE, col= c("blue", "red","blue", "red"),main="Indian",names.arg= "Freddie MAE                                                     Freddie MAC")

#loan to value value

AfroAmerican_ratio <- c(80.83, 81.27, 79.57, 79.109)
white_ratio <- c(75.96, 75.89, 75.77, 75.59)
asian_ratio <- c(73.63, 71.91, 74.145, 72.899)
indian_ratio <- c(77.21, 76.30, 76.020, 75.4469)

par(mfrow=c(2,2))
barplot(AfroAmerican_ratio, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Black",names.arg= "Freddie MAE               Freddie MAC")
barplot(white_ratio, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="White",names.arg= "Freddie MAE               Freddie MAC")
barplot(asian_ratio, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Asian",names.arg= "Freddie MAE               Freddie MAC")
barplot(indian_ratio, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Indian",names.arg= "Freddie MAE              Freddie MAC")

  
#property value 
  
AfroAmerican_value <- c(326141.82,299510.02,322383.45,304785.88)
White_value <- c(371893.19, 335318.68,364044.81,327433.15)
Asian_value <- c(496999.69,478612.97,480289.37,461793.75)
indian_value <- c(334207.14, 328151.41, 347879.05,323338.69)
  
par(mfrow=c(2,2))
barplot(AfroAmerican_value, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Black")
barplot(White_value, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="White")
barplot(Asian_value, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Asian")
barplot(indian_value, axes=FALSE,horiz = TRUE, col= c("blue", "red","blue", "red"),main="Indian")


n=10000; dt=0.1; h=sqrt(dt)
X=3:n; Y=3:n
X[1]=-10; Y[1]=-10
for (t in 1:(n-1)) {
  X[t+1]=X[t]+h*rnorm(1)
  Y[t+1]=Y[t]+h*rnorm(1)
}
plot(X,Y,type="l", col=1); abline(h=0); abline(0,1000)

n=10000; dt=0.01; h=sqrt(dt)
X=1:n; Y=1:n
X[1]=0; Y[1]=0
plot(c(-10,10),c(-10,10))
abline(h=0)
abline(0,1000)
for (t in 1:(n-1)) {
  X[t+1]=X[t]+h*rnorm(1)
  Y[t+1]=Y[t]+h*rnorm(1)
  lines(X[1:t],Y[1:t],type="2", col=3)
}
..........................................................
n=10000; dt=0.01; h=sqrt(dt)
X=1:n; Y=1:n
X[1]=0; Y[1]=0

rio:
  drift=function(x) -x
n=10000; dt=0.01; h=sqrt(dt); sig=1
X=1:n; X[1]=1
for (t in 1:(n-1)) {X[t+1]=X[t]+ dt*drift(X[t]) + h*sig*rnorm(1)}
plot(X,type="l", col=2); abline(h=0); abline(0,1000)


  drift=function(x) x-x^3
n=10000; dt=0.01; h=sqrt(dt); sig=0.5
X=1:n; X[1]=1
for (t in 1:(n-1)) {X[t+1]=X[t]+ dt*drift(X[t]) + h*sig*rnorm(1)}
plot(X,type="l", col=4); abline(h=0); abline(0,1000)


n=10000; dt=0.01; h=sqrt(dt)
X=1:n; Y=1:n
X[1]=0; Y[1]=0
for (t in 1:(n-1)) {
  X[t+1]=X[t]+h*rnorm(1)
  Y[t+1]=Y[t]+h*rnorm(1)
}
plot(X,Y,type="l", col=3); abline(h=0); abline(0,1000)


n=1000; N=1000; dt=0.001; h=sqrt(dt)
X=matrix(nrow=N,ncol=n); Y=X
X[,1]=rnorm(N,0,1); Y[,1]=rnorm(N,0,1)
for (t in 1:(n-1)) {
  X[,t+1]=X[,t]+h*rnorm(N)
  Y[,t+1]=Y[,t]+h*rnorm(N)
  plot(c(-10,10),c(-10,10))
  lines(X[,t+1],Y[,t+1],type="p", col=1)
  abline(h=0)
  abline(0,1000)
}

