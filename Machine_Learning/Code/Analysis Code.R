#examining the effects of Virginia's minimum wage hike in May of 2021 
#on the employment level in the state using a difference in differences regression
#using North Carolina as a control
#and logistic and lasso regression for variable selection guidance 

library(dplyr)
library(tidyr)
library(glmnet)
library(knitr)
library(stargazer)
library(gridExtra)
library(grid)
library(ggplot2)


#importing employment data for food services industry first

all_employment.2019 <- read.csv("Data/Employment/2019.q1-q4 445 NAICS 445 Food and beverage stores.csv")
all_employment.2021 <- read.csv("Data/Employment/2021.q1-q4 445 NAICS 445 Food and beverage stores.csv")
all_employment.2022 <- read.csv("Data/Employment/2022.q1-q1 445 NAICS 445 Food and beverage retailers.csv")

keeps <- c("area_fips","own_code", "year", "qtr", "industry_title", "month1_emplvl", "month2_emplvl", "month3_emplvl")
all_employment.2019 <- all_employment.2019[ , keeps, drop = T]
all_employment.2021 <- all_employment.2021[ , keeps, drop = T]
all_employment.2022 <- all_employment.2022[ , keeps, drop = T]

#getting only the employment data for NC and VA



employment2019 <- subset(all_employment.2019, area_fips > 37000 & area_fips < 38000 | area_fips > 51000 & area_fips < 52000)
employment2021 <- subset(all_employment.2021, area_fips > 37000 & area_fips < 38000 | area_fips > 51000 & area_fips < 52000)
employment2022 <- subset(all_employment.2022, area_fips > 37000 & area_fips < 38000 | area_fips > 51000 & area_fips < 52000)

employment <- rbind(employment2019, employment2021)
employment22 <- rbind(employment2019, employment2022)

#own_code 5 is private institutions
employment <- subset(employment, year == 2019 & qtr == 1 & own_code == 5 | year == 2021 & qtr == 4 & own_code == 5)
employment22 <- subset(employment22, year == 2019 & qtr == 1 & own_code == 5 | year == 2022 & qtr == 1 & own_code == 5)



#getting the average employment level for the quarter
employment$av_qtr_employment  <- round(rowMeans(subset(employment, select = c("month1_emplvl", "month2_emplvl", "month3_emplvl")), na.rm = TRUE), 1)
employment22$av_qtr_employment  <- round(rowMeans(subset(employment22, select = c("month1_emplvl", "month2_emplvl", "month3_emplvl")), na.rm = TRUE), 1)


colnames(employment)[c(1,3)] <- c('FIPS', 'YEAR')
colnames(employment22)[c(1,3)] <- c('FIPS', 'YEAR')






#demographics


VA_demographics.2010_2019 <- read.csv("Data/Demographics/cc-est2019-alldata-51.csv", header= TRUE)
VA_demographics.2020_2021 <- read.csv("Data/Demographics/cc-est2021-alldata-51.csv", header= TRUE)
NC_demographics.2010_2019 <- read.csv("Data/Demographics/cc-est2019-alldata-37.csv", header= TRUE)
NC_demographics.2020_2021 <- read.csv("Data/Demographics/cc-est2021-alldata-37.csv", header= TRUE)

all_demographics.2010_2019 <- rbind(VA_demographics.2010_2019,NC_demographics.2010_2019)
all_demographics.2020_2021 <- rbind(VA_demographics.2020_2021,NC_demographics.2020_2021)

#variable vector

all_demographics.vector <- c(
  "STATE", "COUNTY","STNAME", "CTYNAME", "YEAR", 
  "AGEGRP", "TOT_POP", "TOT_FEMALE", "WAC_MALE", "WAC_FEMALE", "BAC_MALE", "BAC_FEMALE", "H_MALE", "H_FEMALE")

#we only need data from 2021 & 2019

all_demographics.2019 <- subset(all_demographics.2010_2019, YEAR == 12, select = all_demographics.vector)
all_demographics.2021 <- subset(all_demographics.2020_2021, YEAR ==  3, select = all_demographics.vector)

all_demographics.2019$YEAR <- 2019
all_demographics.2021$YEAR <- 2021

demographics_both <- rbind(all_demographics.2019, all_demographics.2021)

nCOUNTY <- sprintf("%03d",demographics_both$COUNTY)
demographics_both <- cbind(demographics_both,nCOUNTY)

demographics_both$FIPS <- paste(demographics_both$STATE, demographics_both$nCOUNTY, sep = '')

demographics <- subset(demographics_both, AGEGRP == 0)


#3 race dummies
demographics$WHITE <- demographics$WAC_MALE + demographics$WAC_FEMALE
demographics$BLACK <- demographics$BAC_MALE + demographics$BAC_FEMALE
demographics$HISP <- demographics$H_MALE + demographics$H_FEMALE



keeps1 <- c(
  "FIPS","STNAME", "CTYNAME", "YEAR", 
  "TOT_POP", "TOT_FEMALE", "WHITE", "BLACK", "HISP")

demographics <- demographics[ , keeps1, drop = T]

demographics_ages <- subset(demographics_both, AGEGRP != 0)

demographics_ages <- demographics_ages %>% 
  group_by(FIPS, YEAR) %>%
  mutate(TOTAL = sum(TOT_POP))

#check to see that total = total pop

check_age_totals <- subset(demographics_ages, AGEGRP == 1)

all(demographics$TOT_POP == check_age_totals$TOTAL) #TRUE

demographics_ages$proportion <- demographics_ages$TOT_POP/demographics_ages$TOTAL
demographics_ages$AAGE <- demographics_ages$AGEGRP*demographics_ages$proportion

age_weights <- aggregate(AAGE ~ FIPS + YEAR, demographics_ages, sum)

age_weights$AAGE <- round(age_weights$AAGE, 2)

demographics <- merge(demographics, age_weights, by = c('FIPS', 'YEAR'))

#now we have all the totals and average age 
#next is to get the percentages

demographics$GENDER <- round(demographics$TOT_FEMALE / demographics$TOT_POP, 3)
demographics$PWHITE <- round(demographics$WHITE / demographics$TOT_POP, 3)
demographics$PBLACK <- round(demographics$BLACK / demographics$TOT_POP, 3)
demographics$PHISP <- round(demographics$HISP / demographics$TOT_POP,3)


# checking the correlations for informed set-up of the regression: 

cor(demographics[, c('AAGE','GENDER','PWHITE','PBLACK','PHISP')])

#PWHITE and PBLACK are too highly correlated to include them both in a regression. 
#multicollinearity would throw off the interpretation


keeps2 <- c(
  "FIPS","YEAR", "STNAME", "CTYNAME", 
  "TOT_POP", "AAGE", "GENDER", "PWHITE", "PBLACK", "PHISP")

demographics <- demographics[ , keeps2, drop = T]



#now importing size by square miles to get pop density...
all_LandData <- read.csv("Data/Land/LND01.csv", header= TRUE)


glimpse(all_LandData)

LandData <- subset(all_LandData, STCOU > 37000 & STCOU < 38000 | STCOU > 51000 & STCOU < 52000)

keeps <- c("Areaname","STCOU", "LND010190D")
LandData <- LandData[ , keeps, drop = T]

colnames(LandData) <- c("county", "FIPS", "Sq_mi")

#leave that there for a second

#moving on to educational attainment
#has two lines

header <- scan("Data/Education/ACSST5Y2020.S1501_data_with_overlays_2022-05-02T141241.csv", nlines = 1, what = character(), sep = ",")
edu.NC <- read.csv("Data/Education/ACSST5Y2020.S1501_data_with_overlays_2022-05-02T141241.csv", skip = 2, header = FALSE)
names(edu.NC) <- header

header <- scan("Data/Education/ACSST5Y2020.S1501_data_with_overlays_2022-06-10T122115.csv", nlines = 1, what = character(), sep = ",")
edu.VA <- read.csv("Data/Education/ACSST5Y2020.S1501_data_with_overlays_2022-06-10T122115.csv", skip = 2, header = FALSE)
names(edu.VA) <- header

education <- rbind(edu.NC, edu.VA)

education <- separate(education, 1, into = c(NA, "FIPS"), sep = "US", remove = T)

#totalling # of graduates
education$uni_gradt <- education$S1501_C01_005E + education$S1501_C01_018E + education$S1501_C01_021E + education$S1501_C01_024E + education$S1501_C01_027E

keeps <- c("FIPS","uni_gradt")
education <- education[ , keeps, drop = T]



#the population total educational attainment is divided by should be an average of 2019 and 2021 data
#or 2020 total pop data

education.05 <- merge(education, subset(demographics, YEAR == 2019)[,c("FIPS", "TOT_POP")], by = "FIPS")

education.1 <- merge(education.05, subset(demographics, YEAR == 2021)[,c("FIPS", "TOT_POP")], by = "FIPS")

education$pop_av <- (education.1$TOT_POP.x + education.1$TOT_POP.y)/2


#combine data
Data <- merge(employment, demographics, by = c("FIPS", "YEAR"), all = T )

demographics22 <- demographics

demographics22$YEAR[demographics22$YEAR == 2021] <- 2022

#note I am assigning 2021 demographic data for 2022 just as a filler 
#because I dont have real 2022 data yet

Data22 <- merge(employment22, demographics22, by = c("FIPS", "YEAR"), all = T )





#putting in the non-year-dependent constants 
Data <- merge(Data, LandData, by=c("FIPS"), all = T)
Data <- merge(Data, education, by=c("FIPS"), all = T)

Data$pop_density <- round(Data$TOT_POP / Data$Sq_mi,1)
Data$uni_gradrt <- round(Data$uni_gradt / Data$pop_av,3)

Data22 <- merge(Data22, LandData, by=c("FIPS"), all = T)
Data22 <- merge(Data22, education, by=c("FIPS"), all = T)

Data22$pop_density <- round(Data22$TOT_POP / Data22$Sq_mi, 1)
Data22$uni_gradrt <- round(Data22$uni_gradt / Data22$pop_av, 3)


#DID variables to be added after cleaning

#cleaning

colSums(Data == 0, na.rm = T)   #months are all the same

#keeping only the columns i need
keeps <- c("FIPS","YEAR", "qtr", "av_qtr_employment", 
           "STNAME", "CTYNAME", "TOT_POP", "AAGE", 
           "GENDER", "PWHITE", "PBLACK", "PHISP", 
           "pop_density", "uni_gradrt")
Data <- Data[ , keeps, drop = T]

Data$av_qtr_employment[Data$av_qtr_employment == 0] <- NA


FIPS <- read.csv("Data/FIPS/2019_Virginia_Census_Counties___County_Equivalents.csv", header= T)

keeps <- c("GEOID")
FIPS <- FIPS[ , keeps, drop = T]

"The Commonwealth of Virginia is divided into 95 counties, 
along with 38 independent cities that are considered 
county-equivalents for census purposes."

"The U.S. state of North Carolina is divided into 100 counties."

95 + 38 + 100 #233

n_distinct(Data$FIPS) #238
#5 counties too many

FIPS <- c(seq.int(37001, 37199, by = 2), FIPS)
n_distinct(FIPS) #233
#now I have a checklist for all counties in both states: FIPS

#turn decimals into percentages for easier interpretation 
Data$PWHITE <- Data$PWHITE*100
Data22$PWHITE <- Data22$PWHITE*100

Data$PBLACK <- Data$PBLACK*100
Data22$PBLACK <- Data22$PBLACK*100

Data$PHISP <- Data$PHISP*100
Data22$PHISP <- Data22$PHISP*100

Data$uni_gradrt <- Data$uni_gradrt*100
Data22$uni_gradrt <- Data22$uni_gradrt*100



#checking our county checklist against the counties we have 
setdiff(Data$FIPS, FIPS) #"37999" "51515" "51560" "51780" "51999"

#37999, 51999
#these arnt real fips codes in the states and its not clear what theyre referring to
#dropping them

#51515
#one observation, NAs across the board
#"Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into
#Bedford County (FIPS 51019)." - FIPS County Code Changes 2021.doc

#51560
#one observation, NAs across the board
#"Virginia, 2001: The independent city of Clifton Forge (FIPS 51560) merges
#into Alleghany county (FIPS 51005)." - FIPS County Code Changes 2021.doc

#51780
#one observation, NAs across the board
#"Virginia, 1995: The independent city of South Boston (FIPS 51780)
#merges into Halifax county (FIPS 51083)." - FIPS County Code Changes 2021.doc

Data.clean <-Data[!(Data$FIPS=="37999" | Data$FIPS=="51999" | Data$FIPS=="51515" | Data$FIPS=="51560" | Data$FIPS=="51780" ),]

dim(Data.clean) #466
n_distinct(Data.clean$FIPS) #233 
#good

colSums(Data.clean == 0, na.rm = T) #0
colSums(is.na(Data.clean)) #no NAs except for the dependent variable  

Data.missing <- subset(Data, is.na(av_qtr_employment))

#missing 97 employment observations

sum(Data.missing$FIPS > 51000) #63 from VA
sum(Data.missing$FIPS < 38000) #34 of them are from NC



n_distinct(Data.missing$FIPS) #60 missing counties out of 233
n_distinct(Data.missing$FIPS[Data.missing$FIPS > 51000]) #39 from VA
n_distinct(Data.missing$FIPS[Data.missing$FIPS < 38000]) #21 from NC

n_distinct(Data.missing$FIPS[Data.missing$YEAR == 2019]) #44 from 2019
n_distinct(Data.missing$FIPS[Data.missing$YEAR == 2021]) #53 from 2021

dim(Data22)
dim(Data)



##same process with Data22



colSums(Data22 == 0, na.rm = T)   #months are all the same

#keeping only the columns i need
keeps <- c("FIPS","YEAR", "qtr", "av_qtr_employment", 
           "STNAME", "CTYNAME", "TOT_POP", "AAGE", 
           "GENDER", "PWHITE", "PBLACK", "PHISP", 
           "pop_density", "uni_gradrt")
Data22 <- Data22[ , keeps, drop = T]

Data22$av_qtr_employment[Data22$av_qtr_employment == 0] <- NA




setdiff(Data22$FIPS, FIPS) #"37999" "51515" "51560" "51780" "51999"

#37999, 51999
#these arnt real fips codes and its not clear what theyre referring to
#dropping them

#51515
#one observation, NAs across the board
#"Virginia, 2013: The independent city of Bedford (FIPS 51515) merges into
#Bedford County (FIPS 51019)." - FIPS County Code Changes 2021.doc

#51560
#one observation, NAs across the board
#"Virginia, 2001: The independent city of Clifton Forge (FIPS 51560) merges
#into Alleghany county (FIPS 51005)." - FIPS County Code Changes 2021.doc

#51780
#one observation, NAs across the board
#"Virginia, 1995: The independent city of South Boston (FIPS 51780)
#merges into Halifax county (FIPS 51083)." - FIPS County Code Changes 2021.doc

Data22.clean <-Data22[!(Data22$FIPS=="37999" | Data22$FIPS=="51999" | Data22$FIPS=="51515" | Data22$FIPS=="51560" | Data22$FIPS=="51780" ),]

colSums(Data22.clean == 0, na.rm = T) #0
colSums(is.na(Data22.clean)) #0 

Data22.missing <- subset(Data22.clean, is.na(av_qtr_employment))
#missing 86  employment observations
sum(Data22.missing$FIPS > 51000) #54 from VA
sum(Data22.missing$FIPS < 38000) #32 of them are from NC



n_distinct(Data22.missing$FIPS) #54 missing counties out of 233
n_distinct(Data22.missing$FIPS[Data22.missing$FIPS > 51000]) #35 from VA
n_distinct(Data22.missing$FIPS[Data22.missing$FIPS < 38000]) #19 from NC


n_distinct(Data22.missing$FIPS[Data22.missing$YEAR == 2019]) #44 from 2019
n_distinct(Data22.missing$FIPS[Data22.missing$YEAR == 2022]) #42 from 2022
#2022 is missing less than 2021 so that's nice



#adding DID variables

#control: 
#0 for control: NC
#1 for target: VA
Data.clean$Control[Data.clean$FIPS > 51000] <- 1
Data.clean$Control[Data.clean$FIPS < 38000] <- 0

Data22.clean$Control[Data22.clean$FIPS > 51000] <- 1
Data22.clean$Control[Data22.clean$FIPS < 38000] <- 0

#time
#0 for before
#1 for after
Data.clean$Time[Data.clean$YEAR == 2019] <- 0
Data.clean$Time[Data.clean$YEAR != 2019] <- 1

Data22.clean$Time[Data22.clean$YEAR == 2019] <- 0
Data22.clean$Time[Data22.clean$YEAR != 2019] <- 1

#treatment term
Data.clean$Treatment = Data.clean$Control * Data.clean$Time

Data22.clean$Treatment = Data22.clean$Control * Data22.clean$Time


cor(Data.clean[, c('AAGE','GENDER','PWHITE', 'PBLACK', 'PHISP', 'pop_density', 'uni_gradrt', 'Control', 'Time', 'Treatment')])


Data.observed <- drop_na(Data.clean)

lm.original.w =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + GENDER + PWHITE + PHISP + pop_density + uni_gradrt, data = Data.clean)
lm.original.w.22 = lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + GENDER + PWHITE + PHISP + pop_density + uni_gradrt, data = Data22.clean)

lm.original.b =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + GENDER + PBLACK + PHISP + pop_density + uni_gradrt, data = Data.clean)
lm.original.b.22 = lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + GENDER + PBLACK + PHISP + pop_density + uni_gradrt, data = Data22.clean)

summary(lm.original.w)
summary(lm.original.w.22)

summary(lm.original.b)
summary(lm.original.b.22)
#treatment is still not significant in 2022





#logistic regression 
#attempting to quantify how different the states from each other 
#to see if NC is a valid control for VA



#binary classification already exists with the control column



Logistic.b <- glm(Control ~ AAGE + GENDER + PBLACK + PHISP + pop_density + uni_gradrt, data = Data.clean, family = binomial)

Logistic.b.22 <- glm(Control ~ AAGE + GENDER + PBLACK + PHISP + pop_density + uni_gradrt, data = Data22.clean, family = binomial)

Logistic.w <- glm(Control ~ AAGE + GENDER + PWHITE + PHISP + pop_density + uni_gradrt, data = Data.clean, family = binomial)

Logistic.w.22 <- glm(Control ~ AAGE + GENDER + PWHITE + PHISP + pop_density + uni_gradrt, data = Data22.clean, family = binomial)

Log.probs <- predict(Logistic.w, type = "response")
Log.probs.22 <- predict(Logistic.w.22, type = "response")

Log.probs[1:10]

Logistic.prediction <- rep("NC", dim(Data.clean)[1])
Logistic.prediction.22 <- rep("NC.pred", dim(Data22.clean)[1])

Logistic.prediction[Log.probs > 0.5] = "VA"
Logistic.prediction.22[Log.probs.22 > 0.5] = "VA.pred"

summary(Logistic.w)
summary(Logistic.w.22)



#confusion matrix
CM <- table(Logistic.prediction, Data.clean$STNAME)
CM
CM.22 <- table(Logistic.prediction.22, Data22.clean$STNAME)
CM.22

(CM[2,1] + CM[1,2]) / dim(Data.clean)[1]
(CM.22[2,1] + CM.22[1,2]) / dim(Data.clean)[1]


fourfoldplot(CM, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Logistic Confusion Matrix")

#0.2274678 when PBLACK is included
#0.2296137 when PWHITE is included

#PBLACK is not significant when included
#PWHITE is significant at the 10% level
#implying neither are very good



#23% error rate
#not great but also not terrible
#so theyre fairly dissimilar 



#Lasso



x <- Data.observed[, c("Time" , "Control" , "Treatment" , "AAGE" , "GENDER" , "PWHITE", "PHISP", "pop_density" , "uni_gradrt" )]
y <- Data.observed$av_qtr_employment

lasso.fit <- glmnet(x, y, alpha = 1) # alpha = 1 for Lasso


coef(lasso.fit)
coef(lasso.fit, s = 51 )


lassos<- as.matrix(coef(lasso.fit))

log.lambda <- log(lasso.fit$lambda)

lassos <- rbind(lassos,lasso.fit$lambda )
lassos <- rbind(lassos,log.lambda )
#now we have a cute little matrix to pick out our lambda



row.names(lassos)[c(11,12)] <- c("lambda", "log.lambda")










#i can see a few clusters that drop out together

lassos.t <- t(lassos)

lassos.t <- as.data.frame(lassos.t)




lambdas <- min((lassos.t[lassos.t$Time == 0, ])$log.lambda)

#making a vector of the lambdas & names

options <- data.frame(row.names = c(names(lassos.t[, !names(lassos.t) %in% c( "(Intercept)", "lambda", "log.lambda")]), "PBLACK"))


for (x in names(lassos.t[, !names(lassos.t) %in% c("Time", "(Intercept)", "lambda", "log.lambda")])){
  
  
  
  lambdas <- append(lambdas, min((lassos.t[lassos.t[x] == 0, ])$log.lambda))
  
}




#another one without gender for a more readable graph

x.2 <- Data.observed[, c("Time" , "Control" , "Treatment" , "AAGE" , "PWHITE", "PHISP", "pop_density" , "uni_gradrt" )]
y.2 <- Data.observed$av_qtr_employment

x.3 <- Data.observed[, c("Time" , "Control" , "Treatment" , "AAGE" , "PBLACK", "PHISP", "pop_density" , "uni_gradrt" )]
y.3 <- Data.observed$av_qtr_employment

lasso.fit.2 <- glmnet(x.2, y.2 , alpha = 1)
lasso.fit.3 <- glmnet(x.3, y.3 , alpha = 1)






coef(lasso.fit.2)
coef(lasso.fit.2, s = 51 )


lassos.2 <- as.matrix(coef(lasso.fit.2))

log.lambda.2 <- log(lasso.fit.2$lambda)

lassos.2 <- rbind(lassos,lasso.fit.2$lambda )
lassos.2 <- rbind(lassos,log.lambda.2 )



row.names(lassos.2)[c(10,11)] <- c("lambda", "log.lambda")

#lambdas

lassos.3 <- as.matrix(coef(lasso.fit.3))

log.lambda.3 <- log(lasso.fit.3$lambda)

lassos.3 <- rbind(lassos.3,lasso.fit.3$lambda )
lassos.3 <- rbind(lassos.3,log.lambda.3 )


row.names(lassos.3)[c(10,11)] <- c("lambda", "log.lambda")

lassos.t.3 <- t(lassos.3)
lassos.t.3 <- as.data.frame(lassos.t.3)

#appending PBLACK value to lambdas
lambdas <- append(lambdas, min((lassos.t.3[lassos.t.3$PBLACK == 0, ])$log.lambda))

options <- cbind(options, lambdas)



idea <- c(1:10)
options <- cbind(options, idea)



options.sorted <-(options)[order(lambdas),]
options.sorted<- options.sorted[-2]


#PBLACK Lasso


#in order:
# options
#Treatment drops first which is important 
# Treatment 1.921979
# GENDER 3.317486
# Time 3.596587


# pop_density 4.433890 #surprising pop density isnt more important 

# PBLACK 4.806025
# PHISP 5.271194
# Control 5.643329
# AAGE 6.294565
# uni_gradrt 6.666700





#since we cant include both PWHITE & PBLACK lets do it again, w PBLACK, w/o PWHITE

# Treatment 1.921979
# GENDER 3.317486
# Time 3.596587

# pop_density 4.526924

# PWHITE 5.271194
# PHISP 5.271194
# Control 5.643329
# AAGE 6.294565
# uni_gradrt 6.666700

#why is PWHITE & PHISP the same lambda?

#no change in PHISP lambda, but PBLACK lambda < PWHITE lambda
#implying it is a worse predictor of the employment level
#so in the final regression we will use PWHITE since the 
#lasso is implying it is a stronger predictor

#a lambda of 5 makes sense due to the large gap and the variables deselected.
#could make an argument to include pop density, however it is the only 
#significant variable this method is suggesting we drop so lets do it 
#since the whole goal of lasso regression is to narrow the focus 

lbs_fun <- function(lasso.fit, ...) {
  L <- length(lasso.fit$lambda)
  x <- options.sorted$lambdas
  xp <- c(-.3, -.3, -.15, -.4, 0,   -.2, -.2, -.2, -.15, 0)
  y <- c(0,0,0,0,0,-100,-150,0,0, 0)
  labs <- rownames(options.sorted)
  text(x + xp, -55+ y, labels=labs, cex = .5, ...)
  y.2 <- lasso.fit$beta[, L]
  labs.2 <- names(y.2)
  legend('bottomleft', legend=labs.2, col=1:6, lty=1, cex = .5)
}


plot(lasso.fit, xvar="lambda" )
abline(v = options$lambdas)
lbs_fun(lasso.fit)
title("Lasso Regression Plot",  line = 3)




plot(lasso.fit.2, xvar="lambda")
abline(v = options$lambdas)
lbs_fun(lasso.fit.2)
title("Lasso Plot with Percentage White (zoomed in: dropped gender)", line = 3)

plot(lasso.fit.3, xvar="lambda")
abline(v = options$lambdas)
lbs_fun(lasso.fit.3)
title("Lasso Plot with Percentage Black (zoomed in: dropped gender)", line = 3)


round(coef(lm.original.w), 3)



lm.lassoed =  lm(av_qtr_employment ~ Control + AAGE + PWHITE + PHISP  + uni_gradrt, data = Data.observed)

summary(lm.original.w)
summary(lm.original.b)
summary(lm.lassoed)

#a glaring disagreement between the traditional LM and the lassoed model:
#the original trad model gave a low significance level to PHISP (p < 10)
#but the lasso kept it in the top 5 
#and then rerunning it after dropping the dropped variables 
#shifted even more significance away from it


lm.test =  lm(av_qtr_employment ~ Time + GENDER + AAGE + PWHITE + PHISP  + uni_gradrt, data = Data.observed)

summary(lm.test)

#removing control and including pop_density shifts significance back on to phisp... 
#so PHISP helps the model differentiate between NC & VA employment levels? 
#and so with control in the model its much less important

#ran lasso without PHISP:


# lambdas
# Treatment 1.735912
# GENDER 3.410519
# Time 3.689621
# pop_density 4.433890
# PWHITE 5.271194
# Control 5.643329
# AAGE 6.294565
# uni_gradrt 6.666700

#control is unchanged. 

#this implies that the differences in both PHISP and population density 
#are virtually solely explained by the differences between the two states
#(i.e. Control)


#rerunning the logistic regression solely on those two variables vs the rest?



Logistic.Hisp.Pop <- glm(Control ~ PHISP + pop_density, data = Data.observed, family = binomial)
Logistic.Else <- glm(Control ~ AAGE + GENDER + PWHITE + uni_gradrt, data = Data.observed, family = binomial)


Hisp.Pop.probs <- predict(Logistic.Hisp.Pop, type = "response")
Else.probs <- predict(Logistic.Else, type = "response")


Hisp.Pop.prediction <- rep("NC", dim(Data.observed)[1])
Else.prediction <- rep("NC", dim(Data.observed)[1])

Hisp.Pop.prediction[Hisp.Pop.probs  > 0.5] = "VA"
Else.prediction[Else.probs > 0.5] = "VA"

summary(Logistic.Hisp.Pop)
summary(Logistic.Else)


#confusion matrix
Hisp.Pop <- table(Hisp.Pop.prediction, Data.observed$STNAME)
Hisp.Pop 

Else <- table(Else.prediction, Data.observed$STNAME)
Else


(Else[2,1] + Else[1,2]) / dim(Data.observed)[1] #0.3841202
(Hisp.Pop[2,1] + Hisp.Pop[1,2]) / dim(Data.observed)[1] #0.2660944


fourfoldplot(Hisp.Pop, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Hisp and Pop Density")

fourfoldplot(Else, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "All Other Demographics")

#as i expected just relying on Hisp and POP_den 
#tells the difference between the two states better than the other four combined
#with the graduation rate being the next most useful (?)
#this implies the better regression should drop HISP and pop_density as variables 
#since they are captured by the control variable

lm.filtered =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + PWHITE + uni_gradrt, data = Data.observed)
lm2.filtered =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + PBLACK + uni_gradrt, data = Data.observed)

lm.filtered.22 =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + PWHITE + uni_gradrt, data = Data22.clean)

lm =    lm(av_qtr_employment ~ Time + Control + Treatment + AAGE + GENDER + PWHITE + PHISP + pop_density + uni_gradrt, data = Data.observed)


summary(lm.original.w)
summary(lm.original.b)
summary(lm)
summary(lm.filtered)
summary(lm2.filtered) 
#as the lasso suggested the PBLACK falls beneath the .05 significance level
#implying PWHITE is a better determiner
summary(lm.filtered.22)



#the difference between the states should be captured in Control
#leaving time treatment & gender differences as having highly non-significant effects 
#on the employment level



#variables left that have a theoretically causal effect on the employment level 
#among Food and beverage retailers:

#Average age: 
#for every 5 years the average age of the population is older, 
#the employment level among Food and beverage retailers decreases by 470
#working out to be 94 per average year older

#Percent White:
#for each percentage point higher a county is white the employment among 
#Food and beverage retailers level decreases by 10

#percent of the county population with a college degree:
#for every percentage point higher of people with a college degree
#Food and beverage retailer employment in the county increases by 89

#State difference:
#employment among Food and beverage retailers in Virginia 
#lowers by 664 when compared to North Carolina

#the goal was to examine the effect of Virginia's minimum wage increase 
#in May of 2021:

#The Treatment variable is not significant in both 2021 and 2022 
#which tells us the increase did not have an effect on the employment level

#the p value for treatment deceased from 0.83317 in 2021 data to 0.79520 in 2022 which is 
#still highly non-significant.

#a large caveat to the data is possible selection bias: 
#the counties that are missing from each state may not be reporting their data 
#to the Bureau of Labor Statistics because they dont have the resources necessary 
#for data collection. Meaning the missing counties may be more rural & lower income 
#and may therefore be more adversely effected by a raise in the minimum wage. 

#another possible issue is a lag effect over time: the possibility the minimum wage
#would not effect the employment level for a few years 
#however naively looking at the significance level difference between 2021 and 2022
#suggests to me that there be an effect that is extremely slow and may be overtaken by 
#inflation anyway over time? further follow up studies would need to be done on this 

#interestingly there was an additional minimum wage increase in Virginia to $11 on Jan 1 2022
#this does not cause an issue because both increases can still be considered a
#singular treatment since they both happened between the snapshot dates 
#if anything the effect on employment would just have been made more dramatic

#An additional issue was to address the differences between the control group NC and the
#target: VA. Logistic and Lasso regression were used to select the best variables that 
#described the differences between employment and not just the differences between the states.
#that being said, finding a more similar control would improve the study

#comparing the states to each other

ggplot(Data.clean, aes(x = AAGE, fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "  Average Age 
       (9 = 40-44, 10 = 45-49)")



ggplot(Data.clean, aes(x = GENDER, fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "Gender (1 = all women)")

ggplot(Data.clean, aes(x = PWHITE, fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "Percent White")



ggplot(Data.clean, aes(x = PBLACK, fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "Percent Black")

ggplot(Data.clean, aes(x = PHISP, fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME))  +
  labs(x = "Percent Hisp/Latino")


ggplot(Data.clean, aes(x = log(pop_density), fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME))  +
  labs(x = "log(Population Density)")

ggplot(Data.clean, aes(x = log(uni_gradrt), fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "log(College Graduation Rate)")


ggplot(Data.observed, aes(x = log(av_qtr_employment), fill = STNAME)) + 
  geom_histogram(alpha = 0.5, aes(y=after_stat(density))) +
  geom_density(alpha=.2, aes(color =STNAME)) +
  labs(x = "log(Employment Level)")



