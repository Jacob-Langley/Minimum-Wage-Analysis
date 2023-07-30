library(dplyr)
library(tidyr)
library(sos)
library(knitr)
library(ggplot2)
library(did)
library(stargazer)



dem1969 <- read.fwf(file = "nc.1969_2020.19ages.txt", widths = c(4,2,5,2,1,1,1,2,8), header = FALSE, sep = ",",
                    skip = 0, col.names = c("year","state","FIPS", "registry", "race", "origin", "sex", "age", "pop"), 
                    buffersize = 2000)

dem18and20 <- subset(dem1969, year == 2018 | year == 2020 , select= c(year,state,FIPS,race, sex, age, pop))

dem18and20m <- subset(dem18and20, sex == 1, select= c(year,state,FIPS,race, sex, age, pop))
dem18and20f <- subset(dem18and20, sex == 2, select= c(year,state,FIPS,race, sex, age, pop))


dem18and20a <- merge(dem18and20m, dem18and20f, by=c("year","FIPS", "state","race","age"), all = T, suffixes = c("ma","fe")) 


dem18and20a[is.na(dem18and20a)] = 0

dem18and20a$tpop <- dem18and20a$popma + dem18and20a$popfe  #create total population column seperated by age race year and FIPS
#dem18and20a <- dem18and20f + dem18and20m

keeps <- c("year","FIPS", "state","race","age", "tpop") 
dem18and20long <- dem18and20a[ , keeps, drop = T]

keeps2 <- c("year","FIPS", "tpop")      #removing gender specific columns
dem18and20b <- dem18and20a[ , keeps2, drop = T]

demsumpop <- aggregate(x = dem18and20b, by = list( dem18and20b$year, dem18and20b$FIPS), FUN = sum)
#add all the tpops by year and FIPS so we have a total population for the county 


keeps3 <- c("Group.1","Group.2", "tpop")
demsumpop <- demsumpop[ , keeps3, drop = T]

colnames(demsumpop) <- c("year", "FIPS", "total_pop")

demsumpop

colSums(is.na(demsumpop))

keeps4 <- c("year","FIPS", "age", "tpop")
demsumpopage <- dem18and20long[ , keeps4, drop = T]

#removing state and race

demsumpopage <- aggregate(x = demsumpopage, by = list( demsumpopage$age, demsumpopage$year, demsumpopage$FIPS), FUN = sum)

keeps5 <- c("Group.1","Group.2", "Group.3", "tpop")
demsumpopage <- demsumpopage[ , keeps5, drop = T]
colnames(demsumpopage) <- c("age_group", "year", "FIPS", "aget")

#correcting column names

#combining with pop totals to get proportions 
demsumpopage
demsumpop

DemSumAgeu <- merge(demsumpopage, demsumpop, by=c("year","FIPS"), all = T) 
DemSumAgeu$ageperc <- DemSumAgeu$aget / DemSumAgeu$total_pop #proportions achieved
#theres a 0.....
DemSumAgeu$age_group <- DemSumAgeu$age_group +1

DemSumAgeu$weighted <- DemSumAgeu$age_group * DemSumAgeu$ageperc #weighted percentaged acheived

#weighted averages....

demsumpopage <- aggregate(x = DemSumAgeu, by = list( DemSumAgeu$year, DemSumAgeu$FIPS), FUN = sum)
#FIPS 37001 in 2018: aget should equal total population of Alamance county 166,516  if all is well

keeps5a <- c("Group.1","Group.2", "aget", "weighted")
Demographics <- demsumpopage[ , keeps5a, drop = T]
colnames(Demographics) <- c("year", "FIPS", "total_pop", "averge_age")

Demographics

keeps6 <- c("year","FIPS", "race", "tpop")
demsumpoprace <- dem18and20long[ , keeps6, drop = T]

DemSumRace <- aggregate(x = demsumpoprace, by = list(demsumpoprace$race, demsumpoprace$year, demsumpoprace$FIPS), FUN = sum)

keeps5 <- c("Group.1","Group.2", "Group.3", "tpop")
DemSumRace <- DemSumRace[ , keeps5, drop = T]
colnames(DemSumRace) <- c("race_group", "year", "FIPS", "racet")



DemSumRaceu <- merge(Demographics, DemSumRace, by=c("year","FIPS"), all = T) 

DemSumRaceu <- subset(DemSumRaceu, race_group == 1)

DemSumRaceu$perc_white <- DemSumRaceu$racet / DemSumRaceu$total_pop

Demographics <- merge(Demographics, DemSumRaceu, by=c("year","FIPS", "total_pop"), all = T)

colnames(Demographics) <- c("year","FIPS", "total_pop", "average_age", "ignore1", "ignore2", "ignore3", "perc_white")

Demographics

keeps7 <- c("year","FIPS", "total_pop", "average_age", "perc_white")
Demographics <- Demographics[ , keeps7, drop = T]

NCdemographics <- Demographics[order(Demographics$FIPS, Demographics$year),]



dem1969 <- read.fwf(file = "va.1969_2020.19ages.txt", widths = c(4,2,5,2,1,1,1,2,8), header = FALSE, sep = ",",
                    skip = 0, col.names = c("year","state","FIPS", "registry", "race", "origin", "sex", "age", "pop"), 
                    buffersize = 2000)

dem18and20 <- subset(dem1969, year == 2018 | year == 2020 , select= c(year,state,FIPS,race, sex, age, pop))

dem18and20m <- subset(dem18and20, sex == 1, select= c(year,state,FIPS,race, sex, age, pop))
dem18and20f <- subset(dem18and20, sex == 2, select= c(year,state,FIPS,race, sex, age, pop))


dem18and20a <- merge(dem18and20m, dem18and20f, by=c("year","FIPS", "state","race","age"), all = T, suffixes = c("ma","fe")) 


dem18and20a[is.na(dem18and20a)] = 0

dem18and20a$tpop <- dem18and20a$popma + dem18and20a$popfe  #create total population column seperated by age race year and FIPS
#dem18and20a <- dem18and20f + dem18and20m

keeps <- c("year","FIPS", "state","race","age", "tpop") 
dem18and20long <- dem18and20a[ , keeps, drop = T]

keeps2 <- c("year","FIPS", "tpop")      #removing gender specific columns
dem18and20b <- dem18and20a[ , keeps2, drop = T]

demsumpop <- aggregate(x = dem18and20b, by = list( dem18and20b$year, dem18and20b$FIPS), FUN = sum)
#add all the tpops by year and FIPS so we have a total population for the county 


keeps3 <- c("Group.1","Group.2", "tpop")
demsumpop <- demsumpop[ , keeps3, drop = T]

colnames(demsumpop) <- c("year", "FIPS", "total_pop")

demsumpop

colSums(is.na(demsumpop))

keeps4 <- c("year","FIPS", "age", "tpop")
demsumpopage <- dem18and20long[ , keeps4, drop = T]

#removing state and race

demsumpopage <- aggregate(x = demsumpopage, by = list( demsumpopage$age, demsumpopage$year, demsumpopage$FIPS), FUN = sum)

keeps5 <- c("Group.1","Group.2", "Group.3", "tpop")
demsumpopage <- demsumpopage[ , keeps5, drop = T]
colnames(demsumpopage) <- c("age_group", "year", "FIPS", "aget")

#correcting column names

#combining with pop totals to get proportions 
demsumpopage
demsumpop

DemSumAgeu <- merge(demsumpopage, demsumpop, by=c("year","FIPS"), all = T) 
DemSumAgeu$ageperc <- DemSumAgeu$aget / DemSumAgeu$total_pop #proportions achieved
#theres a 0.....
DemSumAgeu$age_group <- DemSumAgeu$age_group +1

DemSumAgeu$weighted <- DemSumAgeu$age_group * DemSumAgeu$ageperc #weighted percentaged acheived

#weighted averages....

demsumpopage <- aggregate(x = DemSumAgeu, by = list( DemSumAgeu$year, DemSumAgeu$FIPS), FUN = sum)
#FIPS 37001 in 2018: aget should equal total population of Alamance county 166,516  if all is well

keeps5a <- c("Group.1","Group.2", "aget", "weighted")
Demographics <- demsumpopage[ , keeps5a, drop = T]
colnames(Demographics) <- c("year", "FIPS", "total_pop", "averge_age")

Demographics

keeps6 <- c("year","FIPS", "race", "tpop")
demsumpoprace <- dem18and20long[ , keeps6, drop = T]

DemSumRace <- aggregate(x = demsumpoprace, by = list(demsumpoprace$race, demsumpoprace$year, demsumpoprace$FIPS), FUN = sum)

keeps5 <- c("Group.1","Group.2", "Group.3", "tpop")
DemSumRace <- DemSumRace[ , keeps5, drop = T]
colnames(DemSumRace) <- c("race_group", "year", "FIPS", "racet")



DemSumRaceu <- merge(Demographics, DemSumRace, by=c("year","FIPS"), all = T) 

DemSumRaceu <- subset(DemSumRaceu, race_group == 1)

DemSumRaceu$perc_white <- DemSumRaceu$racet / DemSumRaceu$total_pop

Demographics <- merge(Demographics, DemSumRaceu, by=c("year","FIPS", "total_pop"), all = T)

colnames(Demographics) <- c("year","FIPS", "total_pop", "average_age", "ignore1", "ignore2", "ignore3", "perc_white")

Demographics

keeps7 <- c("year","FIPS", "total_pop", "average_age", "perc_white")
Demographics <- Demographics[ , keeps7, drop = T]

VAdemographics <- Demographics[order(Demographics$FIPS, Demographics$year),]

Demographics <- rbind(NCdemographics,VAdemographics)

Demographics


#now importing size by square miles to get pop density...
LandData <- read.csv("LND01.csv", header= TRUE)


glimpse(LandData)

LandDataNCVA <- subset(LandData, STCOU > 37000 & STCOU < 38000 | STCOU > 51000 & STCOU < 52000)

keeps <- c("ï..Areaname","STCOU", "LND010190D")
LandDataNCVA <- LandDataNCVA[ , keeps, drop = T]

colnames(LandDataNCVA) <- c("county", "FIPS", "Sq_mi")

#Demographics <- merge(Demographics, LandDataNCVA, by=c("FIPS"), all = T)

#Demographics$pop_density <- Demographics$total_pop / Demographics$Sq_mi

Demographics

#now for education attainment....
#header has two lines...
header <- scan("ACSST5Y2020.S1501_data_with_overlays_2022-05-02T141241.csv", nlines = 1, what = character(), sep = ",")
NCeducationData <- read.csv("ACSST5Y2020.S1501_data_with_overlays_2022-05-02T141241.csv", skip = 2, header = FALSE)
names(NCeducationData) <- header

header <- scan("ACSST5Y2020.S1501_data_with_overlays_2022-06-10T122115.csv", nlines = 1, what = character(), sep = ",")
VAeducationData <- read.csv("ACSST5Y2020.S1501_data_with_overlays_2022-06-10T122115.csv", skip = 2, header = FALSE)
names(VAeducationData) <- header

EducationData <- rbind(NCeducationData, VAeducationData)

EducationData <- separate(EducationData, 1, into = c(NA, "FIPS"), sep = "US", remove = T)

#totalling # of graduates
EducationData$uni_gradt <- EducationData$S1501_C01_005E + EducationData$S1501_C01_018E + EducationData$S1501_C01_021E + EducationData$S1501_C01_024E + EducationData$S1501_C01_027E

keeps <- c("FIPS","uni_gradt")
EducationData <- EducationData[ , keeps, drop = T]

EducationData

#Demographics <- merge(Demographics, EducationData, by=c("FIPS"), all = T)

#Demographics$uni_gradrt <- Demographics$uni_gradt / Demographics$total_pop

Demographics

colSums(is.na(Demographics))

#importing employment data for food services industry....

employment2019 <- read.csv("2019 445 Food and beverage stores.csv")
employment2021 <- read.csv("2021 445 Food and beverage stores.csv")

keeps <- c("area_fips","own_code", "year", "qtr", "industry_title", "month1_emplvl", "month2_emplvl", "month3_emplvl")
employment2019 <- employment2019[ , keeps, drop = T]
employment2021 <- employment2021[ , keeps, drop = T]

#getting only the employment data for NC and VA

employment2019NCVA <- subset(employment2019, area_fips > 37000 & area_fips < 38000 | area_fips > 51000 & area_fips < 52000)
employment2021NCVA <- subset(employment2021, area_fips > 37000 & area_fips < 38000 | area_fips > 51000 & area_fips < 52000)
employment2019NCVAQ1 <- subset(employment2019NCVA, qtr == 1 & own_code == 5) #own_code 5 is private institutions
employment2021NCVAQ4 <- subset(employment2021NCVA, qtr == 4 & own_code == 5) 

#treatment dummy: VA 2021 gets a 1
employment2019NCVAQ1$treatment <- 0

employment2021NCVAQ4$treatment <- employment2021NCVAQ4$area_fips

employment2021NCVAQ4$treatment[employment2021NCVAQ4$area_fips < 51000] <- 0
employment2021NCVAQ4$treatment[employment2021NCVAQ4$area_fips > 51000] <- 1

#binary for what state it is: control
employment2019NCVAQ1$control <- employment2019NCVAQ1$area_fips

employment2019NCVAQ1$control[employment2019NCVAQ1$area_fips < 51000] <- 0
employment2019NCVAQ1$control[employment2019NCVAQ1$area_fips > 51000] <- 1

employment2021NCVAQ4$control <- employment2021NCVAQ4$area_fips

employment2021NCVAQ4$control[employment2021NCVAQ4$area_fips < 51000] <- 0
employment2021NCVAQ4$control[employment2021NCVAQ4$area_fips > 51000] <- 1

#binding 2019Q1 & 2021Q4

EmploymentData <- rbind(employment2019NCVAQ1, employment2021NCVAQ4)


EmploymentData$av_qtr_employment  <- rowMeans(subset(EmploymentData, select = c("month1_emplvl", "month2_emplvl", "month3_emplvl")), na.rm = TRUE)




#im noticing a lot of 0s in the data....

EmploymentData$year_dem <- EmploymentData$year - 1

keeps <- c("area_fips", "year_dem", "year", "industry_title","av_qtr_employment", "treatment", "control" )
EmploymentData <- EmploymentData[ , keeps, drop = T]


colnames(EmploymentData) <- c("FIPS", "year", "year_u", "industry_title","av_qtr_employment", "treatment", "control" )

Data <- merge(EmploymentData, Demographics, by = c("FIPS", "year"), all = T )

#putting in the non-year-dependant constants 
Data <- merge(Data, LandDataNCVA, by=c("FIPS"), all = T)
Data <- merge(Data, EducationData, by=c("FIPS"), all = T)

Data$pop_density <- Data$total_pop / Data$Sq_mi
Data$uni_gradrt <- Data$uni_gradt / Data$total_pop





#reorder columns
keeps <- c("county", "FIPS", "year_u", "industry_title","av_qtr_employment", "year", "total_pop", "average_age", "perc_white",  "Sq_mi", "pop_density", "uni_gradt", "uni_gradrt", "treatment", "control" )
Data <- Data[ , keeps, drop = F]

dim(Data)

colSums(Data == 0, na.rm = T)   



n_distinct(Data$FIPS) #264
NonZeroData <- Data[Data$av_qtr_employment != 0,]
n_distinct(NonZeroData$FIPS) #220

#we lose 44 observations if we remove the 0 observations for employment level 

#N, mean, std.dev, min and max 

#findFn("computeEstimate")
NewData <- Data 

NewData$av_qtr_employment[NewData$av_qtr_employment == 0] <- NA

Data$treatment <- as.numeric(unlist(Data$treatment))
NewData$treatment <- as.numeric(unlist(NewData$treatment))






DatNA <- NewData[is.na(NewData$av_qtr_employment),]



find.numeric <- sapply(DatNA, is.numeric)
colMeans(DatNA[, find.numeric], na.rm = T)


NewData <- separate(NewData, county,into = c("county", "state"), sep = ", " )

ggplot(data = NewData, mapping = aes(x=pop_density, y = av_qtr_employment)) + 
  geom_point(aes(colour=state))

ggplot(data = NewData, mapping = aes(x=uni_gradrt, y = av_qtr_employment)) + 
  geom_point(aes(colour=state))

#cleaning data: checking i have all the observations i should have 

DirtyData <- Data 

FIPS <- read.csv("fips_codes.csv", header= T)

dim(FIPS) #235 x2 = 470
dim(DirtyData) #501?? bunch of trash, no wonder i have so many 0s

colSums(DirtyData == 0, na.rm = T)   



DirtyData$FIPS <- as.numeric(unlist(DirtyData$FIPS))


CleanData <- right_join(DirtyData, FIPS, by = "FIPS")

colSums(CleanData == 0, na.rm = T)

colSums(is.na(DirtyData))

colSums(is.na(CleanData))


n_distinct(CleanData$FIPS) #236? 
n_distinct(FIPS$FIPS) #236.
NonZeroData <- CleanData[CleanData$av_qtr_employment != 0,]
n_distinct(NonZeroData$FIPS) #199

#37 0s... 

#nothing i can do about the 37 0s in the data set and i fixed the NA issue by putting the constant data in last...
#so now we can run the regression... DID method..

CleanData$av_qtr_employment[CleanData$av_qtr_employment == 0] <- NA #replacing the data holes with NAs

colSums(CleanData == 0, na.rm = T) # looks great

#need to make a gname column... first year that an observation got treated... 
#i want the period to match when the group would have been treated accurately i think to get the balance right
#so 2019Q1 = p1 so 2020Q4 = p8 so 2021Q4 = p12
#2021 Q1 is when treatment was administered: p9
#so...
CleanedData <- CleanData %>% drop_na(average_age)

colSums(is.na(CleanedData)) #no more NAs, except the holes in my employment data 



#CleanedData$time <- CleanedData$year_u

#CleanedData$time[CleanedData$year_u == 2019] <- 1
#CleanedData$time[CleanedData$year_u == 2021] <- 12

#CleanedData$group <- CleanedData$control


#CleanedData$group[CleanedData$control == 1 & CleanedData$treatment == 0 ] <- 1
#CleanedData$group[CleanedData$control == 1] <- 9

#CleanedData$group <- as.numeric(unlist(CleanedData$group))




#out <- att_gt(yname = "av_qtr_employment", tname = "time", idname = "FIPS", gname = "group", xformla = ~ average_age + perc_white + pop_density + uni_gradrt, data = CleanedData, panel =  F,  print_details = T, est_method = "reg" )
#  summary(out)

#doesnt work for some reason, says "No pre-treatment periods to test" 
#but it does give this
#Group-Time Average Treatment Effects:
#  Group Time  ATT(g,t) Std. Error [95% Pointwise  Conf. Band] 
#    9   12 -393.8754   899.3526       -2017.183    1229.432

#switching tactics... 
#repurposing.... 

CleanedData$time <- CleanedData$year_u


CleanedData$time[CleanedData$year_u == 2019] <- 0
CleanedData$time[CleanedData$year_u == 2021] <- 1

CleanedData$time <- as.numeric(unlist(CleanedData$time))
CleanedData$control <- as.numeric(unlist(CleanedData$control))


CleanedData <- CleanedData[order(CleanedData$FIPS, CleanedData$time),]

CleanedData$did = CleanedData$time * CleanedData$control

colSums(is.na(CleanedData))


didreg = lm(av_qtr_employment ~ time + control + did + average_age + perc_white + pop_density + uni_gradrt, data = CleanedData)

summary(didreg)

all(CleanData$treatment == CleanData$did) #oh 

n_distinct(CleanedData$FIPS) 

#232 - 46 = 186 counties
#186 x2 = 372

n_distinct(FIPS$FIPS) #*2 = 472 so in total im missing 50 counties out of 236


n_distinct(CleanData$FIPS) #236


colSums(employment2019NCVAQ1 == 0, na.rm = F)   #50 0s

colSums(employment2021NCVAQ4 == 0, na.rm = F)   #49 0s 


NCem <- subset(employment2019NCVAQ1, area_fips > 37000 & area_fips < 38000)
VAem <- subset(employment2019NCVAQ1, area_fips > 51000 & area_fips < 52000)
colSums(NCem == 0, na.rm = F)   #16
colSums(VAem == 0, na.rm = F)   #34 
#16 missing from NC, #34 missing from VA, 50 in total 

NCem <- subset(employment2021NCVAQ4, area_fips > 37000 & area_fips < 38000)
VAem <- subset(employment2021NCVAQ4, area_fips > 51000 & area_fips < 52000)
colSums(NCem == 0, na.rm = F)   #15
colSums(VAem == 0, na.rm = F)   #34 
#15 missing from NC, #34 missing from VA, 49 in total 

NCfips <- subset(FIPS, FIPS > 37000 & FIPS < 38000)
VAfips <- subset(FIPS, FIPS > 51000 & FIPS < 52000)

n_distinct(NCfips$FIPS) #100
n_distinct(VAfips$FIPS) #136

100 - 16 # 84 counties from NC
136 - 34 # 102 counties from VA


tab_01 = data.frame(
  Measure = c("Employment", "Average age (grouped)", "Percentage White",  "Population Density", "College Graduation Rate"),
  
  Count =  c(sum(!is.na(CleanedData$av_qtr_employment))/2, sum(!is.na(CleanedData$average_age))/2, sum(!is.na(CleanedData$perc_white))/2, sum(!is.na(CleanedData$pop_density))/2, sum(!is.na(CleanedData$uni_gradrt))/2), 
  
  N =  c(sum(!is.na(CleanedData$av_qtr_employment)), sum(!is.na(CleanedData$average_age)), sum(!is.na(CleanedData$perc_white)), sum(!is.na(CleanedData$pop_density)), sum(!is.na(CleanedData$uni_gradrt))), 
  
  Mean  = c(mean(CleanedData$av_qtr_employment, na.rm = T), mean(CleanedData$average_age, na.rm = T), mean(CleanedData$perc_white, na.rm = T),
            mean(CleanedData$pop_density, na.rm = T), mean(CleanedData$uni_gradrt, na.rm = T)),
  Standard_Deviation = c(sd(CleanedData$av_qtr_employment, na.rm = T), sd(CleanedData$average_age, na.rm = T),
                         sd(CleanedData$perc_white, na.rm = T), sd(CleanedData$pop_density, na.rm = T), sd(CleanedData$uni_gradrt, na.rm = T)),
  Min = c(min(CleanedData$av_qtr_employment, na.rm = T), min(CleanedData$average_age, na.rm = T), min(CleanedData$perc_white, na.rm = T),
          min(CleanedData$pop_density, na.rm = T), min(CleanedData$uni_gradrt, na.rm = T)),
  Max = c(max(CleanedData$av_qtr_employment, na.rm = T), max(CleanedData$average_age, na.rm = T), max(CleanedData$perc_white, na.rm = T),
          max(CleanedData$pop_density, na.rm = T), max(CleanedData$uni_gradrt, na.rm = T)))

tab_01




DatNA <- CleanData[is.na(CleanData$av_qtr_employment),]



find.numeric <- sapply(DatNA, is.numeric)
colMeans(DatNA[, find.numeric], na.rm = T)

tab_02 = data.frame(
  Measure = c("Average age (grouped)", "Percentage White",  "Population Density", "College Graduation Rate"),
  Count =  c(sum(!is.na(DatNA$average_age))/2, sum(!is.na(DatNA$perc_white))/2,
             sum(!is.na(DatNA$pop_density))/2, sum(!is.na(DatNA$uni_gradrt))/2),
  N =  c(sum(!is.na(DatNA$average_age)), sum(!is.na(DatNA$perc_white)),
         sum(!is.na(DatNA$pop_density)), sum(!is.na(DatNA$uni_gradrt))),
  Mean  = c(mean(DatNA$average_age, na.rm = T), mean(DatNA$perc_white, na.rm = T),
            mean(DatNA$pop_density, na.rm = T), mean(DatNA$uni_gradrt, na.rm = T)),
  Standard_Deviation = c(sd(DatNA$average_age, na.rm = T),
                         sd(DatNA$perc_white, na.rm = T), sd(DatNA$pop_density, na.rm = T), sd(DatNA$uni_gradrt, na.rm = T)),
  Min = c(min(DatNA$average_age, na.rm = T), min(DatNA$perc_white, na.rm = T),
          min(DatNA$pop_density, na.rm = T), min(DatNA$uni_gradrt, na.rm = T)),
  Max = c(max(DatNA$average_age, na.rm = T), max(DatNA$perc_white, na.rm = T),
          max(DatNA$pop_density, na.rm = T), max(DatNA$uni_gradrt, na.rm = T)))

CleanData$bias <- CleanData$av_qtr_employment
CleanData$bias[CleanData$bias > 0] <- "Observed"
CleanData$bias <- replace_na(CleanData$bias, "Missing" )

CleanNC <- subset(CleanData, State == "NC")
CleanVA <- subset(CleanData, State == "VA")

didreg2 = lm(av_qtr_employment ~ time + control + did + average_age + perc_white + total_pop  + uni_gradt, data = CleanedData)


regreg = lm(av_qtr_employment ~ control + average_age + perc_white + pop_density + uni_gradrt, data = CleanedData)



kable(
  tab_01,
  col.names = c("Measure", "#", "N", "Mean", "Standard Deviation", "Min", "Max"),
  digits = 2,
  caption = "Descriptive Statistics of Minimum Wage DID Study of Virgina & North Carolina ($n=372$)"
)


kable(
  tab_02,
  col.names = c("Measure", "#", "N", "Mean", "Standard Deviation", "Min", "Max"),
  digits = 2,
  caption = "Means of Variables Assigned as 0s in the Employment Level Dataset ($n=92$)")

#comparing the states to each other

ggplot(CleanData, aes(x = average_age, fill = State)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =State)) +
  labs(x = "  Average Age 
       (9 = 40-44, 10 = 45-49)")
#similar-ish


ggplot(CleanData, aes(x = perc_white, fill = State)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =State)) +
  labs(x = "Percentage race = white")
#similar

ggplot(CleanData, aes(x = log(pop_density), fill = State)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =State))  +
  labs(x = "log(Population Density)")
#VA has lower pop density 


ggplot(CleanData, aes(x = log(uni_gradrt), fill = State)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =State)) +
  labs(x = "log(College Graduation Rate)")
#NC has a lower graduation rate 

ggplot(CleanData, aes(x = log(av_qtr_employment), fill = State)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =State)) +
  labs(x = "log(Employment Level)")
#VA has lower employment overall 

#comparing the missing data to the observed data 

ggplot(CleanNC, aes(x = average_age, fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "  Average Age (grouped)", title = "North Carolina") 
#older

ggplot(CleanVA, aes(x = average_age, fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "  Average Age (grouped)", title = "Virgina")
#slightly older

ggplot(CleanNC, aes(x = perc_white, fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "Percentage race = white", title = "North Carolina")
#less white, more white



ggplot(CleanVA, aes(x = perc_white, fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "Percentage race = white", title = "Virgina")
#similar

ggplot(CleanNC, aes(x = log(pop_density), fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "log(Population Density)", title = "North Carolina")
#less dense 


ggplot(CleanVA, aes(x = log(pop_density), fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias)) +
  labs(x = "log(Population Density)", title = "Virgina")
#less dense 

ggplot(CleanNC, aes(x = log(uni_gradrt), fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias))+
  labs(x = "log(College Graduation Rate)", title = "North Carolina")
#??

ggplot(CleanVA, aes(x = log(uni_gradrt), fill = bias)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +
  geom_density(alpha=.2, aes(color =bias))+
  labs(x = "log(College Graduation Rate)", title = "Virgina")
#less educated


stargazer(didreg, didreg2, regreg, type = "latex", 
          title = "DID and OLS Estimates of the Effect of Minimum Wage on the Employment Level in VA", 
          header=FALSE, covariate.labels = 
            c("Time (dummy)","Control (dummy)", "Treatment (dummy)", "Average Age (grouped)", "Percentage White", "Population Density", "College Graduation Rate", "Total Population", "Total College Graduations"))

