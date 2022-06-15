
### load packages ###
library(readxl)
library(tidyr)
library(tidymodels)
library(readr)
library(dplyr)
library(reshape)
library(reshape2)
library(fastDummies)
library(writexl)
library(ggplot2)
library(lapply)
library(DescTools)
library(tidyverse)

######################################
### import data ###
######################################

setwd("~/Documents/Thesis/Data")
Data_scope3_statistics <- read_excel("~/Documents/Thesis/Data/IndustryCompanies.xlsx", 
                             skip = 1)
Data_scope3_timeseries <- read_excel("~/Documents/Thesis/Data/TimeseriesCompanies.xlsx", 
                                     sheet = "All_var_ts_cleaned", skip = 3)
IncomeGroup <- read_excel("~/Documents/Thesis/Data/IncomeGroup.xlsx", sheet = "Country Analytical History") 
CO2law_ntl <- read_excel("~/Documents/Thesis/Data/CO2law.xlsx", 
                                     sheet = "National")
CO2law_rgl <- read_excel("~/Documents/Thesis/Data/CO2law.xlsx", 
                                     sheet = "Regional")
                              
######################################
### data pre-processing time series ###
######################################

# separate columns
Data_scope3_timeseries <- separate(Data_scope3_timeseries, Name, c('Company', 'Variable'), sep = "-")
Data_scope3_timeseries <- separate(Data_scope3_timeseries, Code, c('Code', 'Variablecode'))

# fill columns company and code
Data_scope3_timeseries <- Data_scope3_timeseries %>% fill(Company)
Data_scope3_timeseries <- Data_scope3_timeseries %>% fill(Code)

# fill Variables names
Variables <- rep(c('CO2e Indirect Emissions Scope 3', 'CO2 Equivalents Emission Indirect', 'CO2 Equivalents Emission Direct',
                   'Net sales/revenue', 'Employees', 'Total Assets', 'Net PPE', 'Intangible Assets', 'Long Term debt',
                   'Gross Profit Margin', 'COGS', 'Raw Materials', 'Capital Expenditures', 'Total Capital',
                   'Depreciation', 'Acc. Depreciation', 'Total investments', 'Other investments', 'Gross PPE', 'Foreign income % total income'), times = 2931)
Data_scope3_timeseries <- cbind(Data_scope3_timeseries, Variables)

my_seq <- 1:2931
Number <- rep(my_seq, each = 20)
Data_scope3_timeseries <- cbind(Data_scope3_timeseries, Number)

# delete unnecessary columns
Data_scope3_timeseries$Variablecode <- NULL
Data_scope3_timeseries$Variable <- NULL
Data_scope3_timeseries$Company <- NULL
# reshape 
Data_scope3_melt <- melt(Data_scope3_timeseries, id=c("Number","Variables", "Code"), variable.name = 'Year')
Data_scope3 <- dcast(Data_scope3_melt, Number + Year + Code ~ Variables)

summary(Data_scope3_timeseries)

# Convert character into numeric values
Data_scope3$'Scope3' <- as.numeric(Data_scope3$'CO2e Indirect Emissions Scope 3')
Data_scope3$'Scope2' <- as.numeric(Data_scope3$'CO2 Equivalents Emission Indirect')
Data_scope3$'Scope1' <- as.numeric(Data_scope3$'CO2 Equivalents Emission Direct')
Data_scope3$'Revenue' <- as.numeric(Data_scope3$'Net sales/revenue')
Data_scope3$'Employees' <- as.numeric(Data_scope3$'Employees')
Data_scope3$'TotalAssets' <- as.numeric(Data_scope3$'Total Assets')
Data_scope3$'NetPPE' <- as.numeric(Data_scope3$'Net PPE')
Data_scope3$'IntangAssets' <- as.numeric(Data_scope3$'Intangible Assets')
Data_scope3$'LTdebt' <- as.numeric(Data_scope3$'Long Term debt')
Data_scope3$'GPMargin' <- as.numeric(Data_scope3$'Gross Profit Margin')
Data_scope3$'COGS' <- as.numeric(Data_scope3$'COGS')
Data_scope3$'RawMat' <- as.numeric(Data_scope3$'Raw Materials')
Data_scope3$'CapExp' <- as.numeric(Data_scope3$'Capital Expenditures')
Data_scope3$'TotCap' <- as.numeric(Data_scope3$'Total Capital')
Data_scope3$'Depreciation' <- as.numeric(Data_scope3$'Depreciation')
Data_scope3$'AccDepr' <- as.numeric(Data_scope3$'Acc. Depreciation')
Data_scope3$'Investments' <- as.numeric(Data_scope3$'Total investments')
Data_scope3$'OtherInvestments' <- as.numeric(Data_scope3$'Other investments')
Data_scope3$'GrossPPE' <- as.numeric(Data_scope3$'Gross PPE')
Data_scope3$'Foreign income % total income' <- as.numeric(Data_scope3$'Foreign income % total income')

summary(Data_scope3)

# subset all final predictors
Data_scope3 <- Data_scope3[c('Number', 'Code', 'Year', 'Scope3','Scope2', 'Scope1', 'Revenue',
                                'Employees', 'TotalAssets','NetPPE', 'IntangAssets', 'LTdebt', 'GPMargin',
                                'CapExp', 'GrossPPE')]
summary(Data_scope3_sub)



######################################
### 1. Merging Statistics/industry ###
######################################

# change column name in order to merge dataset
Data_scope3_statistics$Code <- Data_scope3_statistics$Type
Data_scope3_statistics$Type <- NULL
Data_scope3_statistics$Country <- Data_scope3_statistics$'CTRY OF DOM -NAME'
Data_scope3_statistics$'CTRY OF DOM -NAME' <- NULL

# merge statistics with timeseries
Data_scope3_merged1 <- merge(Data_scope3,Data_scope3_statistics ,by="Code")

######################################
### 2. Merging Incomegroup ###
######################################

# convert Korea variable
IncomeGroup['104', 'Country'] <- 'north korea'
IncomeGroup['105', 'Country'] <- 'south korea'
IncomeGroup['66', 'Country'] <- 'faroe islands'

# reshape IncomeGroup
IncomeGroup$...1 <- NULL
IncomeGroup <- melt(IncomeGroup, id=c("Country"), variable.name = 'Year', value.name = 'IncomeGroup')

# Delete NA and '..' Incomegroup
IncomeGroup <- 
  IncomeGroup %>% drop_na(IncomeGroup)
IncomeGroup <- subset(IncomeGroup, IncomeGroup != "..")

# Merge with IncomeGroup
Data_scope3_merged2 <- Data_scope3_merged1 %>% mutate_each(funs(tolower), 'Country')
IncomeGroup <- IncomeGroup %>% mutate_each(funs(tolower), 'Country')

# Data_scope3_merged2$'IncomeGroup' <- as.numeric(Data_scope3_merged2$'IncomeGroup')
Data_scope3_merged2 <- merge(Data_scope3_merged2,IncomeGroup ,by= c("Country", "Year"), all.x =TRUE)

summary(Data_scope3_merged2)

######################################
###  3. Merging CO2 law ###
######################################

# convert inconvenient variables into simular ones
CO2law_ntl['237', 'DS Country'] <- 'united states'
CO2law_rgl['237', 'DS Country'] <- 'united states'
CO2law_ntl['119', 'DS Country'] <- 'south korea'
CO2law_rgl['119', 'DS Country'] <- 'south korea'
CO2law_ntl['185', 'DS Country'] <- 'russian federation'
CO2law_rgl['185', 'DS Country'] <- 'russian federation'

# reshape CO2 law
CO2law_ntl$ISO2 <- NULL
CO2law_rgl$ISO2 <- NULL
CO2law_ntl$`CO2 Law Ntl. (2020)` <- NULL
CO2law_rgl$`CO2 Law Rgl. (2020)` <- NULL
CO2law_ntl$Country <- CO2law_ntl$'DS Country'
CO2law_ntl$'DS Country' <- NULL
CO2law_rgl$Country <- CO2law_rgl$'DS Country'
CO2law_rgl$'DS Country' <- NULL

CO2law_ntl <- melt(CO2law_ntl, id=c("Country"), variable.name = 'Year', value.name = 'CO2law_ntl')
CO2law_rgl <- melt(CO2law_rgl, id=c("Country"), variable.name = 'Year', value.name = 'CO2law_rgl')

CO2law_ntl <- CO2law_ntl %>% mutate_each(funs(tolower), 'Country')
CO2law_rgl <- CO2law_rgl %>% mutate_each(funs(tolower), 'Country')

Data_scope3_merged3 <- merge(Data_scope3_merged2,CO2law_ntl ,by= c("Country", "Year"), all.x =TRUE)
Data_scope3_merged3 <- merge(Data_scope3_merged3,CO2law_rgl ,by= c("Country", "Year"), all.x =TRUE)

summary(Data_scope3_merged3)

######################################
## Add self-calculated variables ##
######################################

Data_scope3_merged3$leverage <- round((Data_scope3_merged3$LTdebt/Data_scope3_merged3$`TotalAssets`), digits = 2)
Data_scope3_merged3$LTdebt <- NULL

Data_scope3_merged3$CapInt <- round((Data_scope3_merged3$GrossPPE/Data_scope3_merged3$Revenue), digits = 2)
Data_scope3_merged3$GrossPPE <- NULL

Data_scope3_merged3$Scope12 <- Data_scope3_merged3$Scope1 + Data_scope3_merged3$Scope2

######################################
## Descriptive analysis ##
######################################

Descriptive_before <- 
  Data_scope3_merged3 %>% drop_na(Scope3)
Descriptive_before <- 
  Descriptive_before %>% drop_na(Revenue)
Descriptive_before <- 
  Descriptive_before %>% drop_na(TotalAssets)
describe(Descriptive_before)

######################################
## Determine which dataframe to use ##
######################################

# Drop all rows with NA for Scope3 emissions
Data_scope3_sub <- 
  Data_scope3_merged3 %>% drop_na(Scope3)

count(Data_scope3_sub, IntangAssets == 0)

# Drop all rows with obligated financial data
Data_scope3_sub <- 
  Data_scope3_sub %>% drop_na(Revenue)
Data_scope3_sub <- 
  Data_scope3_sub %>% drop_na(TotalAssets)
summary(Data_scope3_sub)

# fill inconvenient NA's with 0 
#Data_scope3_sub$IntangAssets[is.na(Data_scope3_sub$IntangAssets)] <- 0
#Data_scope3_sub$NetPPE[is.na(Data_scope3_sub$NetPPE)] <- 0
# Data_scope3_sub$CapExp[is.na(Data_scope3_sub$CapExp)] <- 0
Data_scope3_sub$GPMargin[is.na(Data_scope3_sub$GPMargin)] <- 100
# ????Test18= Test17[(Test17.CapEx !=0)]????

summary(Data_scope3_sub)


#Which dataframe to use ###
datayear2006 <- subset(Data_scope3_sub, Year == 2006)
datayear2007 <- subset(Data_scope3_sub, Year == 2007)
datayear2008 <- subset(Data_scope3_sub, Year == 2008)
datayear2009 <- subset(Data_scope3_sub, Year == 2009)
datayear2010 <- subset(Data_scope3_sub, Year == 2010)
datayear2011 <- subset(Data_scope3_sub, Year == 2011)
datayear2012 <- subset(Data_scope3_sub, Year == 2012)
datayear2013 <- subset(Data_scope3_sub, Year == 2013)
datayear2014 <- subset(Data_scope3_sub, Year == 2014)
datayear2015 <- subset(Data_scope3_sub, Year == 2015)
datayear2016 <- subset(Data_scope3_sub, Year == 2016)
datayear2017 <- subset(Data_scope3_sub, Year == 2017)
datayear2018 <- subset(Data_scope3_sub, Year == 2018)
datayear2019 <- subset(Data_scope3_sub, Year == 2019)
datayear2020 <- subset(Data_scope3_sub, Year == 2020)
datayear2021 <- subset(Data_scope3_sub, Year == 2021)
NA_year2006 <- sum(is.na(datayear2006))/nrow(datayear2006)
NA_year2007 <- sum(is.na(datayear2007))/nrow(datayear2007)
NA_year2008 <- sum(is.na(datayear2008))/nrow(datayear2008)
NA_year2009 <- sum(is.na(datayear2009))/nrow(datayear2009)
NA_year2010 <- sum(is.na(datayear2010))/nrow(datayear2010)
NA_year2011 <- sum(is.na(datayear2011))/nrow(datayear2011)
NA_year2012 <- sum(is.na(datayear2012))/nrow(datayear2012)
NA_year2013 <- sum(is.na(datayear2013))/nrow(datayear2013)
NA_year2014 <- sum(is.na(datayear2014))/nrow(datayear2014)
NA_year2015 <- sum(is.na(datayear2015))/nrow(datayear2015)
NA_year2016 <- sum(is.na(datayear2016))/nrow(datayear2016)
NA_year2017 <- sum(is.na(datayear2017))/nrow(datayear2017)
NA_year2018 <- sum(is.na(datayear2018))/nrow(datayear2018)
NA_year2019 <- sum(is.na(datayear2019))/nrow(datayear2019)
NA_year2020 <- sum(is.na(datayear2020))/nrow(datayear2020)
NA_year2021 <- sum(is.na(datayear2021))/nrow(datayear2021)

rbind(NA_year2006, NA_year2007, NA_year2008,NA_year2009,NA_year2010,NA_year2011,NA_year2012,NA_year2013,NA_year2014,NA_year2015,NA_year2016,NA_year2017,NA_year2018,NA_year2019,NA_year2020,NA_year2021)

# from 2008 the missing values are relatively half the amount of the years before
# continue with previous dataset

######################################
## 4. Pre-processing data & handling missing values ##
######################################

Data_scope3_merged3 <- Data_scope3_merged3 %>% arrange(Number)
summary(Data_scope3_merged3)

count(Data_scope3_merged3, CapExp == 0)

Data_scope3_merged3$Year <- as.character(Data_scope3_merged3$Year)
Data_scope3_merged3 <- subset(Data_scope3_merged3, Year > 2008)

summary(Data_scope3_merged3$Year)
Data_scope3_merged3$Year <- as.factor(Data_scope3_merged3$Year)
summary(Data_scope3_merged3$Year)

# change inf. into NA for CapInt as /0 provide inf
Data_scope3_merged3[sapply(Data_scope3_merged3, is.infinite)] <- NA
summary(Data_scope3_merged3)

# create a new column with mean group value by industry and year
Data_scope3_merged3 <- Data_scope3_merged3 %>% group_by(`ICB INDUSTRY NAME`, Year) %>% 
  mutate(impute_CapInt = round(replace(CapInt, is.na(CapInt), 
                                             mean(CapInt, na.rm = TRUE)), digits=0),
         impute_Employeesmean = round(replace(Employees, is.na(Employees), 
                                              mean(Employees, na.rm = TRUE)), digits=0),
         impute_Scope1mean = round(replace(Scope1, is.na(Scope1), 
                                           mean(Scope1, na.rm = TRUE)), digits=2),
         impute_Scope2mean = round(replace(Scope2, is.na(Scope2), 
                                           mean(Scope2, na.rm = TRUE)), digits=2),
         impute_CapExpmean = round(replace(CapExp, is.na(CapExp), 
                                           mean(CapExp, na.rm = TRUE)), digits=2),
         impute_IntangAssets = round(replace(IntangAssets, is.na(IntangAssets), 
                                           mean(IntangAssets, na.rm = TRUE)), digits=2),
         impute_NetPPE = round(replace(NetPPE, is.na(NetPPE), 
                                             mean(NetPPE, na.rm = TRUE)), digits=2),
         impute_leverage = round(replace(leverage, is.na(leverage), 
                                      mean(leverage, na.rm = TRUE)), digits=2),
         impute_Scope12 = round(replace(Scope12, is.na(Scope12), 
                                         mean(Scope12, na.rm = TRUE)), digits=2))


#Fill in Missing Data with The Next Year and otherwise previous year
Data_scope3_fill <- Data_scope3_merged3 %>%
  dplyr::group_by(Number) %>%
  fill(Employees, .direction = "updown") %>%
  fill(Scope1, .direction = "updown") %>%
  fill(Scope2, .direction = "updown") %>%
  fill(CapInt, .direction = "updown") %>%
  fill(CapExp, .direction = "updown") %>%
  fill(IntangAssets, .direction = "updown") %>%
  fill(NetPPE, .direction = "updown") %>%
  fill(leverage, .direction = "updown") %>%
  dplyr::ungroup()

summary(Data_scope3_fill)


# Drop all rows with NA for Scope3 emissions
Data_scope3_fill <- 
  Data_scope3_fill %>% drop_na(Scope3)

# Drop all rows with obligated financial data
Data_scope3_fill <- 
  Data_scope3_fill %>% drop_na(Revenue)
Data_scope3_fill <- 
  Data_scope3_fill %>% drop_na(TotalAssets)

summary(Data_scope3_fill)

# fill inconvenient NA's with 0 
#Data_scope3_fill$IntangAssets[is.na(Data_scope3_fill$IntangAssets)] <- 0
#Data_scope3_fill$NetPPE[is.na(Data_scope3_fill$NetPPE)] <- 0
# Data_scope3_fill$CapExp[is.na(Data_scope3_fill$CapExp)] <- 0
Data_scope3_fill$GPMargin[is.na(Data_scope3_fill$GPMargin)] <- 100
# ????Test18= Test17[(Test17.CapEx !=0)]????

summary(Data_scope3_fill)

count(Data_scope3_sub, NetPPE == 100)

# filL NA with group mean value
Data_scope3_fill_mean <- Data_scope3_fill %>% 
  mutate(CapInt = coalesce(CapInt,impute_CapInt),
         Employees = coalesce(Employees,impute_Employeesmean),
         Scope1 = coalesce(Scope1,impute_Scope1mean),
         Scope2 = coalesce(Scope2,impute_Scope2mean),
         CapExp = coalesce(CapExp,impute_CapExpmean),
         IntangAssets = coalesce(IntangAssets,impute_IntangAssets),
         NetPPE = coalesce(NetPPE,impute_NetPPE),
         leverage = coalesce(leverage,impute_leverage),
         Scope12 = coalesce(Scope12,impute_Scope12))

Data_scope3_fill_mean$impute_CapInt <- NULL
Data_scope3_fill_mean$impute_Employeesmean <- NULL
Data_scope3_fill_mean$impute_Scope1mean <- NULL
Data_scope3_fill_mean$impute_Scope2mean <- NULL
Data_scope3_fill_mean$impute_CapExpmean <- NULL
Data_scope3_fill_mean$impute_GPMarginmean <- NULL
Data_scope3_fill_mean$impute_IntangAssets <- NULL
Data_scope3_fill_mean$impute_NetPPE <- NULL
Data_scope3_fill_mean$impute_leverage <- NULL
Data_scope3_fill_mean$impute_Scope12 <- NULL

summary(Data_scope3_fill_mean)

# delete inappropriate financial data
Data_scope3_fill_mean <- Data_scope3_fill_mean[!(Data_scope3_fill_mean$GPMargin < 0 | Data_scope3_fill_mean$GPMargin > 100),]
Data_scope3_fill_mean <- Data_scope3_fill_mean[!(Data_scope3_fill_mean$Revenue < 0),]
Data_scope3_fill_mean <- Data_scope3_fill_mean[!(Data_scope3_fill_mean$NetPPE < 0),]
Data_scope3_fill_mean <- Data_scope3_fill_mean[!(Data_scope3_fill_mean$IntangAssets < 0),]

summary(Data_scope3_fill_mean)

# add self-calculated variables
#Data_scope3_fill_mean$leverage <- round((Data_scope3_fill_mean$LTdebt/Data_scope3_fill_mean$`TotalAssets`), digits = 2)
#Data_scope3_fill_mean$LTdebt <- NULL

#Data_scope3_fill_mean$CapInt <- round((Data_scope3_fill_mean$GrossPPE/Data_scope3_fill_mean$Revenue), digits = 2)
#Data_scope3_fill_mean$GrossPPE <- NULL
# dividing by 0 will cause 'inf' and 'NA' and therefore should be removed


#write_xlsx(Data_scope3_fill,"~/Documents/Thesis/Data/Final_Dataset.xlsx")


# convert categorical variables into dummies
dataf <- dummy_cols(Data_scope3_fill_mean, select_columns = c("ICB INDUSTRY NAME", "Year", "IncomeGroup", "CO2law_ntl", "CO2law_rgl"))
#dataf$'ICB INDUSTRY NAME' <- as.factor(dataf$'ICB INDUSTRY NAME')

# Delete unnessecary variables
#dataf$Number <- NULL
dataf$"ICB SUPRSECTR NAME" <- NULL
dataf$Year <- NULL
dataf$CO2law_ntl_NA <- NULL
dataf$CO2law_rgl_NA <- NULL
dataf$IncomeGroup_NA <- NULL

dataf$IncomeGroup <- NULL
dataf$CO2law_ntl <- NULL
dataf$CO2law_rgl <- NULL
dataf$Country <- NULL
dataf$Code <- NULL
dataf$"ICB INDUSTRY NAME" <- NULL
dataf$"ICB INDUSTRY NAME_NA" <- NULL

# Delete low income and change that single obseration to lower middle income
#dataf['11619', 'IncomeGroup_LM'] <- 1
##dataf$"IncomeGroup_L" <- NULL

summary(dataf)

# Delete missing value column for country level data
data_final <- dataf %>% drop_na(IncomeGroup_H) 
data_final <- data_final %>% drop_na(CO2law_ntl_No)
summary(data_final)

write_xlsx(data_final,"~/Documents/Thesis/Data/Final_Dataset.xlsx")


# log transform every continuous variable 
data_final_log <- data_final
data_final_log[,c((2:9), 11, 14)] <- log1p(data_final[,c((2:9), 11, 14)])
data_final_log

write_xlsx(data_final_log,"~/Documents/Thesis/Data/Final_Dataset_log.xlsx")


######################################
## Dataset without outliers ##
######################################

# detect and remove outliers
boxplot(data_winso$CapInt)
summary(data_winso$CapInt)

data_winso <- data_final
data_winso$Scope12 <- Winsorize(data_winso$Scope12, probs = c(0.01, 0.99))
data_winso$Scope1 <- Winsorize(data_winso$Scope1, probs = c(0.01, 0.99))
data_winso$Scope2 <- Winsorize(data_winso$Scope2, probs = c(0.01, 0.99))
data_winso$Scope3 <- Winsorize(data_winso$Scope3, probs = c(0.01, 0.99))
data_winso$Revenue <- Winsorize(data_winso$Revenue, probs = c(0.01, 0.99))
data_winso$Employees <- Winsorize(data_winso$Employees, probs = c(0.01, 0.99))
data_winso$TotalAssets <- Winsorize(data_winso$TotalAssets, probs = c(0.01, 0.99))
data_winso$NetPPE <- Winsorize(data_winso$NetPPE, probs = c(0.01, 0.99))
data_winso$IntangAssets <- Winsorize(data_winso$IntangAssets, probs = c(0.01, 0.99))
data_winso$CapExp <- Winsorize(data_winso$CapExp, probs = c(0.01, 0.99))

summary(data_winso)

# log transform every continuous variable
describe(data_winso)
data_winso[,c((2:9), 11, 14)] <- log1p(data_winso[,c((2:9), 11, 14)])
describe(data_winso)

write_xlsx(data_winso,"~/Documents/Thesis/Data/Final_Dataset_log_winso.xlsx")
describe(Final_dataset_log_winso)

#number of companies
Number_companies <- Final_dataset_log_winso %>%
  group_by(number) %>%
  summarise(n = n())




