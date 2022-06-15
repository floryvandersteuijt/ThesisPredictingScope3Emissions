
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

######################################
### import data ###
######################################

Robustness_checks <- read_excel("~/Documents/Thesis/Robustness/Robustness checks2.xlsx", 
                                     sheet = "excluded variables")

Data_scope3_merged3 <- merge(Data_scope3_merged2,CO2law_ntl ,by= c("Country", "Year"), all.x =TRUE)
Data_scope3_merged3 <- merge(Data_scope3_merged3,CO2law_rgl ,by= c("Country", "Year"), all.x =TRUE)
Industries <- read_excel("~/Documents/Thesis/Robustness/Robustness checks2.xlsx", 
                                sheet = "industries")
Fuelintensity <- read_excel("~/Documents/Thesis/Robustness/Robustness checks2.xlsx", 
                            sheet = "Fuelintensity")
Figure<- read_excel("~/Documents/Thesis/Robustness/Robustness checks2.xlsx", 
                                sheet = "Figure Evaluation")

######################################
### Pre-processing ###
######################################

# ENERGY USE AND PURCHASE AND AGE
# separate columns
Robustness_checks <- separate(Robustness_checks, Code, c('Code', 'Variablecode'))

# delete unnecessary columns
Robustness_checks$Variablecode <- NULL
Robustness_checks$Name <- NULL

# reshape
Robustness_checks_melt <- melt(Robustness_checks, id=c("Code","Variable"), variable.name = 'Year')
Robustness_checks_melt <- dcast(Robustness_checks_melt, Code + Year ~ Variable)

# FUEL INTENSITY
colnames(Fuelintensity)[colnames(Fuelintensity) == 'Region/Country/Economy'] <- 'Country'
Fuelintensity <- melt(Fuelintensity, id=c("Country"), variable.name = 'Year')

# INDUSTRY
#subset industries with convenient data
Industry <- subset(Industries, select = c(1, 7))

# Merging industries
Industry$Code <- Industry$Type
Industry$Type <- NULL

######################################
### Merging data ###
######################################

#INDUSTRY
# merge statistics with timeseries
Robustness_checks_melt <- merge(Robustness_checks_melt,Industry ,by="Code")

#ENERGY PURCHASE AND USE AND AGE
Robustness_checks_merged <- merge(Data_scope3_merged3,Robustness_checks_melt ,by= c("Code", "Year"), all.x =TRUE)

# delete unnessecary columns and rename
Robustness_checks_merged <- subset(Robustness_checks_merged, select = -c(22,23,25) )

colnames(Robustness_checks_merged)[colnames(Robustness_checks_merged) == 'Direct Energy/Energy Purchased Direct'] <- 'ENE_purch'
colnames(Robustness_checks_merged)[colnames(Robustness_checks_merged) == 'Energy Use Total'] <- 'ENE_use'
colnames(Robustness_checks_merged)[colnames(Robustness_checks_merged) == 'DEPRECIATION'] <- 'Depreciation'

#FUEL INTENSITY
Fuelintensity <- Fuelintensity %>% mutate_each(funs(tolower), 'Country')
Robustness_checks_merged <- merge(Robustness_checks_merged,Fuelintensity ,by= c("Country", "Year"), all.x =TRUE)
colnames(Robustness_checks_merged)[colnames(Robustness_checks_merged) == 'value'] <- 'Fuelintensity'
Robustness_checks_merged$Fuelintensity <- as.numeric(Robustness_checks_merged$Fuelintensity)

summary(Robustness_checks_merged)

######################################
## Add self-calculated variables ##
######################################

Robustness_checks_merged$Age <- round((Robustness_checks_merged$GrossPPE/Robustness_checks_merged$`Depreciation`), digits = 2)
Robustness_checks_merged$Depreciation <- NULL

Robustness_checks_merged$CapInt <- round((Robustness_checks_merged$GrossPPE/Robustness_checks_merged$Revenue), digits = 2)
Robustness_checks_merged$GrossPPE <- NULL

Robustness_checks_merged$Scope12 <- Robustness_checks_merged$Scope1 + Robustness_checks_merged$Scope2

Robustness_checks_merged$leverage <- round((Robustness_checks_merged$LTdebt/Robustness_checks_merged$`TotalAssets`), digits = 2)
Robustness_checks_merged$LTdebt <- NULL


######################################
## 4. Pre-processing data & handling missing values ##
######################################

Robustness_checks_merged <- Robustness_checks_merged %>% arrange(Number)
summary(Robustness_checks_merged)

Robustness_checks_merged$Year <- as.character(Robustness_checks_merged$Year)
Robustness_checks_merged <- subset(Robustness_checks_merged, Year > 2008)

summary(Robustness_checks_merged$Year)
Robustness_checks_merged$Year <- as.factor(Robustness_checks_merged$Year)
summary(Robustness_checks_merged$Year)

# change inf. into NA for CapInt as /0 provide inf
Robustness_checks_merged[sapply(Robustness_checks_merged, is.infinite)] <- NA
summary(Robustness_checks_merged)

Robustness_checks_merged$ENE_purch[is.nan(Robustness_checks_merged$ENE_purch)]<-NA
Robustness_checks_merged$ENE_use[is.nan(Robustness_checks_merged$ENE_use)]<-NA
Robustness_checks_merged$Age[is.nan(Robustness_checks_merged$Age)]<-NA

# create a new column with mean group value by industry and year
Robustness_checks_fill <- Robustness_checks_merged %>% group_by(`ICB INDUSTRY NAME`, Year) %>% 
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
                                        mean(Scope12, na.rm = TRUE)), digits=2),
         impute_ENE_purch = round(replace(ENE_purch, is.na(ENE_purch), 
                                          mean(ENE_purch, na.rm = TRUE)), digits=2),
         impute_ENE_used = round(replace(ENE_use, is.na(ENE_use), 
                                         mean(ENE_use, na.rm = TRUE)), digits=2),
         impute_Age = round(replace(Age, is.na(Age), 
                                             mean(Age, na.rm = TRUE)), digits=2),
         impute_Fuelintensity = round(replace(Fuelintensity, is.na(Fuelintensity), 
                                      mean(Fuelintensity, na.rm = TRUE)), digits=2))
         


#Fill in Missing Data with The Next Year and otherwise previous year
Robustness_checks_fill <- Robustness_checks_fill %>%
  dplyr::group_by(Number) %>%
  fill(Employees, .direction = "updown") %>%
  fill(Scope1, .direction = "updown") %>%
  fill(Scope2, .direction = "updown") %>%
  fill(CapInt, .direction = "updown") %>%
  fill(CapExp, .direction = "updown") %>%
  fill(IntangAssets, .direction = "updown") %>%
  fill(NetPPE, .direction = "updown") %>%
  fill(leverage, .direction = "updown") %>%
  fill(ENE_purch, .direction = "updown") %>%
  fill(ENE_use, .direction = "updown") %>%
  fill(Age, .direction = "updown") %>%
  fill(Fuelintensity, .direction = "updown") %>%
  dplyr::ungroup()

summary(Robustness_checks_fill)


# Drop all rows with NA for Scope3 emissions
Robustness_checks_fill <- 
  Robustness_checks_fill %>% drop_na(Scope3)

# Drop all rows with obligated financial data
Robustness_checks_fill <- 
  Robustness_checks_fill %>% drop_na(Revenue)
Robustness_checks_fill <- 
  Robustness_checks_fill %>% drop_na(TotalAssets)

summary(Robustness_checks_fill)

# fill inconvenient NA's with 100 
Robustness_checks_fill$GPMargin[is.na(Robustness_checks_fill$GPMargin)] <- 100
# ????Test18= Test17[(Test17.CapEx !=0)]????

summary(Robustness_checks_fill)

# filL NA with group mean value
Robustness_checks_fill_mean <- Robustness_checks_fill %>% 
  mutate(CapInt = coalesce(CapInt,impute_CapInt),
         Employees = coalesce(Employees,impute_Employeesmean),
         Scope1 = coalesce(Scope1,impute_Scope1mean),
         Scope2 = coalesce(Scope2,impute_Scope2mean),
         CapExp = coalesce(CapExp,impute_CapExpmean),
         IntangAssets = coalesce(IntangAssets,impute_IntangAssets),
         NetPPE = coalesce(NetPPE,impute_NetPPE),
         leverage = coalesce(leverage,impute_leverage),
         Scope12 = coalesce(Scope12,impute_Scope12),
         ENE_purch = coalesce(ENE_purch,impute_ENE_purch),
         ENE_use = coalesce(ENE_use,impute_ENE_used),
         Age = coalesce(Age,impute_Age),
         Fuelintensity = coalesce(Fuelintensity,impute_Fuelintensity))

Robustness_checks_fill_mean$impute_CapInt <- NULL
Robustness_checks_fill_mean$impute_Employeesmean <- NULL
Robustness_checks_fill_mean$impute_Scope1mean <- NULL
Robustness_checks_fill_mean$impute_Scope2mean <- NULL
Robustness_checks_fill_mean$impute_CapExpmean <- NULL
Robustness_checks_fill_mean$impute_GPMarginmean <- NULL
Robustness_checks_fill_mean$impute_IntangAssets <- NULL
Robustness_checks_fill_mean$impute_NetPPE <- NULL
Robustness_checks_fill_mean$impute_leverage <- NULL
Robustness_checks_fill_mean$impute_Scope12 <- NULL
Robustness_checks_fill_mean$impute_ENE_purch <- NULL
Robustness_checks_fill_mean$impute_ENE_used <- NULL
Robustness_checks_fill_mean$impute_Age <- NULL
Robustness_checks_fill_mean$impute_Fuelintensity <- NULL

summary(Robustness_checks_fill_mean)

# delete inappropriate financial data
Robustness_checks_fill_mean <- Robustness_checks_fill_mean[!(Robustness_checks_fill_mean$GPMargin < 0 | Robustness_checks_fill_mean$GPMargin > 100),]
Robustness_checks_fill_mean <- Robustness_checks_fill_mean[!(Robustness_checks_fill_mean$Revenue < 0),]
Robustness_checks_fill_mean <- Robustness_checks_fill_mean[!(Robustness_checks_fill_mean$NetPPE < 0),]
Robustness_checks_fill_mean <- Robustness_checks_fill_mean[!(Robustness_checks_fill_mean$IntangAssets < 0),]

summary(Robustness_checks_fill_mean)

# convert categorical variables into dummies
dataf2 <- dummy_cols(Robustness_checks_fill_mean, select_columns = c("ICB INDUSTRY NAME", "Year", "IncomeGroup", "CO2law_ntl", "CO2law_rgl","ICBSSN"))
#dataf$'ICB INDUSTRY NAME' <- as.factor(dataf$'ICB INDUSTRY NAME')

# Delete unnessecary variables
#dataf$Number <- NULL
dataf2$"ICB SUPRSECTR NAME" <- NULL
dataf2$Year <- NULL
dataf2$CO2law_ntl_NA <- NULL
dataf2$CO2law_rgl_NA <- NULL
dataf2$IncomeGroup_NA <- NULL

dataf2$IncomeGroup <- NULL
dataf2$CO2law_ntl <- NULL
dataf2$CO2law_rgl <- NULL
dataf2$Country <- NULL
dataf2$Code <- NULL
dataf2$"ICB INDUSTRY NAME" <- NULL
dataf2$"ICB INDUSTRY NAME_NA" <- NULL

dataf2$"ICBSSN_NA" <- NULL
dataf2$"ICBSSN" <- NULL
dataf2$"ICBSSN_NA.1" <- NULL

summary(dataf2)

# Delete missing value column for country level data
data_final2 <- dataf2 %>% drop_na(IncomeGroup_H) 
data_final2 <- data_final2 %>% drop_na(CO2law_ntl_No)
data_final2 <- data_final2 %>% drop_na(ICBSSN_Technology)
data_final2 <- data_final2 %>% drop_na(Fuelintensity)
summary(data_final2)

# log transform every continuous variable 
data_final_log2 <- data_final2
data_final_log2[,c((2:9), (11:14), 16)] <- log1p(data_final2[,c((2:9), (11:14), 16)])
data_final_log2


######################################
## Dataset without outliers ##
######################################

# detect and remove outliers
boxplot(data_winso$CapInt)
summary(data_winso$CapInt)

data_winso <- data_final2
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
data_winso$ENE_purch <- Winsorize(data_winso$ENE_purch, probs = c(0.01, 0.99))
data_winso$ENE_use <- Winsorize(data_winso$ENE_use, probs = c(0.01, 0.99))
data_winso$Age <- Winsorize(data_winso$Age, probs = c(0.01, 0.99))
data_winso$Fuelintensity <- Winsorize(data_winso$Fuelintensity, probs = c(0.01, 0.99))

summary(data_winso)

# log transform every continuous variable
describe(data_winso)
data_winso[,c((2:9), (11:13), 15, 17)] <- log1p(data_winso[,c((2:9), (11:13), 15, 17)])
describe(data_winso)

#Delete ICB industries
data_winso <- subset(data_winso, select = -c(19:29))

write_xlsx(data_winso,"~/Documents/Thesis/Robustness/Final_Dataset_log_winso2.xlsx")

#number of companies
Number_companies <- Final_dataset_log_winso %>%
  group_by(number) %>%
  summarise(n = n())



