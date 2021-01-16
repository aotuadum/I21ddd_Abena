# aotuadu1, 1/11/21, final_project.R

library(dplyr)
library(magrittr)
library(gt)
library(ggplot2)
library(ggpubr)
library(scales)



# 0. import data ####
disease.original <- readxl::read_xlsx("Detail-Disease-Blended-forpub.xlsx", na = "NA")
disease <- disease.original


# 1. getting to know my data ####
# row names
rownames(disease)
# The row names in this data set are all numbers. The row names went up to 1000, and 4817
# entries were omitted. 


# column names
colnames(disease)
# The column names in this data set are the year, medical condition, and different 
# monetary/spending measures, such as per capita dollars, NIPA dollars, and more.


# number of rows
length(rownames(disease))
# There are 5817 rows in this data set.


# number of columns
length(colnames(disease))
# There are 19 columns in this data set.


# format of columns
class(disease$Year) #numeric
class(disease$`AHRQ/CCS Condition Category`) #character
class(disease$`CCS Category Number`) #numeric
class(disease$`AHRQ Category`) #character
class(disease$`Volatility Level`) #character
class(disease$`NIPA dollars`) #numeric
class(disease$`Uncontrolled dollars`) #numeric
class(disease$`Number of Episodes`) #numeric
class(disease$`Per Capita Index`) #numeric
class(disease$`Medical Care Expenditure Index`) #numeric
class(disease$`Prevalence Index`) #numeric
class(disease$`Per Capita Dollars`) #numeric
class(disease$`Cost Per Case Dollars`) #numeric
class(disease$`Unweighted MEPS Patients`) #numeric
class(disease$`Share MEPS Spending`) #numeric
class(disease$`CCS Chapter Number`) #numeric
class(disease$`CCS Chapter`) #character
class(disease$`CCS Chapter Dollars (Billions)`) #numeric
class(disease$`CCS Category`) #character
# In this data set, 5 out of the 19 columns were characters; the remaining 14 were numeric.


# identifying missing data
is.na(disease$Year) 
is.na(disease$`AHRQ/CCS Condition Category`) 
is.na(disease$`CCS Category Number`) 
is.na(disease$`AHRQ Category`)
is.na(disease$`Volatility Level`) 
is.na(disease$`NIPA dollars`) 
is.na(disease$`Uncontrolled dollars`) 
is.na(disease$`Number of Episodes`) 
is.na(disease$`Per Capita Index`) 
is.na(disease$`Medical Care Expenditure Index`) 
is.na(disease$`Prevalence Index`) 
is.na(disease$`Per Capita Dollars`) 
is.na(disease$`Cost Per Case Dollars`) 
is.na(disease$`Unweighted MEPS Patients`) 
is.na(disease$`Share MEPS Spending`) 
is.na(disease$`CCS Chapter Number`) 
is.na(disease$`CCS Chapter`) 
is.na(disease$`CCS Chapter Dollars (Billions)`) 
is.na(disease$`CCS Category`) 
# Admittedly, this isn't efficient due to the large size of this data set.


# naming columns
# Original column names had spaces
colnames(disease)[2] <- "AHRQ.CCS.Condition.Category"
colnames(disease)[3] <- "CCS.Category.Number"
colnames(disease)[4] <- "AHRQ.Category"
colnames(disease)[5] <- "Volatility.Level"
colnames(disease)[6] <- "NIPA.Dollars"
colnames(disease)[7] <- "Uncontrolled.Dollars"
colnames(disease)[8] <- "Number.Of.Episodes"
colnames(disease)[9] <- "Per.Capita.Index"
colnames(disease)[10] <- "Medical.Care.Expenditure.Index"
colnames(disease)[11] <- "Prevalence.Index"
colnames(disease)[12] <- "Per.Capita.Dollars"
colnames(disease)[13] <- "Cost.Per.Case.Dollars"
colnames(disease)[14] <- "Unweighted.MEPS.Patients"
colnames(disease)[15] <- "Share.MEPS.Spending"
colnames(disease)[16] <- "CCS.Chapter.Number"
colnames(disease)[17] <- "CCS.Chapter"
colnames(disease)[18] <- "CCS.Chapter.Dollars.Billions"
colnames(disease)[19] <- "CCS.Category"


# assigning observation ID
disease$disease.ID <- rownames(disease)


# 2. missing data ####
# creating binary flags
disease$Year.flag <- ifelse(
  is.na(disease$Year), 1, 0
  )
disease$AHRQ.CCS.Condition.Category.flag <- ifelse(
  is.na(disease$AHRQ.CCS.Condition.Category), 1, 0
  )
disease$CCS.Category.Number.flag <- ifelse(
  is.na(disease$CCS.Category.Number), 1, 0
  )
disease$AHRQ.Category.flag <- ifelse(
  is.na(disease$AHRQ.Category), 1, 0
  )
disease$Volatility.Level.flag <- ifelse(
  is.na(disease$Volatility.Level), 1, 0
  )
disease$NIPA.Dollars.flag <- ifelse(
  is.na(disease$NIPA.Dollars), 1, 0
  )
disease$Uncontrolled.Dollars.flag <- ifelse(
  is.na(disease$Uncontrolled.Dollars), 1, 0
  )
disease$Number.Of.Episodes.flag <- ifelse(
  is.na(disease$Number.Of.Episodes), 1, 0
  )
disease$Per.Capita.Index.flag <- ifelse(
  is.na(disease$Per.Capita.Index), 1, 0
  )
disease$Medical.Care.Expenditure.Index.flag <- ifelse(
  is.na(disease$Medical.Care.Expenditure.Index), 1, 0
  )
disease$Prevalence.Index.flag <- ifelse(
  is.na(disease$Prevalence.Index), 1, 0
  )
disease$Per.Capita.Dollars.flag <- ifelse(
  is.na(disease$Per.Capita.Dollars), 1, 0
  )
disease$Cost.Per.Case.Dollars.flag <- ifelse(
  is.na(disease$Cost.Per.Case.Dollars), 1, 0
  )
disease$Unweighted.MEPS.Patients.flag <- ifelse(
  is.na(disease$Unweighted.MEPS.Patients), 1, 0
  )
disease$Share.MEPS.Spending.flag <- ifelse(
  is.na(disease$Share.MEPS.Spending), 1, 0
  )
disease$CCS.Chapter.Number.flag <- ifelse(
  is.na(disease$CCS.Chapter.Number), 1, 0
  )
disease$CCS.Chapter.flag <- ifelse(
  is.na(disease$CCS.Chapter), 1, 0
  )
disease$CCS.Chapter.Dollars.Billions.flag <- ifelse(
  is.na(disease$CCS.Chapter.Dollars.Billions), 1, 0
  )
disease$CCS.Category.flag <- ifelse(
  is.na(disease$CCS.Category), 1, 0
  )
# Using binary flags, I've found that the last 5 columns are all missing the first 17 values,
# if not more.



# dealing with missing values
length(grep(1, disease$Year.flag))
grep(1, disease$Year.flag)
# I chose the variable year to look at because logically, it shouldn't have values missing.
# rows 5815, 5816, and 5817 all have missing values.


as.list(disease[5815, 1:19])
as.list(disease[5816, 1:19])
as.list(disease[5817, 1:19])
# all three of these rows are completely made of missing values.


disease <- disease[1:5814, ]
# removing empty rows


# finding what conditions the missing data correspond to
length(grep(1, disease$Year.flag)) # 0 missing values
length(grep(1, disease$AHRQ.CCS.Condition.Category.flag)) # 0 missing values
length(grep(1, disease$CCS.Category.Number.flag)) # 0 missing values
length(grep(1, disease$AHRQ.Category.flag)) # 0 missing values
length(grep(1, disease$Volatility.Level.flag)) # 1098 missing values
length(grep(1, disease$NIPA.Dollars.flag)) # 0 missing values
length(grep(1, disease$Uncontrolled.Dollars.flag)) # 0 missing values
length(grep(1, disease$Number.Of.Episodes.flag)) # 0 missing values
length(grep(1, disease$Per.Capita.Index.flag)) # 0 missing values
length(grep(1, disease$Medical.Care.Expenditure.Index.flag)) # 0 missing values
length(grep(1, disease$Prevalence.Index.flag)) # 0 missing values
length(grep(1, disease$Per.Capita.Dollars.flag)) # 0 missing values
length(grep(1, disease$Cost.Per.Case.Dollars.flag)) # 0 missing values
length(grep(1, disease$Unweighted.MEPS.Patients.flag)) # 0 missing values
length(grep(1, disease$Share.MEPS.Spending.flag)) # 1422 missing values
length(grep(1, disease$CCS.Chapter.Number.flag)) # 1098 missing values
length(grep(1, disease$CCS.Chapter.flag)) # 1098 missing values
length(grep(1, disease$CCS.Chapter.Dollars.Billions.flag)) # 1098 missing values
length(grep(1, disease$CCS.Category.flag)) # 1098 missing values


disease %>% 
  filter(Volatility.Level.flag == 1, 
         CCS.Chapter.Number.flag == 1, 
         CCS.Chapter.flag == 1,
         CCS.Category.flag == 1) %>% 
  select(AHRQ.CCS.Condition.Category) %>% 
  unique() %>% 
  as.list()
# All the missing values belong to summaries within the original category column, so there
# is no reason for concern. Additionally, these variables will not be used in my analysis.

# However, the column Share.MEPS.Spending has 1422 values missing as opposed to 1098.
disease %>% 
  filter(Share.MEPS.Spending.flag == 1) %>% 
  select(AHRQ.CCS.Condition.Category) %>% 
  unique() %>% 
  as.list()

# missing values correspond to _AHRQ entries and some more specific conditions. This is also
# explainable, for the MEPS changed its classification system, which didn't initially include 
# the specific conditions that we see are missing.

# 3. general cleaning and formatting ####
# N/A

# 4. variable creation and coding ####

unique(grep("_AHRQ", disease$AHRQ.CCS.Condition.Category, value = TRUE))
# This shows all the different medical condition categories.


# I will now categorize each condition by disease type, and use the disease types to create
# a column of factor variables.
Communicable <- c("Skin disorders_AHRQ",
                  "Infectious diseases_AHRQ", 
                  "Acute bronchitis and URI_AHRQ", 
                  "Other eye disorders_AHRQ",
                  "Pneumonia_AHRQ",
                  "Urinary tract infections_AHRQ",
                  "Otitis media_AHRQ",
                  "Appendicitis_AHRQ",
                  "Intestinal infection_AHRQ",
                  "Influenza_AHRQ",
                  "CNS infection_AHRQ",
                  "Tonsillitis_AHRQ")


Noncommunicable <- c("Heart conditions_AHRQ",
                     "COPD, asthma_AHRQ",
                     "Cancer_AHRQ",
                     "Mental disorders_AHRQ",
                     "Osteoarthritis and other non-traumatic joint disorders_AHRQ",
                     "Diabetes mellitus_AHRQ",
                     "Hypertension_AHRQ",
                     "Back problems_AHRQ",
                     "Kidney disease_AHRQ",
                     "Systemic lupus and connective tissues disorders_AHRQ",
                     "Hyperlipidemia_AHRQ",
                     "Other CNS disorders_AHRQ",
                     "Other circulatory conditions arteries, veins, and lymphatics_AHRQ",
                     "Other GI_AHRQ",
                     "Other endocrine, nutritionial and immune disorder_AHRQ",
                     "Other bone and musculoskeletal disease_AHRQ",
                     "Cerebrovascular disease_AHRQ",
                     "Female genital disorders, and contraception_AHRQ",
                     "Disorders of the upper GI_AHRQ",
                     "Non-malignant neoplasm_AHRQ",
                     "Gallbladder, pancreatic, and liver disease_AHRQ",
                     "Other stomach and intestinal disorders_AHRQ",
                     "Other urinary_AHRQ",
                     "Cataract_AHRQ",
                     "Hereditary, degenerative and other nervous system disorders_AHRQ",
                     "Anemia and other deficiencies_AHRQ",
                     "Thyroid disease_AHRQ",                                             
                     "Hemorrhagic, coagulation, and disorders of white blood wells_AHRQ",
                     "Male genital disorders_AHRQ",                                      
                     "Epilepsy and convulsions_AHRQ",                                    
                     "Non-malignant breast disease_AHRQ",                                
                     "Congenital anomalies_AHRQ",                                        
                     "Glaucoma_AHRQ",                                                    
                     "Hernias_AHRQ",                                                     
                     "Perinatal conditions_AHRQ",
                     "Disorders of mouth and esophagus_AHRQ",   
                     "Paralysis_AHRQ")


Injury <- c("Trauma-related disorders_AHRQ",
            "Poisoning by medical and non-medical substances_AHRQ",
            "Coma, brain damage_AHRQ")


Other <- c("Other care and screening_AHRQ",
           "Symptoms_AHRQ",
           "Residual codes_AHRQ",
           "Complications of surgery or device_AHRQ",
           "Normal birth/live born_AHRQ",
           "Headache_AHRQ",
           "Complications of pregnancy and birth_AHRQ",
           "Allergic reactions_AHRQ")
# there is a category of "other" conditions that I was unable to classify as one of the previous
# three categories. This will be seen in the final factor variable column.


# creating factor variable column for disease type
disease$Disease.Type <- ifelse(disease$AHRQ.CCS.Condition.Category %in% Communicable, 'Communicable',
                        ifelse(disease$AHRQ.CCS.Condition.Category %in% Noncommunicable, 'Noncommunicable',
                        ifelse(disease$AHRQ.CCS.Condition.Category %in% Injury, 'Injury',
                        ifelse(disease$AHRQ.CCS.Condition.Category %in% Other, 'Other', 'Not Included'))))
disease$Disease.Type <- as.factor(disease$Disease.Type)
class(disease$Disease.Type)


# create dataframe disease.clean
disease.clean <- disease %>% 
filter(Disease.Type != "Not Included") %>% 
  arrange(Year, Disease.Type) %>% 
  select(Year, AHRQ.CCS.Condition.Category, NIPA.Dollars, Disease.Type)



# 5. data manipulation ####


# separating and creating new yearly dataframes with sum and percentage columns
# for loop used for every year to calculate both the total medical expenditure and proportions
i = 1

# 2000
disease.clean.2000 <- disease.clean %>%
  filter(Year == 2000) %>% 
  arrange(Disease.Type)

disease.clean.2000$Percentage.2000 <- 0

for (i in 1:60) {
  disease.clean.2000$Sum.2000 <- sum(disease.clean.2000$NIPA.Dollars)
  disease.clean.2000$Percentage.2000[i] <- 
    (disease.clean.2000$NIPA.Dollars[i] / disease.clean.2000$Sum.2000[i]) 
}


# 2001
disease.clean.2001 <- disease.clean %>%
  filter(Year == 2001) %>% 
  arrange(Disease.Type)

disease.clean.2001$Percentage.2001 <- 0

for (i in 1:60) {
  disease.clean.2001$Sum.2001 <- sum(disease.clean.2001$NIPA.Dollars)
  disease.clean.2001$Percentage.2001[i] <- 
    (disease.clean.2001$NIPA.Dollars[i] / disease.clean.2001$Sum.2001[i]) 
}


# 2002
disease.clean.2002 <- disease.clean %>%
  filter(Year == 2002) %>% 
  arrange(Disease.Type)

disease.clean.2002$Percentage.2002 <- 0

for (i in 1:60) {
  disease.clean.2002$Sum.2002 <- sum(disease.clean.2002$NIPA.Dollars)
  disease.clean.2002$Percentage.2002[i] <- 
    (disease.clean.2002$NIPA.Dollars[i] / disease.clean.2002$Sum.2002[i]) 
}


# 2003
disease.clean.2003 <- disease.clean %>%
  filter(Year == 2003) %>% 
  arrange(Disease.Type)

disease.clean.2003$Percentage.2003 <- 0

for (i in 1:60) {
  disease.clean.2003$Sum.2003 <- sum(disease.clean.2003$NIPA.Dollars)
  disease.clean.2003$Percentage.2003[i] <- 
    (disease.clean.2003$NIPA.Dollars[i] / disease.clean.2003$Sum.2003[i]) 
}


# 2004
disease.clean.2004 <- disease.clean %>%
  filter(Year == 2004) %>% 
  arrange(Disease.Type)

disease.clean.2004$Percentage.2004 <- 0

for (i in 1:60) {
  disease.clean.2004$Sum.2004 <- sum(disease.clean.2004$NIPA.Dollars)
  disease.clean.2004$Percentage.2004[i] <- 
    (disease.clean.2004$NIPA.Dollars[i] / disease.clean.2004$Sum.2004[i]) 
}


# 2005
disease.clean.2005 <- disease.clean %>%
  filter(Year == 2005) %>% 
  arrange(Disease.Type)

disease.clean.2005$Percentage.2005 <- 0

for (i in 1:60) {
  disease.clean.2005$Sum.2005 <- sum(disease.clean.2005$NIPA.Dollars)
  disease.clean.2005$Percentage.2005[i] <- 
    (disease.clean.2005$NIPA.Dollars[i] / disease.clean.2005$Sum.2005[i]) 
}


# 2006
disease.clean.2006 <- disease.clean %>%
  filter(Year == 2006) %>% 
  arrange(Disease.Type)

disease.clean.2006$Percentage.2006 <- 0

for (i in 1:60) {
  disease.clean.2006$Sum.2006 <- sum(disease.clean.2006$NIPA.Dollars)
  disease.clean.2006$Percentage.2006[i] <- 
    (disease.clean.2006$NIPA.Dollars[i] / disease.clean.2006$Sum.2006[i]) 
}


# 2007
disease.clean.2007 <- disease.clean %>%
  filter(Year == 2007) %>% 
  arrange(Disease.Type)

disease.clean.2007$Percentage.2007 <- 0

for (i in 1:60) {
  disease.clean.2007$Sum.2007 <- sum(disease.clean.2007$NIPA.Dollars)
  disease.clean.2007$Percentage.2007[i] <- 
    (disease.clean.2007$NIPA.Dollars[i] / disease.clean.2007$Sum.2007[i]) 
}


# 2008
disease.clean.2008 <- disease.clean %>%
  filter(Year == 2008) %>% 
  arrange(Disease.Type)

disease.clean.2008$Percentage.2008 <- 0

for (i in 1:60) {
  disease.clean.2008$Sum.2008 <- sum(disease.clean.2008$NIPA.Dollars)
  disease.clean.2008$Percentage.2008[i] <- 
    (disease.clean.2008$NIPA.Dollars[i] / disease.clean.2008$Sum.2008[i]) 
}


# 2009
disease.clean.2009 <- disease.clean %>%
  filter(Year == 2009) %>% 
  arrange(Disease.Type)

disease.clean.2009$Percentage.2009 <- 0

for (i in 1:60) {
  disease.clean.2009$Sum.2009 <- sum(disease.clean.2009$NIPA.Dollars)
  disease.clean.2009$Percentage.2009[i] <- 
    (disease.clean.2009$NIPA.Dollars[i] / disease.clean.2009$Sum.2009[i]) 
}


# 2010
disease.clean.2010 <- disease.clean %>%
  filter(Year == 2010) %>% 
  arrange(Disease.Type)

disease.clean.2010$Percentage.2010 <- 0

for (i in 1:60) {
  disease.clean.2010$Sum.2010 <- sum(disease.clean.2010$NIPA.Dollars)
  disease.clean.2010$Percentage.2010[i] <- 
    (disease.clean.2010$NIPA.Dollars[i] / disease.clean.2010$Sum.2010[i]) 
}


# 2011
disease.clean.2011 <- disease.clean %>%
  filter(Year == 2011) %>% 
  arrange(Disease.Type)

disease.clean.2011$Percentage.2011 <- 0

for (i in 1:60) {
  disease.clean.2011$Sum.2011 <- sum(disease.clean.2011$NIPA.Dollars)
  disease.clean.2011$Percentage.2011[i] <- 
    (disease.clean.2011$NIPA.Dollars[i] / disease.clean.2011$Sum.2011[i]) 
}


# 2012
disease.clean.2012 <- disease.clean %>%
  filter(Year == 2012) %>% 
  arrange(Disease.Type)

disease.clean.2012$Percentage.2012 <- 0

for (i in 1:60) {
  disease.clean.2012$Sum.2012 <- sum(disease.clean.2012$NIPA.Dollars)
  disease.clean.2012$Percentage.2012[i] <- 
    (disease.clean.2012$NIPA.Dollars[i] / disease.clean.2012$Sum.2012[i]) 
}


# 2013
disease.clean.2013 <- disease.clean %>%
  filter(Year == 2013) %>% 
  arrange(Disease.Type)

disease.clean.2013$Percentage.2013 <- 0

for (i in 1:60) {
  disease.clean.2013$Sum.2013 <- sum(disease.clean.2013$NIPA.Dollars)
  disease.clean.2013$Percentage.2013[i] <- 
    (disease.clean.2013$NIPA.Dollars[i] / disease.clean.2013$Sum.2013[i]) 
}


# 2014
disease.clean.2014 <- disease.clean %>%
  filter(Year == 2014) %>% 
  arrange(Disease.Type)

disease.clean.2014$Percentage.2014 <- 0

for (i in 1:60) {
  disease.clean.2014$Sum.2014 <- sum(disease.clean.2014$NIPA.Dollars)
  disease.clean.2014$Percentage.2014[i] <- 
    (disease.clean.2014$NIPA.Dollars[i] / disease.clean.2014$Sum.2014[i]) 
}


# 2015
disease.clean.2015 <- disease.clean %>%
  filter(Year == 2015) %>% 
  arrange(Disease.Type)

disease.clean.2015$Percentage.2015 <- 0

for (i in 1:60) {
  disease.clean.2015$Sum.2015 <- sum(disease.clean.2015$NIPA.Dollars)
  disease.clean.2015$Percentage.2015[i] <- 
    (disease.clean.2015$NIPA.Dollars[i] / disease.clean.2015$Sum.2015[i]) 
}


# 2016
disease.clean.2016 <- disease.clean %>%
  filter(Year == 2016) %>% 
  arrange(Disease.Type)

disease.clean.2016$Percentage.2016 <- 0

for (i in 1:60) {
  disease.clean.2016$Sum.2016 <- sum(disease.clean.2016$NIPA.Dollars)
  disease.clean.2016$Percentage.2016[i] <- 
    (disease.clean.2016$NIPA.Dollars[i] / disease.clean.2016$Sum.2016[i]) 
}


# 2017
disease.clean.2017 <- disease.clean %>%
  filter(Year == 2017) %>% 
  arrange(Disease.Type)

disease.clean.2017$Percentage.2017 <- 0

for (i in 1:60) {
  disease.clean.2017$Sum.2017 <- sum(disease.clean.2017$NIPA.Dollars)
  disease.clean.2017$Percentage.2017[i] <- 
    (disease.clean.2017$NIPA.Dollars[i] / disease.clean.2017$Sum.2017[i]) 
}


# checking my work
sum(disease.clean.2000$Percentage.2000)
sum(disease.clean.2001$Percentage.2001)
sum(disease.clean.2002$Percentage.2002)
sum(disease.clean.2003$Percentage.2003)
sum(disease.clean.2004$Percentage.2004)
sum(disease.clean.2005$Percentage.2005)
sum(disease.clean.2006$Percentage.2006)
sum(disease.clean.2007$Percentage.2007)
sum(disease.clean.2008$Percentage.2008)
sum(disease.clean.2009$Percentage.2009)
sum(disease.clean.2010$Percentage.2010)
sum(disease.clean.2011$Percentage.2011)
sum(disease.clean.2012$Percentage.2012)
sum(disease.clean.2013$Percentage.2013)
sum(disease.clean.2014$Percentage.2014)
sum(disease.clean.2015$Percentage.2015)
sum(disease.clean.2016$Percentage.2016)
sum(disease.clean.2017$Percentage.2017)
# all percentages equal 1


# overall change in medical expenditure
cols <- c(1,6)
disease.clean.total <- disease.clean.2000[1:18,cols]
colnames(disease.clean.total)[2] <- "Sum.All"


# adding 2001
disease.clean.total[2,] <- disease.clean.2001[1,cols]


# adding 2002
disease.clean.total[3,] <- disease.clean.2002[1,cols]


# adding 2003
disease.clean.total[4,] <- disease.clean.2003[1,cols]


# adding 2004
disease.clean.total[5,] <- disease.clean.2004[1,cols]


# adding 2005
disease.clean.total[6,] <- disease.clean.2005[1,cols]


# adding 2006
disease.clean.total[7,] <- disease.clean.2006[1,cols]


# adding 2007
disease.clean.total[8,] <- disease.clean.2007[1,cols]


# adding 2008
disease.clean.total[9,] <- disease.clean.2008[1,cols]


# adding 2009
disease.clean.total[10,] <- disease.clean.2009[1,cols]


# adding 2010
disease.clean.total[11,] <- disease.clean.2010[1,cols]


# adding 2011
disease.clean.total[12,] <- disease.clean.2011[1,cols]


# adding 2012
disease.clean.total[13,] <- disease.clean.2012[1,cols]


# adding 2013
disease.clean.total[14,] <- disease.clean.2013[1,cols]


# adding 2014
disease.clean.total[15,] <- disease.clean.2014[1,cols]


# adding 2015
disease.clean.total[16,] <- disease.clean.2015[1,cols]


# adding 2016
disease.clean.total[17,] <- disease.clean.2016[1,cols]


# adding 2017
disease.clean.total[18,] <- disease.clean.2017[1,cols]



# disease type percentages of overall medical expenditure
disease.clean.percent <- cbind.data.frame(disease.clean.total$Year, 
                                          disease.clean.total$Year, 
                                          disease.clean.total$Year,
                                          disease.clean.total$Year,
                                          disease.clean.total$Year)
# the above dataframe creation was to create an 18 x 5 dataframe; values will be replaced


# rename columns in disease.clean.percent
colnames(disease.clean.percent) <- c("Year", "Communicable", "Noncommunicable", "Injury", "Other")


# finding indexes for each disease type
# indexes for disease type
Communicable.index <- grep("Communicable", 
                           disease.clean.2000$Disease.Type, 
                           value = FALSE, 
                           ignore.case = FALSE)
Noncommunicable.index <- grep("Noncommunicable",
                              disease.clean.2000$Disease.Type,
                              value = FALSE,
                              ignore.case = FALSE)
Injury.index <- grep("Injury",
                              disease.clean.2000$Disease.Type,
                              value = FALSE,
                              ignore.case = FALSE)
Other.index <- grep("Other",
                              disease.clean.2000$Disease.Type,
                              value = FALSE,
                              ignore.case = FALSE)


# adding 2000
disease.clean.percent[1,"Communicable"] <- sum(disease.clean.2000$Percentage.2000[Communicable.index])
disease.clean.percent[1,"Noncommunicable"] <- sum(disease.clean.2000$Percentage.2000[Noncommunicable.index])
disease.clean.percent[1,"Injury"] <- sum(disease.clean.2000$Percentage.2000[Injury.index])
disease.clean.percent[1,"Other"] <- sum(disease.clean.2000$Percentage.2000[Other.index])


# adding 2001
disease.clean.percent[2,"Communicable"] <- sum(disease.clean.2001$Percentage.2001[Communicable.index])
disease.clean.percent[2,"Noncommunicable"] <- sum(disease.clean.2001$Percentage.2001[Noncommunicable.index])
disease.clean.percent[2,"Injury"] <- sum(disease.clean.2001$Percentage.2001[Injury.index])
disease.clean.percent[2,"Other"] <- sum(disease.clean.2001$Percentage.2001[Other.index])


# adding 2002
disease.clean.percent[3,"Communicable"] <- sum(disease.clean.2002$Percentage.2002[Communicable.index])
disease.clean.percent[3,"Noncommunicable"] <- sum(disease.clean.2002$Percentage.2002[Noncommunicable.index])
disease.clean.percent[3,"Injury"] <- sum(disease.clean.2002$Percentage.2002[Injury.index])
disease.clean.percent[3,"Other"] <- sum(disease.clean.2002$Percentage.2002[Other.index])


# adding 2003
disease.clean.percent[4,"Communicable"] <- sum(disease.clean.2003$Percentage.2003[Communicable.index])
disease.clean.percent[4,"Noncommunicable"] <- sum(disease.clean.2003$Percentage.2003[Noncommunicable.index])
disease.clean.percent[4,"Injury"] <- sum(disease.clean.2003$Percentage.2003[Injury.index])
disease.clean.percent[4,"Other"] <- sum(disease.clean.2003$Percentage.2003[Other.index])


# adding 2004
disease.clean.percent[5,"Communicable"] <- sum(disease.clean.2004$Percentage.2004[Communicable.index])
disease.clean.percent[5,"Noncommunicable"] <- sum(disease.clean.2004$Percentage.2004[Noncommunicable.index])
disease.clean.percent[5,"Injury"] <- sum(disease.clean.2004$Percentage.2004[Injury.index])
disease.clean.percent[5,"Other"] <- sum(disease.clean.2004$Percentage.2004[Other.index])


# adding 2005
disease.clean.percent[6,"Communicable"] <- sum(disease.clean.2005$Percentage.2005[Communicable.index])
disease.clean.percent[6,"Noncommunicable"] <- sum(disease.clean.2005$Percentage.2005[Noncommunicable.index])
disease.clean.percent[6,"Injury"] <- sum(disease.clean.2005$Percentage.2005[Injury.index])
disease.clean.percent[6,"Other"] <- sum(disease.clean.2005$Percentage.2005[Other.index])


# adding 2006
disease.clean.percent[7,"Communicable"] <- sum(disease.clean.2006$Percentage.2006[Communicable.index])
disease.clean.percent[7,"Noncommunicable"] <- sum(disease.clean.2006$Percentage.2006[Noncommunicable.index])
disease.clean.percent[7,"Injury"] <- sum(disease.clean.2006$Percentage.2006[Injury.index])
disease.clean.percent[7,"Other"] <- sum(disease.clean.2006$Percentage.2006[Other.index])


# adding 2007
disease.clean.percent[8,"Communicable"] <- sum(disease.clean.2007$Percentage.2007[Communicable.index])
disease.clean.percent[8,"Noncommunicable"] <- sum(disease.clean.2007$Percentage.2007[Noncommunicable.index])
disease.clean.percent[8,"Injury"] <- sum(disease.clean.2007$Percentage.2007[Injury.index])
disease.clean.percent[8,"Other"] <- sum(disease.clean.2007$Percentage.2007[Other.index])


# adding 2008
disease.clean.percent[9,"Communicable"] <- sum(disease.clean.2008$Percentage.2008[Communicable.index])
disease.clean.percent[9,"Noncommunicable"] <- sum(disease.clean.2008$Percentage.2008[Noncommunicable.index])
disease.clean.percent[9,"Injury"] <- sum(disease.clean.2008$Percentage.2008[Injury.index])
disease.clean.percent[9,"Other"] <- sum(disease.clean.2008$Percentage.2008[Other.index])


# adding 2009
disease.clean.percent[10,"Communicable"] <- sum(disease.clean.2009$Percentage.2009[Communicable.index])
disease.clean.percent[10,"Noncommunicable"] <- sum(disease.clean.2009$Percentage.2009[Noncommunicable.index])
disease.clean.percent[10,"Injury"] <- sum(disease.clean.2009$Percentage.2009[Injury.index])
disease.clean.percent[10,"Other"] <- sum(disease.clean.2009$Percentage.2009[Other.index])


# adding 2010
disease.clean.percent[11,"Communicable"] <- sum(disease.clean.2010$Percentage.2010[Communicable.index])
disease.clean.percent[11,"Noncommunicable"] <- sum(disease.clean.2010$Percentage.2010[Noncommunicable.index])
disease.clean.percent[11,"Injury"] <- sum(disease.clean.2010$Percentage.2010[Injury.index])
disease.clean.percent[11,"Other"] <- sum(disease.clean.2010$Percentage.2010[Other.index])


# adding 2011
disease.clean.percent[12,"Communicable"] <- sum(disease.clean.2011$Percentage.2011[Communicable.index])
disease.clean.percent[12,"Noncommunicable"] <- sum(disease.clean.2011$Percentage.2011[Noncommunicable.index])
disease.clean.percent[12,"Injury"] <- sum(disease.clean.2011$Percentage.2011[Injury.index])
disease.clean.percent[12,"Other"] <- sum(disease.clean.2011$Percentage.2011[Other.index])


# adding 2012
disease.clean.percent[13,"Communicable"] <- sum(disease.clean.2012$Percentage.2012[Communicable.index])
disease.clean.percent[13,"Noncommunicable"] <- sum(disease.clean.2012$Percentage.2012[Noncommunicable.index])
disease.clean.percent[13,"Injury"] <- sum(disease.clean.2012$Percentage.2012[Injury.index])
disease.clean.percent[13,"Other"] <- sum(disease.clean.2012$Percentage.2012[Other.index])


# adding 2013
disease.clean.percent[14,"Communicable"] <- sum(disease.clean.2013$Percentage.2013[Communicable.index])
disease.clean.percent[14,"Noncommunicable"] <- sum(disease.clean.2013$Percentage.2013[Noncommunicable.index])
disease.clean.percent[14,"Injury"] <- sum(disease.clean.2013$Percentage.2013[Injury.index])
disease.clean.percent[14,"Other"] <- sum(disease.clean.2013$Percentage.2013[Other.index])


# adding 2014
disease.clean.percent[15,"Communicable"] <- sum(disease.clean.2014$Percentage.2014[Communicable.index])
disease.clean.percent[15,"Noncommunicable"] <- sum(disease.clean.2014$Percentage.2014[Noncommunicable.index])
disease.clean.percent[15,"Injury"] <- sum(disease.clean.2014$Percentage.2014[Injury.index])
disease.clean.percent[15,"Other"] <- sum(disease.clean.2014$Percentage.2014[Other.index])


# adding 2015
disease.clean.percent[16,"Communicable"] <- sum(disease.clean.2015$Percentage.2015[Communicable.index])
disease.clean.percent[16,"Noncommunicable"] <- sum(disease.clean.2015$Percentage.2015[Noncommunicable.index])
disease.clean.percent[16,"Injury"] <- sum(disease.clean.2015$Percentage.2015[Injury.index])
disease.clean.percent[16,"Other"] <- sum(disease.clean.2015$Percentage.2015[Other.index])


# adding 2016
disease.clean.percent[17,"Communicable"] <- sum(disease.clean.2016$Percentage.2016[Communicable.index])
disease.clean.percent[17,"Noncommunicable"] <- sum(disease.clean.2016$Percentage.2016[Noncommunicable.index])
disease.clean.percent[17,"Injury"] <- sum(disease.clean.2016$Percentage.2016[Injury.index])
disease.clean.percent[17,"Other"] <- sum(disease.clean.2016$Percentage.2016[Other.index])


# adding 2017
disease.clean.percent[18,"Communicable"] <- sum(disease.clean.2017$Percentage.2017[Communicable.index])
disease.clean.percent[18,"Noncommunicable"] <- sum(disease.clean.2017$Percentage.2017[Noncommunicable.index])
disease.clean.percent[18,"Injury"] <- sum(disease.clean.2017$Percentage.2017[Injury.index])
disease.clean.percent[18,"Other"] <- sum(disease.clean.2017$Percentage.2017[Other.index])


# calculating total medical expenditure for each disease type
disease.clean.total$Sum.Communicable <- disease.clean.total$Sum.All * disease.clean.percent$Communicable
disease.clean.total$Sum.Noncommunicable <- disease.clean.total$Sum.All * disease.clean.percent$Noncommunicable
disease.clean.total$Sum.Injury <- disease.clean.total$Sum.All * disease.clean.percent$Injury
disease.clean.total$Sum.Other <- disease.clean.total$Sum.All * disease.clean.percent$Other


# changing column names in disease.clean.total
colnames(disease.clean.total) <- c("Year", 
                                   "Total", 
                                   "Communicable", 
                                   "Noncommunicable", 
                                   "Injury", 
                                  "Other")


# 6. data visualization ####

# total expenditure graph
ggplot(data = disease.clean.total,
                      aes(x = Year, y = Total)) +
  geom_line(color = "purple") +
  xlab("Year") +
  ylab("Total Expenditure (in dollars)") +
  ggtitle("Total Medical Expenditure in the United States 2000-2017") +
  theme_bw(base_size = 12) +
  scale_x_discrete(limits = 2000:2017)


# disease type graphs
# communicable disease
Communicable.graph <- ggplot(data = disease.clean.total,
                             aes(x = Year, y = Communicable)) +
                             geom_line(color = "blue") +
                             xlab("Year") +
                             ylab("Total Expenditure (in dollars)") +
                             theme_bw(base_size = 12) +
            ggtitle("Communicable Expenditure 2000-2017")


# noncommunicable disease
Noncommunicable.graph <- ggplot(data = disease.clean.total,
                             aes(x = Year, y = Noncommunicable)) +
  geom_line(color = "green") +
  xlab("Year") +
  ylab("Total Expenditure (in dollars)") +
  theme_bw(base_size = 12) +
  ggtitle("Noncommunicable Expenditure 2000-2017")


# injury
Injury.graph <- ggplot(data = disease.clean.total,
                             aes(x = Year, y = Injury)) +
  geom_line(color = "red") +
  xlab("Year") +
  ylab("Total Expenditure (in dollars)") +
  theme_bw(base_size = 12) +
  ggtitle("Expenditure on Injuries 2000-2017")


# other
Other.graph <- ggplot(data = disease.clean.total,
                       aes(x = Year, y = Other)) +
  geom_line(color = "orange") +
  xlab("Year") +
  ylab("Total Expenditure (in dollars)") +
  theme_bw(base_size = 12) +
  ggtitle(" Unspecified Expenditure 2000-2017")


# combining disease type graphs
ggarrange(Communicable.graph, Noncommunicable.graph, Injury.graph, Other.graph)


# percentage table editing
  # creating percentages and rounding
disease.clean.percent$Communicable <- disease.clean.percent$Communicable * 100 %>% 
  round(digits = 2)
disease.clean.percent$Noncommunicable <- disease.clean.percent$Noncommunicable * 100 %>% 
  round(digits = 2)
disease.clean.percent$Injury <- disease.clean.percent$Injury * 100 %>% 
  round(digits = 2)
disease.clean.percent$Other <- disease.clean.percent$Other * 100 %>% 
  round(digits = 2)


# creating tables
  # total expenditure table
disease.clean.total %>% 
  gt(rowname_col = "Year") %>% 
  tab_header(
    title = "Medical Expenditure in the United States from 2000 to 2017",
    subtitle = "in Dollars"
  ) %>% 
  tab_spanner(
    label = "Disease Type",
    columns = vars(Communicable, Noncommunicable, Injury, Other)
  ) %>%
  fmt_number(columns = vars(Total, Communicable, Noncommunicable, Injury, Other),
             suffixing = TRUE) %>% 
  tab_source_note(source_note = "Source: Bureau of Economic Analysis Blended Account, 2020")


  # percentage table
disease.clean.percent %>% 
  gt(rowname_col = "Year") %>% 
  tab_header(
    title = "Medical Expenditure in the United States from 2000 to 2017 Proportioned by Disease Type",
    subtitle = "as a Percentage"
  ) %>% 
  fmt_number(columns = vars(Communicable, Noncommunicable, Injury, Other),
             decimals = 2) %>% 
  tab_source_note(source_note = "Source: Bureau of Economic Analysis Blended Account, 2020")
