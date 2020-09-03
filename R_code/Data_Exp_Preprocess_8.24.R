#Set Working Directory
getwd()



#Load Packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(esquisse)
library(haven)
library(stringr)
library(broom)
library(summarytools)


#Read in data and save it in df object (original data for comparison)
dataFile = '/mnt/workspace/GWTG/COVID19/data/covid19_cvd_aug20.sas7bdat'
formatsFile = '/mnt/workspace/GWTG/COVID19/data/R_Python_windows_formats.sas7bcat'

dfor <- read_sas(dataFile, formatsFile)

#Get Dimensions and Var names
dim(dfor)
names(dfor)

#Capitalize Var names 

str_to_upper("hgbadm, wbcadm, platelet, initscr, tropadm,antihyprtnsv,liplowthrp,antiplt, anticoag, antihyperglym")


#Select Working Variables #ORIG DATA SET DOES NOT HAVE PSOURCE 08

dft1 <- df_orig %>% dplyr::select(CASE_ID,SEX,AGEi,
                                  PSOURCE_01,PSOURCE_02, PSOURCE_03, 
                                  PSOURCE_04, PSOURCE_05, PSOURCE_06,
                                  PSOURCE_07, PSOURCE_09,
                                  RACE_01, RACE_02, RACE_03, RACE_04,
                                  RACE_05, RACE_06, ASIAN_01,
                                  ASIAN_02, ASIAN_03, ASIAN_04, 
                                  ASIAN_05, ASIAN_06, ASIAN_07,
                                  HAWAIIAN_01, HAWAIIAN_02, HAWAIIAN_03,
                                  HAWAIIAN_04, ETHNICYS_01, ETHNICYS_02,
                                  ETHNICYS_03, ETHNICYS_04,
                                  HGBADM, WBCADM, PLATELET,
                                  INITSCR, TROPADM,ANTIHYPRTNSV,
                                  LIPLOWTHRP,ANTIPLT, ANTICOAG,
                                  ANTIHYPRGLYM, HISETHNI,DDMER,
                                  TROPADM, INITSCR, COVCLINTRIAL,
                                  MEDHISTO_01, MEDHISTO_02, MEDHISTO_08,
                                  MEDHISTO_09,MEDHISTO_11, MEDHISTO_12)

dt<-ExpCTable(dft1,Target=NULL,clim=5,nlim=15,round=2,bin=NULL,per=F)

kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#Numerical Summary by Var
dtnum<-ExpCustomStat(dft1,Cvar = c("COVCLINTRIAL"), Nvar=c("AGEi",  "HGBADM", "WBCADM", "PLATELET",
                                                       "INITSCR", "TROPADM","ANTIHYPRTNSV",
                                                       "LIPLOWTHRP","ANTIPLT", "ANTICOAG",
                                                       "ANTIHYPRGLYM", "HISETHNI", "DDMER",

                                                                                                              "TROPADM", "INITSCR"), stat = c('Count','Prop','mean','min','P0.25','median','p0.75','max'),gpby=FALSE)
#kable dtum

kable(dtnum) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#Summary of Data Frame
summary(as.factor(df$METHDIAG))
summary(as.factor(dft1$MEDHISTO_01))


summary(as.factor(df$METHDIAG))
#Quick proportions 


#Overall Proportion of Enrollment in Clinical Trials
prop.table(table(dft1$HISETHNI))

#Vectorizing Var names to iterate over later 
namevars<- names(dft1)

#Plot Disrtibutions of Vars
long_data <- melt(dft1, id.vars = "CASE_ID", variable.name = "Var")


#Visualize Distributions 

library(ggplot2)

#Age for the entire df 
ggplot(dft1) +
 aes(x = AGEi) +
 geom_histogram(bins = 100L, fill = "#6dcd59") +
 labs(x = "Age", y = "Count") +
 theme_classic()

library(dplyr)
library(ggplot2)

#Age comparison Enrolled vs Not-Enrolled
dft1 %>%
 filter(!is.na(SEX)) %>%
 filter(!is.na(AGEi)) %>%
 filter(!is.na(COVCLINTRIAL)) %>%
 ggplot() +
 aes(x = AGEi, fill = SEX) +
 geom_histogram(bins = 18L, position = "dodge") +
 scale_fill_hue() +
 labs(x = "Age", y = "Count", title = "Enrolled/Not Enrolled", fill = "Sex") +
 theme_classic() +
 facet_wrap(vars(COVCLINTRIAL), scales = "free_y")


#Enrolled/Not Enrroled by Sex
dft1 %>%
 filter(SEX %in% c("1", "2")) %>%
 filter(!is.na(AGEi)) %>%
 filter(!is.na(COVCLINTRIAL)) %>%
 ggplot() +
 aes(x = SEX, fill = SEX) +
 geom_bar(position = "dodge") +
 scale_fill_hue() +
 labs(x = "Sex", y = "Count", title = "Enrolled/Not Enrolled") +
 facet_wrap(vars(COVCLINTRIAL), scales = "free_y")+
theme_classic()

#Sex Count entire df
dft1 %>%
 filter(!is.na(SEX)) %>%
 filter(!is.na(AGEi)) %>%
 filter(!is.na(HISETHNI)) %>%
 
    filter(!is.na(COVCLINTRIAL)) %>%
 ggplot() +
 aes(x = SEX, fill = SEX) +
 geom_bar(position = "dodge") +
 scale_fill_hue() +
 labs(x = "Sex", y = "Count", fill = "Sex") +
 theme_classic()

#Count of Hispanic Ethnicity entire sample
dft1 %>%
 filter(!is.na(SEX)) %>%
 filter(!is.na(AGEi)) %>%
 filter(!is.na(HISETHNI)) %>%
 
    filter(!is.na(COVCLINTRIAL)) %>%
 ggplot() +
 aes(x = HISETHNI) +
 geom_bar(position = "dodge", fill = "#35b779") +
 labs(x = "Hispanic Ethnicity (Yes/No)", y = "Count", title = "Hispanic Ethnicity (Yes/No)") +
 theme_classic()

#Enrolled and Not Enrolled Hispanic/Not Hispanic
dft1 %>%
 filter(!is.na(SEX)) %>%
 filter(!is.na(AGEi)) %>%
 filter(!is.na(HISETHNI)) %>%
 
    filter(!is.na(COVCLINTRIAL)) %>%
 ggplot() +
 aes(x = HISETHNI, fill = SEX) +
 geom_bar(position = "dodge") +
 scale_fill_hue() +
 labs(x = "Hispanic Ethnicity (Yes/No)", y = "Count", title = "Enrolled/Not Enrolled", fill = "Sex") +
 theme_classic() +
 facet_wrap(vars(COVCLINTRIAL))


