##  PACKAGES NEEDED
library(readxl)
library(lavaan)
library(psych)
library(corrplot)
library(semTools) # for additional functions in SEM
library(semPlot) # for path

#LOAD DATA
getwd()
setwd("D:/MA ED Measurement Evaluation and Assessment")
NSD2687_1_no_1 <- read_excel("THESIS 2022/R scripts/NSD2687-1-no 1.xlsx")
#View(NSD2687_1_no_1)

#DATA PREPARATION

Data<-NSD2687_1_no_1
preli.df<-Data[c(6,13,14,93:102)]
apply(preli.df[1:13],2,table,exclude=NULL)#A glance at the subset data
preli.df[1:13][preli.df[1:13]==9999]<-NA # change 9999 to NAs
preli.df[1:13][preli.df[1:13]==999]<-NA # change 999 to NAs

#  SUBSET 6 STUDY PROGRAMS
AKADM<-preli.df[preli.df$Utd_type == 'Ã˜KADM',]   # business and adminstration-Enterprising/Ã˜KADM/
DATAIT<-preli.df[preli.df$Utd_type == 'DATA-IT',] # Conventional
KUNST<-preli.df[preli.df$Utd_type == 'KUNST',]    # Artistic
SIVING<-preli.df[preli.df$Utd_type == 'SIVING',]  # Investigative
SYKEPLEIE<-preli.df[preli.df$Utd_type == 'SYKEPLEIE',] #Social
TEKN.FAG<-preli.df[preli.df$Utd_type == 'TEKN-FAG',]   #Realistic

# New Dataset
#Put all data frames into list
list.dfprel <- list(SIVING, TEKN.FAG, KUNST,SYKEPLEIE,AKADM,DATAIT)

#Merge all data frames in list
ANALYSIS.DATApreli<-Reduce(function(x, y) merge(x, y, all=TRUE), list.dfprel)
#Remove NAs
DF.ANALYSIS<-ANALYSIS.DATApreli[complete.cases(ANALYSIS.DATApreli[1:13]),]

#Subset LO items
data2<-DF.ANALYSIS[c(4:13)]
apply(data1[1:10],2,table,exclude=NULL)#A glance at the subset data

## CFA Data
CFA_DATA<-data2
#############################################
## STEP 1. DATA SCREENING
#############################################

#Descriptives

describe(DF.ANALYSIS)

describe(CFA_DATA)

#Multivariate statistics*
  
mardia(CFA_DATA) #multivariate skweness and kurtosis

#b1p =  5.89   skew =  6628.6  with probability  <=  0
#b2p =  156.93   kurtosis =  97.98  with probability <=  0
#Data has a non normal distribution. Thus use robust estimator

##################################################
# STEP 2.**SPECIFY AND FIT CFA MODELS**
##################################################
#Fit CFA using Robust diagonally weighted least squares method (DWLS)- for ordinal, non normal data data**
#One factor
unimodel_DW <- "Learning_Outcomes =~ Laerutb_teori_13 + Laerutb_metforsk_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13"

unimodel_DW_fit <- lavaan::cfa(unimodel_DW, CFA_DATA,       
                               ordered=names(CFA_DATA))

summary(unimodel_DW_fit, fit.measures = T, standardized = T)
fitMeasures(unimodel_DW_fit, c("gfi","cfi","tli","rmsea","srmr"))

#gfi   cfi   tli rmsea  srmr 
#0.979 0.963 0.953 0.141 0.087
#################################
#mi
modindices(unimodel_DW_fit)

#**Suggested Changes*
#Laerutb_metforsk_13	~~	Laerutb_egenerf_13	2721.104	
#Laerutb_samarb_13	~~	Laerutb_muntkom_13	837.873	

#UnStandardized Residual matrix
lavResiduals(unimodel_DW_fit)# "cor.bentler" table-cases of above 0.1

#**Laerutb_metforsk_13- has problems with six out of ten items- could be similar to egenerf*

#Revision DW1- Remove metforsk
dw.data<-CFA_DATA[c(1,3:10)] #) 9items

#One factor
unimodel_DW1 <- "Learning_Outcomes =~ Laerutb_teori_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13"

unimodel_DW1_fit <- lavaan::cfa(unimodel_DW1, dw.data,       
                                ordered=names(dw.data))

summary(unimodel_DW1_fit, fit.measures = T, standardized = T)
fitMeasures(unimodel_DW1_fit, c("gfi","cfi","tli","rmsea","srmr"))
#gfi   cfi   tli rmsea  srmr 
#0.992 0.985 0.980 0.086 0.054 

#**Fit is marginal**

################################################################################
#Revision DW 2- covary muntkom and samarbeid
unimodel_DW2 <- "Learning_Outcomes =~ Laerutb_teori_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13
#covariance
Laerutb_samarb_13~~Laerutb_muntkom_13"

unimodel_DW2_fit <- lavaan::cfa(unimodel_DW2, dw.data,       
                                ordered=names(dw.data))
summary(unimodel_DW2_fit, fit.measures = T, standardized = T)
fitMeasures(unimodel_DW2_fit, c("gfi","cfi","tli","rmsea","srmr"))

#gfi   cfi   tli rmsea  srmr 
#0.995 0.991 0.987 0.070 0.045 
#** This model Looks better: however, chi still significant and robust RMSEA still**
#**above the cutoff of 0.08**

################################################################################  
  ##**TRY TWO FACTORS**
################################################################################  
#Two factor (10 items)
Model2F_DWL <- "Skills Achievement =~ Laerutb_samarb_13+Laerutb_muntkom_13+
                                     Laerutb_refleks_13 + Laerutb_tenke_13+
                                     Laerutb_skriftkom_13+Laerutb_selvst_13+
                                     Laerutb_fagspes_13
               Knowledge Achievement =~ Laerutb_teori_13 +Laerutb_metforsk_13+
                                     Laerutb_egenerf_13 "

Model2F_DWL_fit <-lavaan:: cfa(Model2F_DWL, CFA_DATA,       
                             ordered = names(CFA_DATA))

summary(Model2F_DWL_fit, standardized = TRUE, fit.measures = TRUE,rsquare = TRUE)
fitMeasures(Model2F_DWL_fit, c("gfi","cfi","tli","rmsea","srmr"))
#gfi   cfi   tli rmsea  srmr 
#0.991 0.985 0.980 0.093 0.058 

#**Local fit*
#*Mi
modindices(Model2F_DWL_fit)

#**Recommended changes*
#*Laerutb_metforsk_13 ~~Laerutb_egenerf_13 546.107
#*Laerutb_samarb_13~~Laerutb_muntkom_13 548.764 
#*SkillsAchievement =~     Laerutb_teori_13 546.098

#UnStandardized Residual matrix
lavResiduals(Model2F_DWL_fit)# "cor.bentler" table-cases of above 0.1
"Factor model revision"

Model2F_DWLrev <- "Skills Achievement =~ Laerutb_samarb_13+Laerutb_muntkom_13+
                                     Laerutb_refleks_13 + Laerutb_tenke_13+
                                     Laerutb_skriftkom_13+Laerutb_selvst_13+
                                     Laerutb_fagspes_13
               Knowledge Achievement =~ Laerutb_teori_13 +Laerutb_metforsk_13+
                                     Laerutb_egenerf_13 
              #Covariance
             Laerutb_samarb_13~~Laerutb_muntkom_13"

Model2F_DWLrev_fit <-lavaan:: cfa(Model2F_DWLrev, CFA_DATA,       
                               ordered = names(CFA_DATA))

summary(Model2F_DWLrev_fit, standardized = TRUE, fit.measures = TRUE,rsquare = TRUE)
fitMeasures(Model2F_DWLrev_fit, c("gfi","cfi","tli","rmsea","srmr"))
#gfi   cfi   tli rmsea  srmr 
#0.994 0.989 0.985 0.080 0.051 
#**RMSEA still above cutpoint*
#*
lavResiduals(Model2F_DWLrev_fit)# "cor.bentler" table-cases of above 0.1

