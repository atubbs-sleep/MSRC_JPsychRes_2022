###### Load Packages #####
library(tidyverse)
library(gtsummary)
library(excel.link)
library(ggpubr)
library(mice)
library(parallel)
library(sandwich)
library(lmtest)
library(mediation)
library(glmnet)
library(doParallel)
library(foreach)
library(lavaan)
library(semPlot)
library(snow)
library(semptools)
library(mitml)
library(miceadds)
library(wec)
library(cowplot)

###### Load and organize data #####
msrc <- read_csv("c:/users/atala/sync/research/data/msrc/msrcdata.csv",
                 na=c("#NULL!","999.00","888.00","777.00",999,888,998),
                 guess_max = 6556) %>%
  mutate(Combat_experience = if_else(Combat_experience==999 | Combat_experience==888, NA_real_,Combat_experience)) %>% 
  transmute(
  "PatientType" = factor(case_when(Sample_type == 1 & Patient_status == 1 ~ "Non-Patient",
                                  Sample_type == 1 & Patient_status == 4 ~ "Outpatient",
                                  Sample_type == 1 & Patient_status == NA_real_ ~ NA_character_,
                                  Sample_type == 2 & Patient_status == 2 ~ "Outpatient",
                                  Sample_type == 2 & Patient_status == 3 ~ "Inpatient",
                                  Sample_type == 2 & Patient_status == 4 ~ NA_character_,
                                  Sample_type == 2 & Patient_status == 5 ~ NA_character_,
                                  Sample_type == 3 & Patient_status == 3 ~ "Inpatient",
                                  Sample_type == 3 & Patient_status == 4 ~ NA_character_),
                     levels = c("Non-Patient","Outpatient","Inpatient")),
  "Age" = Age,
  "Sex" = factor(Gender, levels = 1:2, labels = c("Male","Female")),
  "Race" = factor(Race, levels = 1:7, labels = c("White","Black","AIAN","Asian","Asian","Multiracial","Other")),
  "Ethnicity" = factor(Ethnicity, levels = 1:2, labels = c("Hispanic","Non-Hispanic")),
  "Education" = factor(EducationJT, levels = 0:4, labels=c("Less than HS","HS","Some college","College","More than college")),
  "Marital" = factor(Relationship_Status, c(1:5,7), c("Married","Single","Cohabitating","Widowed","Divorced","Other")),
  "Military" = factor(Any_Service, 1:4, c("Military","Military","Civilian","Military")),
  "Veteran" = factor(case_when(Military=="Civilian" ~ "Civilian",
                               Any_Service==2 ~ "Yes",
                               Any_Service==1 ~ "No",
                               Any_Service==4 ~ NA_character_),
                     levels = c("Civilian","No","Yes")),
  "Deployment" = factor(case_when(Military=="Civilian" ~ "Civilian",
                                  Deployment==2 ~ "No",
                                  Deployment==1 ~ "Yes"),
                        levels = c("Civilian","No","Yes")),
  "ServiceBranch" = factor(case_when(Military == "Civilian" ~ "Civilian",
                                     Military_Branch %in% 1:3 ~ "Army",
                                     Military_Branch %in% 4:6 ~ "Air Force",
                                     Military_Branch %in% 7:8 ~ "Navy",
                                     Military_Branch %in% 9:10 ~ "Marines",
                                     Military_Branch %in% 11:12 ~ "Coast Guard",
                                     Military_Branch == 13 ~ NA_character_),
                           levels = c("Civilian","Army","Air Force","Navy","Marines","Coast Guard")),
  "ServiceType" = factor(case_when(Military=="Civilian" ~ "Civilian",
                                   Military_Branch %in% c(1,4,7,9,11) | ActiveDuty==1 ~ "Active Duty",
                                   Military_Branch %in% c(2,5,8,10,12) ~ "Reserves",
                                   Military_Branch %in% c(3,6) ~ "National Guard"),
                         levels = c("Civilian","Reserves","National Guard","Active Duty")),
  "Combat" = factor(case_when(Military=="Civilian"~"Civilian",
                              Combat_experience==2 ~ "No",
                              Combat_experience==1 ~ "Yes"),
                    levels = c("Civilian","No","Yes")),
  "DSISS_Thoughts" = CDE1,
  "DSISS_Plan" = CDE2,
  "DSISS_Control" = CDE3,
  "DSISS_Impulse" = CDE4,
  "SBQ_1" = case_when(CDE5==1 | CDE5_Schmidt==1 ~ 1,
                      CDE5==2 | CDE5_Schmidt==2 ~ 2,
                      CDE5==3 | CDE5_Schmidt==3 | CDE5==4 ~ 3,
                      CDE5==5 | CDE5_Schmidt==4 | CDE5==6 | CDE5==7 ~ 4),
  "SBQ_2" = case_when(CDE6==0 | CDE6_Schmidt==1 ~ 1,
                      CDE6==1 | CDE6_Schmidt==2 ~ 2,
                      CDE6==2 | CDE6_Schmidt==3 ~ 3,
                      CDE6==3 | CDE6_Schmidt==4 ~ 4,
                      CDE6==4 | CDE6_Schmidt==5 ~ 4),
  "SBQ_3" = case_when(CDE7==1 | CDE7_Schmidt==1 ~ 1,
                      CDE7==2 | CDE7==3 |CDE7_Schmidt==2 ~ 2,
                      CDE7==4 | CDE7==5 |CDE7_Schmidt==3 ~ 3),
  "SBQ_4" = CDE8,
  "SIS_1" = CDE9,
  "SIS_2" = CDE10,
  "SIS_3" = CDE11,
  "SIS_4" = CDE12,
  "nSuicAttempts" = if_else(ceiling(CDE13)>10,10,ceiling(CDE13)),
  "SAMedicalCare" = factor(CDE16, 0:4, c("None","ER","ER","Hospital","ICU")),
  "NSSI" = if_else(CDE17>30,30,CDE17),
  "KnowSuicide" = CDE57,
  "SuicideinMilitary" = case_when(CDE57a==0 ~ 0, CDE57a==1~1, CDE57a==3 ~ NA_real_),
  "Howcloserelationship" = ceiling(CDE57c),
  "EffectofSuicide" = case_when(CDE57d==1 | CDE57d_CAST==1 ~ 1,
                                CDE57d==2 | CDE57d_CAST==2 ~ 2,
                                CDE57d==3 | CDE57d_CAST==3 ~ 3,
                                CDE57d==4 | CDE57d_CAST==4 ~ 4,
                                CDE57d==5 | CDE57d_CAST==5 ~ 5),
  "INQ_1" = INQ1,
  "INQ_2" = INQ2,
  "INQ_3" = INQ3,
  "INQ_4" = INQ4,
  "INQ_5" = INQ5,
  "INQ_6" = INQ6,
  "INQ_7" = 8-if_else(is.na(CDE24)==F,CDE24,INQ7),
  "INQ_8" = 8-if_else(is.na(CDE25)==F,CDE24,INQ8),
  "INQ_9" = INQ9,
  "INQ_10" = 8-if_else(is.na(CDE28)==F, CDE28, 
                    if_else(is.na(CDE28INQ)==F,CDE28INQ,INQ10)),
  "INQ_11" = INQ11,
  "INQ_12" = INQ12,
  "INQ_13" = 8-if_else(is.na(CDE26)==F,CDE26,INQ13),
  "INQ_14" = 8-if_else(is.na(CDE27)==F,CDE27,INQ14),
  "INQ_15" = 8-INQ15,
  "BSS_1" = if_else(is.na(SSI1)==F,SSI1,if_else(is.na(CDE19)==F,CDE19,BSS1)),
  "BSS_2" = if_else(is.na(SSI2)==F,SSI2,if_else(is.na(CDE20)==F,CDE20,BSS2)),
  "BSS_3" = if_else(is.na(SSI3)==F,SSI3,BSS3),
  "BSS_4" = if_else(is.na(SSI4)==F,SSI4,BSS4),
  "BSS_5" = if_else(is.na(SSI5)==F,SSI5,BSS5),
  "BSS_6" = if_else(is.na(SSI6)==F,SSI6,BSS6),
  "BSS_7" = if_else(is.na(SSI7)==F,SSI7,BSS7),
  "BSS_8" = if_else(is.na(SSI8)==F,SSI8,BSS8),
  "BSS_9" = if_else(is.na(SSI9)==F,SSI9,BSS9),
  "BSS_10" = if_else(is.na(SSI10)==F,SSI10,BSS10),
  "BSS_11" = if_else(is.na(SSI11)==F,SSI11,BSS11),
  "BSS_12" = if_else(is.na(SSI12)==F,SSI12,BSS12),
  "BSS_13" = if_else(is.na(SSI13)==F,SSI13,BSS13),
  "BSS_14" = if_else(is.na(SSI14)==F,SSI14,BSS14),
  "BSS_15" = if_else(is.na(SSI15)==F,SSI15,BSS15),
  "BSS_16" = if_else(is.na(SSI16)==F,SSI16,BSS16),
  "BSS_17" = if_else(is.na(SSI17)==F,SSI17,BSS17),
  "BSS_18" = if_else(is.na(SSI18)==F,SSI18,BSS18),
  "BSS_19" = if_else(is.na(SSI19)==F,SSI19,BSS19),
  "BSS_20" = BSS20,
  "BSS_21" = BSS21,
  "BDI_1" = BDI1, "BDI_2" = BDI2, "BDI_3" = BDI3, "BDI_4" = BDI4, "BDI_5" = BDI5,
  "BDI_6" = BDI6, "BDI_7" = BDI7, "BDI_8" = BDI8, "BDI_9" = BDI9, "BDI_10" = BDI10,
  "BDI_11" = BDI11, "BDI_12" = BDI12, "BDI_13" = BDI13, "BDI_14" = BDI14, "BDI_15" = BDI15,
  "BDI_16" = BDI16, "BDI_17" = BDI17, "BDI_18" = BDI18, "BDI_19" = BDI19, "BDI_20" = BDI20, "BDI_21" = BDI21,
  "BHS_1" = BHS1, "BHS_2" = BHS2, "BHS_3" = BHS3, "BHS_4" = BHS4, "BHS_5" = BHS5, "BHS_6" = BHS6, "BHS_7" = BHS7,
  "BHS_8" = if_else(is.na(CDE21)==F, CDE21, BHS8),
  "BHS_9" = BHS9, "BHS_10" = BHS10, "BHS_11" = BHS11, 
  "BHS_12" = if_else(is.na(CDE22)==F, CDE22, BHS12),
  "BHS_13" = BHS13, 
  "BHS_14" = if_else(is.na(CDE23)==F, CDE23, BHS14),
  "BHS_15" = BHS15, "BHS_16" = BHS16, "BHS_17" = BHS17, "BHS_18" = BHS18, "BHS_19" = BHS19, "BHS_20" = BHS20,
  "ACSS_1" = ACSS1, "ACSS_2" = ACSS2,"ACSS_3" = ACSS3,"ACSS_4" = ACSS4,"ACSS_5" = ACSS5,"ACSS_6" = ACSS6,"ACSS_7" = ACSS7,
  "ACSS_8" = ACSS8, "ACSS_9" = ACSS9,"ACSS_10" = ACSS10,"ACSS_11" = ACSS11,"ACSS_12" = ACSS12,"ACSS_13" = ACSS13,
  "ACSS_14" = if_else(is.na(ACSS14)==F,ACSS14,4-ACSS14_PI24),
  "ACSS_15" = ACSS15,"ACSS_16" = ACSS16,"ACSS_17" = ACSS17,"ACSS_18" = ACSS18,
  "ACSS_19" = ACSS19,"ACSS_20" = ACSS20, 
  "AUDIT_1" = if_else(is.na(CDE46)==F,CDE46,AUDIT1),
  "AUDIT_2" = if_else(is.na(CDE47)==F,CDE47,AUDIT2),
  "AUDIT_3" = if_else(is.na(CDE48)==F,CDE48,AUDIT3),
  "PCLversion" = factor(CDE_PCL_Instruct,1:2,c("Military","Civilian")),
  "PCL_1" = if_else(is.na(CDE34)==F,CDE34,PCLM1),
  "PCL_2" = if_else(is.na(CDE35)==F,CDE35,PCLM2),
  "PCL_3" = if_else(is.na(CDE36)==F,CDE36,PCLM3),
  "PCL_4" = PCLM4,
  "PCL_5" = if_else(is.na(CDE37)==F,CDE37,PCLM5),
  "PCL_6" = if_else(is.na(CDE38)==F,CDE38,PCLM6),
  "PCL_7" = if_else(is.na(CDE39)==F,CDE39,PCLM7),
  "PCL_8" = PCLM8,
  "PCL_9" = PCLM9,
  "PCL_10" = PCLM10,
  "PCL_11" = PCLM11,
  "PCL_12" = PCLM12,
  "PCL_13" = PCLM13,
  "PCL_14" = PCLM14,
  "PCL_15" = PCLM15,
  "PCL_16" = if_else(is.na(CDE40)==F,CDE40,PCLM16),
  "PCL_17" = if_else(is.na(CDE41)==F,CDE41,PCLM17),
  "TBI_1" = CDE42,
  "TBI_2" = CDE43,
  "TBI_3" = CDE44,
  "TBI_4" = CDE45,
  "ISIFalling" = CDE52,
  "ISIStaying" = CDE53,
  "ISIEMA" = CDE54,
  "ISISatisfied" = CDE55,
  "ISIInterfere" = CDE56,
  "ASI_1" = case_when(CDE29==1 | CDE29_7point==1 ~ 1,
                      CDE29==2 | CDE29_7point==2 | CDE29_7point==3 ~ 2,
                      CDE29==3 | CDE29_7point==4 ~ 3,
                      CDE29==4 | CDE29_7point==5 | CDE29_7point==6 ~ 4,
                      CDE29==5 | CDE29_7point==7 ~ 5),
  "ASI_2" = case_when(CDE30==1 | CDE30_7point==1 ~ 1,
                      CDE30==2 | CDE30_7point==2 | CDE30_7point==3 ~ 2,
                      CDE30==3 | CDE30_7point==4 ~ 3,
                      CDE30==4 | CDE30_7point==5 | CDE30_7point==6 ~ 4,
                      CDE30==5 | CDE30_7point==7 ~ 5),
  "ASI_3" = case_when(CDE31==1 | CDE31_7point==1 ~ 1,
                      CDE31==2 | CDE31_7point==2 | CDE31_7point==3 ~ 2,
                      CDE31==3 | CDE31_7point==4 ~ 3,
                      CDE31==4 | CDE31_7point==5 | CDE31_7point==6 ~ 4,
                      CDE31==5 | CDE31_7point==7 ~ 5),
  "ASI_4" = case_when(CDE32==1 | CDE32_7point==1 ~ 1,
                      CDE32==2 | CDE32_7point==2 | CDE32_7point==3 ~ 2,
                      CDE32==3 | CDE32_7point==4 ~ 3,
                      CDE32==4 | CDE32_7point==5 | CDE32_7point==6 ~ 4,
                      CDE32==5 | CDE32_7point==7 ~ 5),
  "ASI_5" = case_when(CDE33==1 | CDE33_7point==1 ~ 1,
                      CDE33==2 | CDE33_7point==2 | CDE33_7point==3 ~ 2,
                      CDE33==3 | CDE33_7point==4 ~ 3,
                      CDE33==4 | CDE33_7point==5 | CDE33_7point==6 ~ 4,
                      CDE33==5 | CDE33_7point==7 ~ 5)) %>% 
  mutate(
    "DSISS_Total" = DSISS_Thoughts+DSISS_Plan+DSISS_Control+DSISS_Impulse,
    "DSISS_Cat" = if_else(DSISS_Total<4,0,1),
    "SBQ_Total" = SBQ_1+SBQ_2+SBQ_3+SBQ_4,
    "SBQ_Cat" = if_else(SBQ_Total<10,0,1),
    "INQ_Belong" = INQ_7+INQ_8+INQ_10+INQ_13+INQ_14,
    "SSI_5Item" = BSS_1+BSS_2+BSS_3+BSS_4+BSS_5,
    "SSI_Cat" = if_else(SSI_5Item<3,0,1),
    "BDITotal" = BDI_1+BDI_2+BDI_3+BDI_4+BDI_5+BDI_6+BDI_7+BDI_8+BDI_9+BDI_10+BDI_11+BDI_12+BDI_13+BDI_14+
      BDI_15+BDI_16+BDI_17+BDI_18+BDI_19+BDI_20+BDI_21,
    "BHSTotal" = BHS_1+BHS_2+BHS_3+BHS_4+BHS_5+BHS_6+BHS_7+BHS_8+BHS_9+BHS_10+BHS_11+BHS_12+BHS_13+BHS_14+
      BHS_15+BHS_16+BHS_17+BHS_18+BHS_19+BHS_20,
    "ACSSTotal" = ACSS_1+ACSS_2+ACSS_3+ACSS_4+ACSS_5+ACSS_6+ACSS_7+ACSS_8+ACSS_9+ACSS_10+ACSS_11+ACSS_12+
      ACSS_13+ACSS_14+ACSS_15+ACSS_16+ACSS_17+ACSS_18+ACSS_19+ACSS_20,
    "ASITotal" = ASI_1+ASI_2+ASI_3+ASI_4+ASI_5,
    "PCLTotal" = PCL_1+PCL_2+PCL_3+PCL_4+PCL_5+PCL_6+PCL_7+PCL_8+PCL_9+PCL_10+PCL_11+
      PCL_12+PCL_13+PCL_14+PCL_15+PCL_16+PCL_17,
    "PTSD_BItem" = if_else(PCL_1>2 | PCL_2>2 | PCL_3>2 | PCL_4>2| PCL_5>2,1,0),
    "PTSD_CItem" = if_else(if_else(PCL_6<3,0,1)+if_else(PCL_7<3,0,1)+if_else(PCL_8<3,0,1)+if_else(PCL_9<3,0,1)+
                             if_else(PCL_10<3,0,1)+if_else(PCL_11<3,0,1)+if_else(PCL_12<3,0,1)>2,1,0),
    "PTSD_DItem" = if_else(if_else(PCL_13<3,0,1)+if_else(PCL_14<3,0,1)+if_else(PCL_15<3,0,1)+if_else(PCL_16<3,0,1)+if_else(PCL_17<3,0,1)>1,1,0),
    "PTSD_Cat" = if_else(PTSD_BItem==1 & PTSD_CItem==1 & PTSD_DItem==1,1,0),
    "AUDITTotal" = AUDIT_1+AUDIT_2+AUDIT_3,
    "ISIsum" = (ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere)/5,
    "MilitaryStatus" = case_when(Veteran=="Civilian" | Deployment=="Civilian" ~ "Civilian",
                                 Veteran=="No" & Deployment== "No" ~ "Active Duty Not Deployed",
                                 Veteran=="No" & Deployment== "Yes" ~ "Active Duty Deployed",
                                 Veteran=="Yes" ~ "Veteran") %>%
      factor(levels = c("Civilian","Active Duty Not Deployed","Active Duty Deployed","Veteran")),
    "RecentSI" = case_when(SSI_Cat==1 | DSISS_Cat==1 ~ 1,
                           SSI_Cat==0 & DSISS_Cat==0 ~ 0,
                           SSI_Cat==0 & is.na(DSISS_Cat)==T ~ 0,
                           is.na(SSI_Cat)==T & DSISS_Cat==0 ~ 0,
                           is.na(SSI_Cat)==T & is.na(DSISS_Cat)==T ~ NA_real_),
    "Age" = factor(case_when(Age<=22 ~ "<23",
                             Age>22 & Age<=28 ~ "23-28",
                             Age>28 & Age<=40 ~ "28-40",
                             Age>40 ~ ">40"),
                   levels = c("<23","23-28","28-40",">40")),
    "Race" = factor(case_when(Race=="White" ~ "White",
                              Race=="Black" ~ "Black",
                              Race!="White" & Race!="Black" ~ "Other"),
                    levels = c("White","Black","Other")),
    "Education" = factor(if_else(Education=="Less than HS","HS",as.character(Education)),
                         levels = c("HS","Some college","College","More than college")),
    "ISIsum2" = (ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere),
    "ISI_Mil" = ISIsum2 * (as.numeric(MilitaryStatus)-1)
    
)

### Impute missing data (SI outcomes not used as predictors in imputation)
msrc.imp.setup <- mice(msrc,maxit=0)
msrc.imp.predmat <- quickpred(msrc, mincor = .2, minpuc = .2,
                              exclude = c("nSuicAttempts","NSSI","DSISS_Thoughts",
                                          "DSISS_Plan","DSISS_Impulse","DSISS_Control",
                                          "SBQ_1","SBQ_2","SBQ_3","SBQ_4",
                                          "BSS_1","BSS_2","BSS_3","BSS_4","BSS_5","BSS_6",
                                          "BSS_7",'BSS_8','BSS_9','BSS_10','BSS_11','BSS_12',
                                          'BSS_13','BSS_14','BSS_15','BSS_16','BSS_17',
                                          'BSS_18','BSS_19','BSS_20','BSS_21'))
msrc.imp.predmat[,c(14:27,48:68,165:168,170:171,184)] <- 0 # Suicide variables can't be predictors
msrc.imp.predmat[c(14:27,48:68,165:168,170:171,184),] <- 0 # Suicide variables won't be imputed
msrc.imp.predmeth <- msrc.imp.setup$method
msrc.imp.predmeth[c(14:27,48:68,165:168,170:171,184)] <- ""
msrc.imp.predmeth[40] <- "pmm"
msrc.imp.predmeth[169] <- "~I(INQ_7+INQ_8+INQ_10+INQ_13+INQ_14)"
msrc.imp.predmeth[172] <- "~I(BDI_1+BDI_2+BDI_3+BDI_4+BDI_5+BDI_6+BDI_7+BDI_8+BDI_9+BDI_10+BDI_11+BDI_12+
                           BDI_13+BDI_14+BDI_15+BDI_16+BDI_17+BDI_18+BDI_19+BDI_20+BDI_21)"
msrc.imp.predmeth[173] <- "~I(BHS_1+BHS_2+BHS_3+BHS_4+BHS_5+BHS_6+BHS_7+BHS_8+BHS_9+BHS_10+BHS_11+BHS_12+
                           BHS_13+BHS_14+BHS_15+BHS_16+BHS_17+BHS_18+BHS_19+BHS_20)"
msrc.imp.predmeth[174] <- "~I(ACSS_1+ACSS_2+ACSS_3+ACSS_4+ACSS_5+ACSS_6+ACSS_7+ACSS_8+ACSS_9+ACSS_10+ACSS_11+ACSS_12+
                           ACSS_13+ACSS_14+ACSS_15+ACSS_16+ACSS_17+ACSS_18+ACSS_19+ACSS_20)"
msrc.imp.predmeth[175] <- "~I(ASI_1+ASI_2+ASI_3+ASI_4+ASI_5)"
msrc.imp.predmeth[176] <- "~I(PCL_1+PCL_2+PCL_3+PCL_4+PCL_5+PCL_6+PCL_7+PCL_8+PCL_9+PCL_10+PCL_11+
                           PCL_12+PCL_13+PCL_14+PCL_15+PCL_16+PCL_17)"
msrc.imp.predmeth[c(177:180)] <- "logreg"
msrc.imp.predmeth[181] <- "~I(AUDIT_1+AUDIT_2+AUDIT_3)"
msrc.imp.predmeth[182] <- "~I((ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere)/5)"
msrc.imp.predmeth[183] <- "polyreg"
msrc.imp.predmeth[185] <- "~I(ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere)"
msrc.imp.predmeth[186] <- "~I(ISIsum * (as.numeric(MilitaryStatus)-1))"
msrc.mids <- parlmice(msrc,
                      predictorMatrix = msrc.imp.predmat,
                      method = msrc.imp.predmeth,
                      cluster.seed=123,
                      n.core=6,m=30, n.imp.core = 5)

######  BEGIN ANALYSES   #####
##### Table 1 ####
tbl1 <- tbl_summary(msrc,
                    include=c(Age,Sex,Race,Ethnicity,Education,INQ_Belong,
                              ISIsum,ISIFalling,ISIStaying,ISIEMA,MilitaryStatus, RecentSI),
                    by=MilitaryStatus,
                    type = list(ISIFalling = "continuous",ISIStaying="continuous",ISIEMA="continuous"),
                    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                                     all_categorical() ~ "{n} ({p}%)"),
                    digits = list(all_continuous() ~ c(1,2),
                                  all_categorical() ~ c(0,1)),
                    missing = "no") %>% 
  add_p(test = list(all_continuous() ~ "aov",
                     all_categorical() ~ "chisq.test"))
# Write Table 1

#### Part 1: Whole Sample Analysis ####
## Question 1: Does ISI severity and subtype predict Recent SI
model <- c("RecentSI ~ ISIsum",
           "RecentSI ~ ISIsum+Age+Sex+Race+Ethnicity+Education+MilitaryStatus")
modelname <- c("Unadjusted",'Adjusted')
simodeltable.p1 <- tibble()
for(jdx in 1:length(model)){
  simodeltable.p1 <- bind_rows(
    simodeltable.p1,
    bind_cols(
      "Model" = modelname[jdx],
      with(msrc.mids, 
           coeftest(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                    sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")))
           ) %>% pool() %>% tidy() %>% as_tibble() %>% filter(str_detect(term, "ISI")==T),
      with(msrc.mids, 
           coefci(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                  vcov. = sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")),
                  parm=2))$analyses %>% 
        unlist() %>% matrix(nrow=2) %>% t() %>% as_tibble() %>% 
        summarize(Lower = mean(V1), Upper = mean(V2))
      ) %>%
      transmute(Model, term, 
                PRR = round(exp(estimate),2),
                Lower = round(exp(Lower),2),
                Upper = round(exp(Upper),2),
                "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                p = round(p.value,4),
                FMI = fmi,
                Lambda = lambda)
  )
}
model <- c("RecentSI ~ if_else(ISIFalling>2,1,0)+if_else(ISIStaying>2,1,0)+if_else(ISIEMA>2,1,0)",
           "RecentSI ~ if_else(ISIFalling>2,1,0)+if_else(ISIStaying>2,1,0)+if_else(ISIEMA>2,1,0)+
           Age+Sex+Race+Ethnicity+Education+MilitaryStatus")
for(jdx in 1:length(model)){
  simodeltable.p1 <- bind_rows(
    simodeltable.p1,
    bind_cols(
      "Model" = modelname[jdx],
      with(msrc.mids, 
           coeftest(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                    sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")))
           ) %>% pool() %>% tidy() %>% as_tibble() %>% filter(str_detect(term, "ISI")==T),
      with(msrc.mids, 
           coefci(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                  vcov. = sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")),
                  parm=2:4))$analyses %>% 
        unlist() %>% matrix(nrow=6) %>% t() %>% as_tibble() %>% 
        summarize(across(1:6,~mean(.x))) %>% as_vector() %>% matrix(nrow=3) %>%
        as_tibble() %>% transmute("Lower" = V1, "Upper" = V2)
      ) %>%
      transmute(Model, term, 
                PRR = round(exp(estimate),2),
                Lower = round(exp(Lower),2),
                Upper = round(exp(Upper),2),
                "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
                p = round(p.value,4),
                FMI = fmi,
                Lambda = lambda)
  )
}      
simodeltable.p1 <- simodeltable.p1 %>%
  mutate("Predictors" = factor(term, levels = c("ISIsum","if_else(ISIFalling > 2, 1, 0)","if_else(ISIStaying > 2, 1, 0)","if_else(ISIEMA > 2, 1, 0)"), 
                               labels = c("Insomnia Severity", "Difficulty Falling Asleep",
                                          "Difficulty Staying Asleep","Early Morning Awakening")),
         "Model" = factor(Model, levels = c("Unadjusted","Adjusted")))
# write_csv(simodeltable.p1, "C:/users/atala/sync/research/projects/to publish/MSRC Project/data/supervised stbs paper/table1.csv")

fig1 <- ggscatter(simodeltable.p1, x="Model",y="PRR",position=position_dodge(width=0.5)) %>%
  facet(facet.by="Predictors",nrow = 1,ncol=4,strip.position = "bottom")+
  geom_errorbar(aes(ymin=Lower,ymax=Upper),width=0.25,position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 1)+
  theme(strip.placement = "outside",strip.background = element_blank(),
        strip.text = element_text(size=12),legend.title = element_blank())+
  xlab("")+ylab("Prevalence Risk of Current Suicidal Ideation")
# tiff("c:/users/atala/sync/research/projects/to publish/MSRC Project/data/supervised stbs paper/fig1.tiff",width = 2600,height = 1500,res = 300)
# ggpar(fig1, font.tickslab = c(12,"plain","black"),
#       font.x = c(12,"plain","black"),
#       font.y = c(12,"plain","black"))
# dev.off()

## Question 2: Does TB mediate ISI on Recent SI for the whole sample?
  # following code borrowed/copied from Jessie-Raye Bauer, 
  # https://jessierayebauer.wixsite.com/jrbauer/single-post/2017/09/16/multipleimputationmediation
set.seed(49581)
msrc.datlist <- datlist_create(msrc.mids)
# analysis based on all imputed datasets
si.med.total <- list()
si.med.mods <- lapply(
  msrc.datlist, 
  FUN=function(data){
  res <- sem('# Models
             RecentSI ~ b1*INQ_Belong + c*ISIsum2
             INQ_Belong ~ a1*ISIsum2
             # Effects
             indirect_belong := a1 * b1
             total := c + (a1*b1)
             direct := c', 
             data = data,
             estimator="DWLS",
             se="bootstrap",
             bootstrap=2000,
             parallel="snow",
             ncpus=5)
  return(res)
  } 
)
# extract all parameters
qhat <- lapply(si.med.mods , FUN = function(ll){
  h1 <- lavaan::parameterEstimates(ll,standardized = T,boot.ci.type="bca.simple")
  parnames <- paste0( h1$label )
  v1 <- h1$std.all
  names(v1) <- parnames
  return(v1)
} )
se <- lapply( si.med.mods , FUN = function(ll){
  h1 <- lavaan::parameterEstimates(ll,standardized = T,boot.ci.type="bca.simple")
  parnames <- paste0( h1$label )
  v1 <- h1$se
  names(v1) <- parnames
  return(v1)
} )
# use mitml for mediation
si.med.total <- mitml::testEstimates(qhat=qhat, uhat=se)

#### Part 2: Moderation by Military Status ####
## Question 1: Does ISI severity/subtype predict SI differently by military status?
model <- c("RecentSI ~ ISIsum*MilitaryStatus",
           "RecentSI ~ ISIsum*MilitaryStatus+Age+Sex+Race+Ethnicity+Education")
modelname <- c("Unadjusted",'Adjusted')
parms = list(2:8,c(2:5,16:18))
simodeltable.p2 <- tibble()
for(jdx in 1:length(model)){
  simodeltable.p2 <- bind_rows(
    simodeltable.p2,
    bind_cols(
      "Model" = modelname[jdx],
      with(msrc.mids, 
           coeftest(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                    sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")))
      ) %>% pool() %>% tidy() %>% as_tibble() %>% filter(str_detect(term, "ISI")==T|str_detect(term, "Mil")==T),
      with(msrc.mids, 
           coefci(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                  vcov. = sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")),
                  parm=parms[[jdx]]))$analyses %>% 
        unlist() %>% matrix(nrow=14) %>% t() %>% as_tibble() %>% 
        summarize(across(1:14,~mean(.x))) %>% as_vector() %>% matrix(nrow=7) %>%
        as_tibble() %>% transmute("Lower" = V1, "Upper" = V2)
    ) %>%
      transmute(Model, term, 
                PRR = estimate,
                Lower = Lower,
                Upper = Upper,
                p = round(p.value,4),
                FMI = fmi,
                Lambda = lambda)
  )
}
model <- c("RecentSI ~ (if_else(ISIFalling>2,1,0)+if_else(ISIStaying>2,1,0)+if_else(ISIEMA>2,1,0))*MilitaryStatus",
           "RecentSI ~ (if_else(ISIFalling>2,1,0)+if_else(ISIStaying>2,1,0)+if_else(ISIEMA>2,1,0))*MilitaryStatus+Age+Sex+Race+Ethnicity+Education")
parms <- list(2:16,c(2:7,18:26))
for(jdx in 1:length(model)){
  simodeltable.p2 <- bind_rows(
    simodeltable.p2,
    bind_cols(
      "Model" = modelname[jdx],
      with(msrc.mids, 
           coeftest(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                    sandwich(glm(as.formula(paste(model[jdx],sep="")),family="poisson")))
      ) %>% pool() %>% tidy() %>% as_tibble() %>% filter(str_detect(term, "ISI") | str_detect(term,"Military")),
      with(msrc.mids, 
           coefci(glm(as.formula(paste(model[jdx],sep="")),family="poisson"),
                  vcov. = sandwich(glm(as.formula(paste(model[jdx],sep="")),
                                       family="poisson")),
                  parm = parms[[jdx]]))$analyses %>% 
        unlist() %>% matrix(nrow=30) %>% t() %>% as_tibble() %>% 
        summarize(across(1:30,~mean(.x))) %>% as_vector() %>% matrix(nrow=15) %>%
        as_tibble() %>% transmute("Lower" = V1, "Upper" = V2)
    ) %>%
      transmute(Model, term, 
                PRR = estimate,
                Lower = Lower,
                Upper = Upper,
                p = round(p.value,4),
                FMI = fmi,
                Lambda = lambda)
  )
}
simodeltable.p2.t <- 
  mutate(simodeltable.p2,
         PRR = round(exp(PRR),2),
         Lower = round(exp(Lower),2),
         Upper = round(exp(Upper),2),
         "95% CI" = paste("[",Lower,", ",Upper,"]",sep = "")
         )
simodeltable.p2.p <- simodeltable.p2 %>%
  filter(str_detect(term, "ISI")) %>% 
  dplyr::select(Model,term,PRR,Lower,Upper) %>%
  separate(term, into = c("Insomnia","Military"),sep=":") %>%
  mutate(Military = replace_na(Military,"Civilian")) %>%
  mutate(Military = str_remove(Military,"MilitaryStatus")) %>%
  pivot_wider(id_cols = c(Model,Insomnia),names_from = Military,values_from = c(PRR,Lower,Upper)) %>%
  mutate(`PRR_Civilian` = PRR_Civilian,
         `PRR_Active Duty Not Deployed` = PRR_Civilian + `PRR_Active Duty Not Deployed`,
         `PRR_Active Duty Deployed` = PRR_Civilian + `PRR_Active Duty Deployed`,
         `PRR_Veteran` = PRR_Civilian + `PRR_Veteran`,
         `Lower_Civilian` = Lower_Civilian,
         `Lower_Active Duty Not Deployed` = Lower_Civilian + `Lower_Active Duty Not Deployed`,
         `Lower_Active Duty Deployed` = Lower_Civilian + `Lower_Active Duty Deployed`,
         `Lower_Veteran` = Lower_Civilian + `Lower_Veteran`,
         `Upper_Civilian` = Upper_Civilian,
         `Upper_Active Duty Not Deployed` = Upper_Civilian + `Upper_Active Duty Not Deployed`,
         `Upper_Active Duty Deployed` = Upper_Civilian + `Upper_Active Duty Deployed`,
         `Upper_Veteran` = Upper_Civilian + `Upper_Veteran`,
         ) %>%
  pivot_longer(cols = 3:14, names_to = c("Metric","Military"),names_sep = "_",values_to = "Value") %>%
  pivot_wider(id_cols = c(Model,Insomnia,Military),names_from = Metric,values_from = Value) %>%
  mutate(Model = factor(Model, levels = c("Unadjusted","Adjusted")),
         Insomnia = factor(Insomnia, 
                           levels = c("ISIsum","if_else(ISIFalling > 2, 1, 0)","if_else(ISIStaying > 2, 1, 0)","if_else(ISIEMA > 2, 1, 0)"),
                           labels = c("Insomnia Severity","Difficulty Falling Asleep","Difficulty Staying Asleep","Early Morning Awakening")),
         Military = factor(Military, levels = c("Civilian","Active Duty Not Deployed","Active Duty Deployed","Veteran"),
                           labels = c("Civilian","Never Deployed Active Service","Previously Deployed Active Service","Veteran")),
         PRR = round(exp(PRR),2),
         Lower = round(exp(Lower),2),
         Upper = round(exp(Upper),2))

# write_csv(simodeltable.p2.t, "C:/users/atala/sync/research/projects/to publish/MSRC Project/data/supervised stbs paper/table3.csv")

# Graph it
fig2 <- ggscatter(simodeltable.p2.p, x="Model",y="PRR",color="Military",position=position_dodge(width=0.5),palette="Dark2") %>%
  facet(facet.by="Insomnia",nrow = 1,ncol=4,strip.position = "bottom")+
  geom_errorbar(aes(ymin=Lower,ymax=Upper,color=Military),width=0.25,position=position_dodge(width=0.5))+
  geom_hline(yintercept = 1)+
  theme(strip.placement = "outside",strip.background = element_blank(),
       strip.text = element_text(size=12),legend.title = element_blank())+
  xlab("")+ylab("Prevalence Risk of Current Suicidal Ideation")
  
tiff("c:/users/atala/sync/research/projects/to publish/MSRC Project/data/supervised stbs paper/fig2.tiff",width = 2600,height = 1500,res = 300)
ggpar(fig2, font.tickslab = c(12,"plain","black"),
      font.x = c(12,"plain","black"),
      font.y = c(12,"plain","black"),
      font.legend = c(12,"plain","black"),
      legend.title = "")
dev.off()

## Question 2: Does TB moderate ISI on SI differently by military status?
si.isi.model.mil <- '# Models
    RecentSI ~ b*INQ_Belong + c*ISIsum2 + cw*ISI_Mil
    INQ_Belong ~ a1*ISIsum2 + aw*ISI_Mil 
    # Effects
    ab0 := a1*b
    c0 := c
    Totalciv := ab0 + c
    ab1 := a1*b  + 1*aw*b
    c1 := c + 1*cw
    Totaladnd := ab1 + c + 1*cw
    ab2 := a1*b  + 2*aw*b
    c2 := c + 2*cw
    Totaladd := ab2 + c + 2*cw
    ab3 := a1*b  + 3*aw*b
    c3 := c + 3*cw
    Totalvet := ab3 + c + 3*cw
    '
si.med.mil <- list()
set.seed(55555)
si.med.mods.mil <- lapply(msrc.datlist, 
                      FUN=function(data){
                        res <- sem(si.isi.model.mil, 
                                   data = data,
                                   estimator="DWLS",
                                   se="bootstrap",
                                   bootstrap=2000,
                                   parallel="snow",
                                   ncpus=5)
                        return(res)
                      } 
)
# extract all parameters
qhat <- lapply(si.med.mods.mil , FUN = function(ll){
  h1 <- lavaan::parameterEstimates(ll,standardized = T,boot.ci.type="bca.simple")
  parnames <- paste0( h1$label )
  v1 <- h1$std.all
  names(v1) <- parnames
  return(v1)
} )
se <- lapply( si.med.mods.mil , FUN = function(ll){
  h1 <- lavaan::parameterEstimates(ll,standardized = T,boot.ci.type="bca.simple")
  parnames <- paste0( h1$label )
  v1 <- h1$se
  names(v1) <- parnames
  return(v1)
} )
# use mitml for mediation
si.med.mil <- mitml::testEstimates(qhat=qhat, uhat=se)