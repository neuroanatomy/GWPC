library(plyr)
library(gtools)
library(prettyR)

script.dir <- {
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  sourceDir <- getSrcDirectory(function(dummy) {dummy})
  if (length(script.name)) { # called from command
    (dirname(script.name))
  } else if (nchar(sourceDir)) { # called with source
    sourceDir
  } else if (rstudioapi::isAvailable()) { # called from RStudio
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else getwd()
}

base.dir <- system(paste("cd", script.dir, "&& git rev-parse --show-toplevel"), intern=T)
data.dir <- file.path(base.dir, "data")
raw.dir <- file.path(data.dir, "raw")
derived.dir <- file.path(data.dir, "derived")

# description de l'echantillon abide_final
abide_final<-read.table(file.path(derived.dir, "abide_final.txt"), sep = '\t', check.names = FALSE, header = TRUE)

# on veut comme r??f??rence pour le diagnostic Control et pour le SEX Male :
abide_final$DX_GROUP<- relevel(abide_final$DX_GROUP, ref="Control")
abide_final$SEX<- relevel(abide_final$SEX, ref="Male")

# groupe ASD
ASD <- subset(abide_final, DX_GROUP=="ASD")
#neuroTypicall control=TC
TC<-subset(abide_final, DX_GROUP=="Control")
# sex
male<- subset(abide_final, SEX=="Male")
female<-subset(abide_final, SEX=="Female")


#moyenne d'age des ASD : 17.04 ans (min : 5,22 ?? max : 62,00 ans), sd = 9,23, median : 14,20
describe(ASD$AGE_AT_SCAN, xname="ASD$AGE_AT_SCAN")
summary(ASD[,"AGE_AT_SCAN"])

#moyenne d'age des TC : 16,292 ans (min : 5,887 ?? max : 64,000 ans), sd = 8.72, mediane : 13,18 ans
describe(TC$AGE_AT_SCAN, xname="TC$AGE_AT_SCAN")
summary(TC[,"AGE_AT_SCAN"])

#comparaison des ages entre TSA et TC avec t test : condition de validit?? ; n>30 ok et loi normale (pas tr??s loin, ok), la variance de la variable dans les deux groupes doit etre egale (ici : sd : 9,39 et 8.81 : ok)
hist(TC$AGE_AT_SCAN, breaks = seq(5, 65, 5))
hist(ASD$AGE_AT_SCAN, breaks = seq(5, 65, 5))
t.test(abide_final$AGE_AT_SCAN~ abide_final$DX_GROUP, var.equal=F)
# p-value = 0.1126, (95 percent confidence interval:  -0.175917  1.668859), donc pas de diff??rence significative d'age entre les groupes ASD et TC

#moyenne des FIQ chez les TSA : 105,8 (41,0 ?? 149,0), sd=16,48, mediane : 106,0
describe(ASD$FIQ_total, xname="ASD$FIQ_total")
summary(ASD$FIQ_total)
# combien de retard mental ? (soit FIQ< 70) : 7
table(ASD$FIQ_total<70, useNA="always")

# moyenne des FIQ chez les TC : 113,7 (de 71,0 ?? 149,0), sd= 16,48, mediane : 106,0 (donc aucun retard mental)
# describe(TC[,"FIQ_total"])
summary(TC$FIQ_total)

# comparaison des FIQ entre les gropes ASD et TC avec t test : n>30+ normalit?? de la distribution ok, + ecart types egaux (sd environ =16 pour les deux : ok)
hist(TC$FIQ_total)
hist(ASD$FIQ_total)
t.test(abide_final$FIQ_total ~ abide_final$DX_GROUP, var.equal=F)
# il y a une diff??rence significative entre les FIQ des ASD et des TC (ASD significativement plus bas QI)(t = -10.594, p-value < 2.2e-16), mais on le prend en compte dans le mod??le lin??aire en corrigeant sur FIQ

#number of female and male in ASD and TC group
dx_sex_table <- table(abide_final[, c("DX_GROUP", "SEX")])
dx_sex_table
# ASDfem : 86 observations 
# ASDmale : 551 oservations 
# TCfem :202 observations 
# TCmale : 638 observations (+ 12 grace au calcul FIQ2)

# Proportion of females by group:
dx_sex_table[, 2] / rowSums(dx_sex_table)
chisq.test(dx_sex_table)

# chez les ASD moyenne, sd des scores ADI et ADOS :
# ADI-R social : moyenne : 19,37 (min : 4,00, max : 30,00), sd: 5,59, mediane : 20,00, 209 NA
describe(ASD$ADI_R_SOCIAL_TOTAL_A, xname="ASD$ADI_R_SOCIAL_TOTAL_A")
summary(ASD$ADI_R_SOCIAL_TOTAL_A)

# ADI-R communication, 208 NA
describe(ASD$ADI_R_VERBAL_TOTAL_BV, xname="ASD$ADI_R_VERBAL_TOTAL_BV")
summary(ASD$ADI_R_VERBAL_TOTAL_BV)
# mean : 15,52 + ou - sd : 4,56 (min : 4,00, max : 26,00)

# ADI-R repetitive behavior, 208 NA 
describe(ASD$ADI_RRB_TOTAL_C, xname="ASD$ADI_RRB_TOTAL_C")
summary(ASD$ADI_RRB_TOTAL_C)
# mean : 5,89 + ou - sd : 2,55 (min : 0,000, max : 13,000)

# ADOS social + communication, 196 NA 
describe(ASD$ADOS_TOTAL, xname="ASD$ADOS_TOTAL")
summary(ASD$ADOS_TOTAL)
# mean : 11,5 + ou - sd : 3,85 (min : 2,0, max : 23,0)


# nombre de 3 tesla versus 1,5 Tesla : 6 IRM 1,5 Tesla(ABIDEII-IP_1=seul site avec 1,5Tesla = Robert Debr?? / Institut Pasteur), le reste = 3 Tesla 
summary(abide_final$SITE_ID)

# DSM 4 TR :
table (abide_final$DSM_IV_TR, useNA = "always")
# il y a donc dans abide_final, concernant le diagnostic : 651 contr??les, 319 autistes, 96 asperger, 48 PDD-NOS, 4 Aspergers or PDD-NOS (Pervasive Developmental Disorder-Not Otherwise Specified), et 359 NA

# main dominante = HANDEDNESS_CATEGORY
table(abide_final$HANDEDNESS_CATEGORY, useNA = "always")
# 1073 droitiers, 71 gauchers, 50 ambidextres (Ambi + Mixed), +1 "L->R", 280 NA (somme = 1475)

# handedness difference between groups
dx_hand_table <- table(abide_final[, c("DX_GROUP", "HANDEDNESS_CATEGORY")])
dx_hand_table
chisq.test(dx_hand_table)

# any other comorbidity ?
length(grep("ADHD", abide_final$COMORBIDITY, value =TRUE)) # 16 dans abide 1(tous ASD)
length(grep("ADHD", abide_final$NONASD_PSYDX_LABEL, value =TRUE)) # 66 dans abide 2  (tous ASD)
which(abide_final$NONASD_PSYDX_LABEL=="ADHD")
table(abide_final$CASI_ADHD.I_CUTOFF, useNA = "always") # +12 dans abide 2 (diff??rents des 66 ci dessus) (dont 2 controles)
which(abide_final$CASI_ADHD.I_CUTOFF==1)
table(abide_final$CSI_ADHD.I_CUTOFF, useNA = "always")
which(abide_final$CSI_ADHD.I_CUTOFF==1) # ne change pas : d??j?? dans CASI
table(abide_final$CASI_ADHD.H_CUTOFF, useNA = "always") # +2 (4 en tout mais 2 d??j?? dans CASI_ADHD.I) (tous ASD)
which(abide_final$CASI_ADHD.H_CUTOFF==1)
table(abide_final$CSI_ADHD.H_CUTOFF, useNA = "always")
which(abide_final$CSI_ADHD.H_CUTOFF==1) # aucun
table(abide_final$CASI_ADHD.C_CUTOFF, useNA = "always")
which(abide_final$CASI_ADHD.C_CUTOFF==1) # +0 (3 d??j?? compt??s)
table(abide_final$CSI_ADHD.C_CUTOFF, useNA = "always")
which(abide_final$CSI_ADHD.C_CUTOFF==1) # aucun
# 16+66+12(ADHD.I)+2 suppl(ADHD.H)+0 suppl(ADHD.C) = 96 avec ADHD (dont 2 contr??les)
length(grep("hobia", abide_final$COMORBIDITY, value =TRUE)) # 10 phobies (7 ASD, 3 controles)
table(abide_final$CASI_SPECIFIC_PHOBIA_CUTOFF, useNA = "always")
which(abide_final$CASI_SPECIFIC_PHOBIA_CUTOFF==1) # + 22 specific phobia (13 ASD, 9 controles)
table(abide_final$CSI_SPECIFIC_PHOBIA_CUTOFF, useNA = "always")
which(abide_final$CSI_SPECIFIC_PHOBIA_CUTOFF==1) # il y en a 2 (1 ASD et 1 control) dont un d??j?? compt?? dans CASI, donc +1
table(abide_final$CASI_SOCIAL_PHOBIA_CUTOFF, useNA = "always") #7+0=7 social phobia (tous ASD)
which(abide_final$CASI_SOCIAL_PHOBIA_CUTOFF==1)
table(abide_final$CSI_SOCIAL_PHOBIA_CUTOFF, useNA = "always")
which(abide_final$CSI_SOCIAL_PHOBIA_CUTOFF==1) # 1 mais d??j?? compt?? dans CASI donc 0 de +
#10+(22+1) (SPECIFIC_PHOBIA) + 7 (social phobia)= 40 avec phobie sociale ou specifique (dont 12 controles)
length(grep("ODD", abide_final$COMORBIDITY, value =TRUE)) #4
table(abide_final$CASI_ODD_CUTOFF, useNA = "always") #8
table(abide_final$CSI_ODD_CUTOFF, useNA = "always") # aucun
# 4+8(ODD_CUTOFF)= 12 avec ODD ( Oppositional Defiant Disorder), dont 1 controle
length(grep("nxiety", abide_final$COMORBIDITY, value =TRUE)) # 8
table(abide_final$CASI_SEPARATION_CUTOFF, useNA = "always") # 0 anxi??t?? de separation
table(abide_final$CASI_SEPARATION_CUTOFF, useNA = "always") # 0 anxi??t?? de separation
length(grep("GAD", abide_final$COMORBIDITY, value =TRUE)) # 1
table(abide_final$CASI_GAD_CUTOFF, useNA = "always") # 5
which(abide_final$CASI_GAD_CUTOFF==1)
table(abide_final$CSI_GAD_CUTOFF, useNA = "always")
which(abide_final$CSI_GAD_CUTOFF==1) # 1 mais d??j?? inclus dans CASI, donc +0
length(grep("GAD", abide_final$NONASD_PSYDX_LABEL, value =TRUE)) #20 (dont 3 controles)
which(abide_final$NONASD_PSYDX_LABEL=="GAD")
table(abide_final$CASI_PANIC_ATTACKS_CUTOFF, useNA = "always") #0 attaques de panique
#8+1+5+20 = 34 with anxiety disorder (+GAD : Generalized Anxiety Disorder) : dont 3 GAD chez les controles
length(grep("ysthymia", abide_final$COMORBIDITY, value =TRUE)) #4
length(grep("ysthymic", abide_final$COMORBIDITY, value =TRUE)) #1
length(grep("ood", abide_final$COMORBIDITY, value =TRUE)) #5
table(abide_final$CASI_MDE_CUTOFF, useNA = "always") # 0  Major Depressive Episode
table(abide_final$CSI_MDD_CUTOFF, useNA = "always") # 0  Major Depressive Disorder
table(abide_final$CASI_DYSTHYMIC_CUTOFF, useNA = "always") # 2 dysthymic disorder
table(abide_final$CSI_DYSTHYMIC_CUTOFF, useNA = "always") # 0
#4+1+5+2 = 12 with mood disorder/ dysthymic disorder /dysthymia  (tous ASD)
length(grep("Tic", abide_final$COMORBIDITY, value =TRUE)) #2
table(abide_final$CASI_MOTOR_TICS_CUTOFF, useNA = "always") # 13 + 1 = 14 Motor tics
which(abide_final$CASI_MOTOR_TICS_CUTOFF==1)
table(abide_final$CSI_MOTOR_TICS_CUTOFF, useNA = "always") # +1
which(abide_final$CSI_MOTOR_TICS_CUTOFF==1)
table(abide_final$CASI_VOCAL_TICS_CUTOFF, useNA = "always") # 15 +1 = 16 vocal tics
which(abide_final$CASI_VOCAL_TICS_CUTOFF==1)
table(abide_final$CSI_VOCAL_TICS_CUTOFF, useNA = "always")
which(abide_final$CSI_VOCAL_TICS_CUTOFF==1) # + 1
#2+14+16 = 32 with Tic
length(grep("Encopresis", abide_final$COMORBIDITY, value =TRUE)) # 2
table(abide_final$CASI_ENURESIS_ENCOPRESIS_CUTOFF, useNA = "always") # 1
table(abide_final$CSI_ENCOPRESIS_CUTOFF, useNA = "always") # aucun
length(grep("Enuresis", abide_final$COMORBIDITY, value =TRUE)) # 3
table(abide_final$CASI_NOCTURNAL_ENURESIS_CUTOFF, useNA = "always") # 5 (dont 1 controle)
which(abide_final$CASI_NOCTURNAL_ENURESIS_CUTOFF==1)
table(abide_final$CSI_ENURESIS_CUTOFF, useNA = "always") 
which(abide_final$CSI_ENURESIS_CUTOFF==1) # 1 de plus (1 controle)
#2+1=3 with Encopresis, 3+5+1 = 9 with Enuresis (dont 2 controle), soit 9+3 = 12 (dont 2 controles) with Enuresis and/or Encopresis 

table(abide_final$CASI_CD_CUTOFF, useNA = "always") # 2 with  Conduct Disorder (ASD)
table(abide_final$CSI_CD_CUTOFF, useNA = "always") # aucun

table(abide_final$CASI_OBSESSIONS_CUTOFF, useNA = "always") # 19 (12 ASD, 7 controles)
which(abide_final$CASI_OBSESSIONS_CUTOFF==1)
table(abide_final$CSI_OBSESSIONS_CUTOFF, useNA = "always")
which(abide_final$CSI_OBSESSIONS_CUTOFF==1) # 3 dont un d??j?? compt?? dans CASI donc +2 (1 ASD et 1 control), donc 19(12 ASD, 7 controles) + 2 (1 ASD et 1 control)= 21 (dont 8 controles) with  Obsessions

table(abide_final$CASI_COMPULSIONS_CUTOFF, useNA = "always") #0 compulsions
table(abide_final$CSI_COMPULSIONS_CUTOFF, useNA = "always") # 1 avec compulsions (ASD)

table(abide_final$CASI_PTSD_CUTOFF, useNA = "always") # 8 with PTSD (7 ASD, 1 controle)

table(abide_final$CASI_SCHIZOPHRENIA_CUTOFF, useNA = "always")  # 0 szp
table(abide_final$CSI_SCHIZOPHRENIA_CUTOFF, useNA = "always") # 0 szp

table(abide_final$CASI_ASPD_CUTOFF, useNA = "always") #0 Antisocial Personality Disorder

table(abide_final$CASI_SOMATIZATION_CUTOFF, useNA = "always") # 1 (sur11) Somatization Disorder (ASD)

table(abide_final$CASI_SCHIZOID_PERSONALITY_CUTOFF, useNA = "always") # 1  (sur 11) Schizoid Personality (control)

table(abide_final$CASI_MANIC_EPISODE_CUTOFF, useNA = "always") # 1 (sur11) with Manic episode (ASD)

table(abide_final$CASI_ANOREXIA_NERVOSA_CUTOFF, useNA = "always") # 0(sur 11) with anorexia nervosa

table(abide_final$CASI_BULIMIA_NERVOSA_CUTOFF, useNA = "always") # 0 (sur 11) with boulimia 

table(abide_final$CASI_SUBSTANCE_USE_CUTOFF, useNA = "always") # 0 (sur 1) avec abus de substance

table(abide_final$CSI_DISTURBING_EVENTS_CUTOFF, useNA = "always") # 1 with disturbing events (ASD)

# (dont 12 avec plusieurs de ces comorbidit??s, sans compter les lignes faites concernant abide2 legend, NB : 66 remplis 0 ou 1 ?? chaque fois pour les comorbidit??s dans abide 2)

table(abide_final$NONASD_PSYDX_LABEL, useNA = "always")

# prise de traitements m??dical en parall??le : 
table (abide_final$CURRENT_MED_STATUS, useNA = "always")
# 1013 not taking medication, 179 taking medication, 283 NA

# medication name ? 55 NA, 565 "none", 657 = "", 22 ="0" (soit 1475-55-565-657-22=176 sur les 179 prenant des traitements dont on a le nom des traitements, soit 3 manquants seulement)
table (abide_final$MEDICATION_NAME, useNA = "always")
# lesquels en plus ? 
sub_40 <- subset(abide_final, !complete.cases(abide_final[,"FIQ"]))
sub_40$MEDICATION_NAME
table(sub_40$MEDICATION_NAME, useNA = "always")
# types de medicaments (non revu apr??s correction de la base de donn??e mais peu de changements) = ADP : 75 (74+1), NLP : 27 (26+1), Methylphenidate et autres d??riv??s amph??tamines : 78, autres traitements du TDAH non amph??taminiques (Guanfacine et Atomoxetine) : 28, Anti-??pileptiques / thymoregulateurs : 14, Anxiolytiques BZD et Z drugs : 4,Melatonine : 4,  Autres (levothyroxine, oestradiol, IPP, mineraux, vitamines, traitements de l'asthme de l'allergie, de l'HTA, du diab??te, anti-inflammatoires, statines, antibiotiques, traitement du retard de croissance par Somatropin) : 53, polymedication : 2 ttts : 52 , 3 et + ttts : 29 

# Pas de stimulants 24h avant l'IRM (1 : oui, 0: non) ?
table(abide_final$OFF_STIMULANTS_AT_SCAN, useNA = "always")
# 27 non, 58 oui, 1412+32=1444 NA


QC3_0_4_10 <- read.table(file.path(derived.dir, "QC3_0_4_10.txt"), sep = '\t', check.names = FALSE, header = TRUE)

abide_final$keep <- "yes"
QC3_0_4_10$keep <- "no"
abide_total <- rbind(abide_final, QC3_0_4_10)


# quelles sont les caract??ristiques des sujets exclus ? -> exclusion de 263 ASD (55,96% d'ASD) + 207 Control (abide_final : 839 control + 636 ASD (43,11% d'ASD)), exclusion de significativement plus d'ASD (p-value = 1.52e-06)
# 26% of subjects were excluded: 22% of controls vs 31% of ASD
summary(QC3_0_4_10$DX_GROUP)
ASD_ex <- subset(QC3_0_4_10, DX_GROUP=="ASD")
TC_ex<-subset(QC3_0_4_10, DX_GROUP=="Control")
dx_keep_table <- table(abide_total[, c("DX_GROUP", "keep")])
dx_keep_table
sum(dx_keep_table[, "no"]) / sum(dx_keep_table)
dx_keep_table[, "no"] / rowSums(dx_keep_table)
chisq.test(dx_keep_table)

# exclusion de 386 male (82,13% of male) et 84 female, versus 80,47% of male dans abide_final, pas de diff??rence significative de proportion de male dans les exclus (p-value = 0.4677)
# 26.2% of males were excluded vs 25.6% of females
male_ex<- subset(QC3_0_4_10, SEX=="Male")
female_ex<-subset(QC3_0_4_10, SEX=="Female")
sex_keep_table <- table(abide_total[, c("SEX", "keep")])
sex_keep_table
sex_keep_table[, "no"] / rowSums(sex_keep_table)
chisq.test(sex_keep_table)


summary(ASD_ex$AGE_AT_SCAN)
summary(TC_ex$AGE_AT_SCAN)
t.test(QC3_0_4_10$AGE_AT_SCAN~ QC3_0_4_10$DX_GROUP)
summary(QC3_0_4_10$AGE_AT_SCAN)
summary(abide_final$AGE_AT_SCAN)
t.test(AGE_AT_SCAN~keep, data=abide_total)
wilcox.test(AGE_AT_SCAN~keep, data=abide_total)
#age moyen des exclus : 14,206 ans; pas de diff??rence significative entre ASD (14,14 ans) et Control (14,28 ans) par rapport ?? l'age (p-value = 0.821), age moyen significativement plus jeune chez les sujets exclus (p-value = 3.379e-11) : 

summary(ASD_ex[,"FIQ_total"])
summary(TC_ex[,"FIQ_total"])
t.test(QC3_0_4_10$FIQ_total ~ QC3_0_4_10$DX_GROUP)
summary(QC3_0_4_10$FIQ_total)
summary(abide_final$FIQ_total)
t.test(FIQ_total~keep, data=abide_total)
wilcox.test(FIQ_total~keep, data=abide_total)
# QI significativement plus grand pour les Control (p-value = 0.0016), QI significativement diff??rent entre les exclus et les non exclus ?? cause du QC (p-value = 0.006231)

summary(QC3_0_4_10$SITE_ID)

# analyse par site :
# Barrow Neurological Institute
BNI_1 <- subset (abide_final, SITE_ID == "ABIDEII-BNI_1" )
BNI_ASD <- subset (BNI_1, DX_GROUP == "ASD")
length(BNI_ASD$SEX=="Male")
length(BNI_1$SEX=="Female")
#ETH Z??rich
ETH_1 <- subset (abide_final, SITE_ID == "ABIDEII-ETH_1")
table(ETH_1$DX_GROUP)
table(ETH_1$SEX)
#Georgetown University 
GU_1 <- subset (abide_final, SITE_ID == "ABIDEII-GU_1")
GU_ASD <- subset (GU_1, DX_GROUP=="ASD")
table(GU_1$DX_GROUP)
table(GU_1$SEX)
table(GU_ASD$SEX)
#Institut Pasteur and Robert Debre Hospital 
IP_1 <- subset (abide_final, SITE_ID == "ABIDEII-IP_1")
IP_ASD <- subset (IP_1 , DX_GROUP=="ASD")
table(IP_ASD$SEX)
table(IP_1$SEX)
table(IP_1$DX_GROUP)
# Indiana University
IU_1 <- subset (abide_final, SITE_ID == "ABIDEII-IU_1")
table(IU_1$SEX)
table(IU_1$DX_GROUP)
IU_ASD <- subset (IU_1, DX_GROUP=="ASD")
table(IU_ASD$SEX)
#Kennedy Krieger Institute 
KKI <- subset (abide_final, SITE_ID == "KKI" )
table(KKI$SEX)
table(KKI$DX_GROUP)
KKI_ASD <- subset (KKI, DX_GROUP=="ASD")
table(KKI_ASD$SEX)

KKI_2 <- subset (abide_final, SITE_ID == "KKI_2" )
table(KKI_2$SEX)
table(KKI_2$DX_GROUP)
KKI_2_ASD <- subset (KKI_2, DX_GROUP=="ASD")
table(KKI_2_ASD$SEX)

KKI_3 <- subset (abide_final, SITE_ID == "KKI_3" )
table(KKI_3$SEX)
table(KKI_3$DX_GROUP)
KKI_3_ASD <- subset (KKI_3, DX_GROUP=="ASD")
table(KKI_3_ASD$SEX)
#NYU Langone Medical Center
NYU_1 <- subset (abide_final, SITE_ID == "ABIDEII-NYU_1" | SITE_ID == "NYU")
table(NYU_1$SEX)
table(NYU_1$DX_GROUP)
NYU_ASD <- subset (NYU_1, DX_GROUP=="ASD")
table(NYU_ASD$SEX)
#Oregon Health and Science University
OHSU_1 <- subset (abide_final, SITE_ID == "ABIDEII-OHSU_1" | SITE_ID == "OHSU")
table(OHSU_1$SEX)
table(OHSU_1$DX_GROUP)
OHSU_ASD <- subset (OHSU_1, DX_GROUP=="ASD")
table(OHSU_ASD$SEX)
# Olin Neuropsychiatry Research Center, Institute of Living at Hartford Hospital 
OLIN <- subset (abide_final, SITE_ID == "OLIN")
table(OLIN$SEX)
table(OLIN$DX_GROUP)
OLIN_ASD <- subset (OLIN, DX_GROUP=="ASD")
table(OLIN_ASD$SEX)

ONRC_2 <- subset (abide_final, SITE_ID == "ABIDEII-ONRC_2")
table(ONRC_2$SEX)
table(ONRC_2$DX_GROUP)
ONRC_ASD <- subset (ONRC_2, DX_GROUP=="ASD")
table(ONRC_ASD$SEX)
#San Diego State University 
SDSU_1 <- subset (abide_final, SITE_ID == "ABIDEII-SDSU_1" | SITE_ID == "SDSU")
table(SDSU_1$SEX)
table(SDSU_1$DX_GROUP)
SDSU_ASD <- subset (SDSU_1, DX_GROUP=="ASD")
table(SDSU_ASD$SEX)
# Trinity Centre for Health Sciences
TCD_1 <- subset (abide_final, SITE_ID == "ABIDEII-TCD_1")
table(TCD_1$SEX)
table(TCD_1$DX_GROUP)

TRINITY <- subset (abide_final, SITE_ID == "TRINITY")
table(TRINITY$SEX)
table(TRINITY$DX_GROUP)
# University of California Davis 
UCD_1 <- subset (abide_final, SITE_ID == "ABIDEII-UCD_1")
table(UCD_1$SEX)
table(UCD_1$DX_GROUP)
UCD_ASD <- subset (UCD_1, DX_GROUP=="ASD")
table(UCD_ASD$SEX)
#University of California Los Angeles
UCLA_ab1 <- subset (abide_final, SITE_ID == "UCLA_1" | SITE_ID == "UCLA_2")
table(UCLA_ab1$SEX)
table(UCLA_ab1$DX_GROUP)
UCLA_ab1_ASD <- subset (UCLA_ab1, DX_GROUP=="ASD")
table(UCLA_ab1_ASD$SEX)

UCLA_ab2 <- subset (abide_final, SITE_ID == "ABIDEII-UCLA_1")
table(UCLA_ab2$SEX)
table(UCLA_ab2$DX_GROUP)
UCLA_ab2_ASD <- subset (UCLA_ab2, DX_GROUP=="ASD")
table(UCLA_ab2_ASD$SEX)
# University of Utah, School of medicine
USM_1 <- subset (abide_final, SITE_ID == "ABIDEII-USM_1" | SITE_ID == "USM")
table(USM_1$SEX)
table(USM_1$DX_GROUP)
USM_ASD <- subset (USM_1, DX_GROUP=="ASD")
table(USM_ASD$SEX)
#California Institute of Technology
CALTECH <- subset (abide_final, SITE_ID == "CALTECH")
table(CALTECH$SEX)
table(CALTECH$DX_GROUP)
CALTECH_ASD <- subset (CALTECH, DX_GROUP=="ASD")
table(CALTECH_ASD$SEX)
# Carnegie Mellon University
CMU <- subset (abide_final, SITE_ID == "CMU")
table(CMU$SEX)
table(CMU$DX_GROUP)
CMU_ASD <- subset (CMU, DX_GROUP=="ASD")
table(CMU_ASD$SEX)
# University of Leuven
LEUVEN <- subset (abide_final, SITE_ID == "LEUVEN_1" | SITE_ID == "LEUVEN_2")
table(LEUVEN$SEX)
table(LEUVEN$DX_GROUP)
LEUVEN_ASD <- subset (LEUVEN, DX_GROUP=="ASD")
table(LEUVEN_ASD$SEX)
#Ludwig Maximilians University Munich 
MAX_MUN <- subset (abide_final, SITE_ID == "MAX_MUN")
table(MAX_MUN$SEX)
table(MAX_MUN$DX_GROUP)
MAX_MUN_ASD <- subset (MAX_MUN, DX_GROUP=="ASD")
table(MAX_MUN_ASD$SEX)
#University of Pittsburgh School of Medicine 
PITT <- subset (abide_final, SITE_ID == "PITT")
table(PITT$SEX)
table(PITT$DX_GROUP)
PITT_ASD <- subset (PITT, DX_GROUP=="ASD")
table(PITT_ASD$SEX)
#University of Michigan: Sample 1 and 2
UM <- subset (abide_final, SITE_ID == "UM_1" | SITE_ID == "UM_2")
table(UM$SEX)
table(UM$DX_GROUP)
UM_ASD <- subset (UM, DX_GROUP=="ASD")
table(UM_ASD$SEX)
#Yale Child Study Center
YALE <- subset (abide_final, SITE_ID == "YALE")
table(YALE$SEX)
table(YALE$DX_GROUP)
YALE_ASD <- subset (YALE, DX_GROUP=="ASD")
table(YALE_ASD$SEX)
