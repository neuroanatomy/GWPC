#but : associer les 4 fichiers (phenot1 et 2 pour les donnees cliniques (phenotypiques) et irmab1 et 2 pour les donnees IRM) en un seul fichier 

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

dir.create(derived.dir, showWarnings = FALSE)

phenot1<-read.csv(file=file.path(raw.dir, "Phenotypic_V1_0b.csv"), na.strings=c("-9999", "", "NA"))
phenot2<-read.csv(file=file.path(raw.dir, "participants-abide2.tsv"), sep="\t", na.strings="n/a")
irmab1<-read.csv(file=file.path(raw.dir, "w-g.abide.csv"), sep='\t')
irmab2<-read.csv(file=file.path(raw.dir, "w-g.abide2.csv"), sep="\t")

#on ajoute une colonne "fsid"
irmab1$fsid <- irmab1$Subject
# ajouter 00 devant les SUB_ID pour generer les fsid dans Abide 1
irmab1$fsid <-sprintf("%07d", as.numeric(irmab1$fsid))

irmab2$fsid <- irmab2$Subject

# ajout des 37 donnees phenotypiques manquantes de abide 2 : 
phenot_eth <- read.csv(file=file.path(raw.dir, "ABIDEII-ETH_1.csv"))

# renommer les colonnes en commun, prealable au merge pour associer les donnees phenotypiques de abide 1 (phenot1), et celles de abide2 (phenot2)

# passage des noms de colonnes en majuscule : 
names(phenot2) <- toupper(names(phenot2))
names(phenot_eth) <- toupper(names(phenot_eth))

library(plyr)
phenot2 <- plyr::rename(phenot2, c("PARTICIPANT_ID"="SUB_ID"))

# ajout des 37 donnees manquantes a phenot22: 
phenot2 <- merge (phenot2, phenot_eth, all=TRUE)

ab2_to_ab1 <- c("ADI_R_RRB_TOTAL_C"="ADI_RRB_TOTAL_C",
                "ADOS_G_TOTAL"="ADOS_TOTAL",
                "ADOS_G_COMM"="ADOS_COMM",
                "ADOS_G_SOCIAL"="ADOS_SOCIAL",
                "ADOS_G_STEREO_BEHAV"="ADOS_STEREO_BEHAV",
                "ADOS_2_SOCAFFECT"="ADOS_GOTHAM_SOCAFFECT",
                "ADOS_2_RRB"="ADOS_GOTHAM_RRB",
                "ADOS_2_TOTAL"="ADOS_GOTHAM_TOTAL",
                "ADOS_2_SEVERITY_TOTAL"="ADOS_GOTHAM_SEVERITY",
                "SRS_TOTAL_RAW"="SRS_RAW_TOTAL",
                "SRS_AWARENESS_RAW"="SRS_AWARENESS",
                "SRS_COGNITION_RAW"="SRS_COGNITION",
                "SRS_COMMUNICATION_RAW"="SRS_COMMUNICATION",
                "SRS_MOTIVATION_RAW"="SRS_MOTIVATION",
                "SRS_MANNERISMS_RAW"="SRS_MANNERISMS",
                "CURRENT_MEDICATION_NAME"="MEDICATION_NAME",
                "VINELAND_DAILYLIVING_STANDARD"="VINELAND_DAILYLVNG_STANDARD",
                "PDD_DSM_IV_TR"="DSM_IV_TR")

phenot2 <- plyr::rename(phenot2, ab2_to_ab1)

# phenot 1 a handedness category en factor, alors que phenot 2 l'a en integer, tout mettre dans la meme unite (factor) :
table(phenot1$HANDEDNESS_CATEGORY, useNA = "always")
table(phenot2$HANDEDNESS_CATEGORY, useNA = "always")
phenot2$HANDEDNESS_CATEGORY <- factor(phenot2$HANDEDNESS_CATEGORY, labels = c("R", "L", "Ambi"))
phenot1$HANDEDNESS_CATEGORY[phenot1$HANDEDNESS_CATEGORY %in% c("L->R", "Mixed")] <- "Ambi"
phenot1$HANDEDNESS_CATEGORY <- factor(phenot1$HANDEDNESS_CATEGORY, levels=levels(phenot2$HANDEDNESS_CATEGORY))

# association en un seul fichier (phenot_total1) de toutes les donnees cliniques (phenot11+phenot222)
phenot_total <- merge(phenot1, phenot2, all=T)


# ajouter a irmab1, les QC surface + segmentation :
# qc abide 1 surface uniquement : 
qc_1_surface <-read.csv(file=file.path(raw.dir, "qc-abide1-surfaces.csv"))
# rename comment column
qc_1_surface <- plyr::rename(qc_1_surface, c("Comments_1"="Comments_Surface"))
# qc abide 1 segmentation uniquement
qc_1_segmentation <-read.csv(file=file.path(raw.dir, "qc-ab1-with-10.txt"), sep='\t')
table(qc_1_segmentation[,"QC"])
# mettre meme nom pour SUB_ID dans les deux fichiers
seg_col_names <- c("Subject"="SUB_ID", "QC"="QC_Segmentation", "Comments"="Comments_Segmentation")
qc_1_segmentation <- plyr::rename (qc_1_segmentation, seg_col_names)
# ne garder que les 3 premieres colonnes de qc_1_segmentation_bis : 
qc_1_segmentation <- qc_1_segmentation[, seg_col_names]

# association segmentation + surface pour abide 1
abide1_qc_joined <- merge(qc_1_surface, qc_1_segmentation, by="SUB_ID", all=TRUE)

# passage de irmab1 et irmab2 avec SUB_ID : 
irmab1 <-plyr::rename(irmab1, c("Subject"="SUB_ID"))
irmab2 <- plyr::rename(irmab2, c("Subject"="SUB_ID"))
# maintenant on peut associer irmab1 avec son qc (abide1_qc_joined)
irmab1 <- merge(irmab1, abide1_qc_joined, by="SUB_ID", all = TRUE)


#ajouter les qc surface + segmentation pour abide 2 a irmab22 :
# qc abide 2 segmentation
qc_2_segmentation <-read.csv(file=file.path(raw.dir, "qc-ab2-with-10.txt"), sep="\t")
# mettre meme nom pour SUB_ID dans les deux fichiers
qc_2_segmentation <- plyr::rename (qc_2_segmentation, seg_col_names)
# ne garder que les 3 premieres colonnes de qc_2_segment_bis : 
qc_2_segmentation <- qc_2_segmentation[, seg_col_names]

table(qc_2_segmentation[,"QC_Segmentation"])
# abide 2 surface (fichier mis à jour) :
qc_2_surface <- read.csv(file=file.path(raw.dir, "qc-abide2-surfaces.csv"))
# rename comment column
qc_2_surface <- plyr::rename(qc_2_surface, c("Comments_1"="Comments_Surface"))

setdiff(qc_2_surface[,1], qc_2_segmentation[,1])
setdiff(qc_2_segmentation[,1], qc_2_surface[,1])
# ok : les seules diff?rences = 5 x "0" (pas de segmentation), et 2 x "3" pour des "_ses-1" (bad)

# association segmentation + surface pour abide 2
abide2_qc_joined <- merge(qc_2_surface, qc_2_segmentation, by="SUB_ID", all=TRUE)

# quelle difference de SUB_ID entre qc_2 et irmab22? ok : les memes que precedemment
setdiff(abide2_qc_joined[,1], irmab2[,1])
setdiff(irmab2[,1], abide2_qc_joined[,1])


# joindre les qc a irmab2 :
irmab2 <- merge(irmab2, abide2_qc_joined, by="SUB_ID", all = TRUE)

# quelle difference entre variables irmab12 et irmab23? aucune
setdiff(names(irmab1), names(irmab2))
setdiff(names(irmab2), names(irmab1))
# on rajoute une colonne dataset pr?cisant si le sujet vient de "Abide1" ou de "Abide2"
irmab1$dataset="Abide1"
irmab2$dataset="Abide2"
# les noms de colonnes sont deja les memes, donc on les assemble
irm_total <- merge (irmab1, irmab2, all=T)
# extraction du vrai SUB_ID
irm_total$SUB_ID <- sub("sub-([^_]*)(_ses-.)?", "\\1", irm_total$SUB_ID)

# list of all subjects with MRI:
sub_total <- irm_total$SUB_ID
sub_total_dx <- merge(data.frame(SUB_ID=unique(sub_total)), phenot_total, all.x=T)$DX_GROUP
table(sub_total_dx)

# supression des irm non segmentes qui ont donc un NA dans irm_total$fsid (et un 0 dans les QC)
irm_total <- subset(irm_total, !is.na(irm_total$fsid))

# list of all subjects with segmented MRI:
sub_seg <- irm_total$SUB_ID
sub_seg_dx <- merge(data.frame(SUB_ID=unique(sub_seg)), phenot_total, all.x=T)$DX_GROUP
table(sub_seg_dx)

# list of subjects with failed segmentation:
sub_not_seg <- setdiff(sub_total, sub_seg)
sub_not_seg_dx <- merge(data.frame(SUB_ID=unique(sub_not_seg)), phenot_total, all.x=T)$DX_GROUP
table(sub_not_seg_dx)

# ajout du numero de session
irm_total$SES_ID <- 1
m <- regexpr("(?<=_ses-).", irm_total$fsid, perl=T)
irm_total$SES_ID[m > 1] <- as.numeric(regmatches(irm_total$fsid, m))

# elimination des doublons
# elimination des sujets de Abide 2 qui sont aussi dans Abide 1
irm_total <- subset(irm_total, !(dataset=="Abide2" & SUB_ID %in% subset(irm_total, dataset=="Abide1")$SUB_ID))
# suppression de la session 2 du sujet 28705 
irm_total <- subset(irm_total, !(irm_total$fsid == "sub-28705_ses-2") )
# recherche de doublons supplementaires : aucun
irm_total$SUB_ID[duplicated(irm_total$SUB_ID)]

# load motion tables
motab1 <- read.csv(file=file.path(raw.dir, "motion-abide1.tsv"), sep='\t', header=F, col.names=c("Subject", "motion"))
motab2 <- read.csv(file=file.path(raw.dir, "motion-abide2.tsv"), sep="\t", header=F, col.names=c("Subject", "motion"))
motion12 <- rbind(motab1, motab2)
motion12$SUB_ID <- as.numeric(sub("(sub-)?([^_]*)(_ses-.)?", "\\2", motion12$Subject))
motion12$SES_ID <- as.numeric(sub(".*_ses-(.+)", "\\1", motion12$Subject))

# identify and remove duplicates
motdup1 <- motion12[duplicated(motion12[,c("SUB_ID", "SES_ID")], fromLast = T),]
motdup2 <- motion12[duplicated(motion12[,c("SUB_ID", "SES_ID")]),]
motdup <- merge(motdup1[,c("SUB_ID", "SES_ID", "motion")], motdup2[,c("SUB_ID", "SES_ID", "motion")], by=c("SUB_ID", "SES_ID"))

motion12 <- motion12[!duplicated(motion12[,c("SUB_ID", "SES_ID")]),]

# add motion values to irm_total
irm_total <- merge(irm_total, motion12[,c("SUB_ID", "SES_ID", "motion")], by=c("SUB_ID", "SES_ID"), all.x=T, all.y=F)


#parmi les SUB_ID de phenot_total, elimination des numeros de SUB_ID absents dans irm_total2
phenot_total<- subset(phenot_total, SUB_ID %in% irm_total$SUB_ID)





#parmi les SUB_ID de irm_total, recherche des numeros de SUB_ID absents dans phenot_total : aucun
setdiff(irm_total$SUB_ID, phenot_total$SUB_ID)

#association en un seul fichier (abide_total) des donnees phenotypiques (phenot_total) et IRM (irm_total) :
abide_total <- merge(phenot_total, irm_total, by="SUB_ID", all.y = TRUE)


# DX_GROUP binaire (1:TSA, 2: control) a transformer en categoriel :
abide_total$DX_GROUP <- factor(abide_total$DX_GROUP, labels=c("ASD", "Control"))
# SEX binaire (1: male, 2:female) a transformer en categoriel :
abide_total$SEX <- factor(abide_total$SEX, labels=c("Male", "Female"))

# ajouter la colonne FIQ2(=FIQ calcules a partir de VIQ et PIQ) et FIQ total (qui inclue FIQ quand present ou FIQ2 si FIQ=NA) :
# combien de PIQ + VIQ parmis ceux sans FIQ ?
abide_no_FIQ <- subset(abide_total, !complete.cases(abide_total[,"FIQ"]))
PIQ_VIQ <- abide_no_FIQ[complete.cases(abide_no_FIQ[, c("PIQ", "VIQ")]),]
# prediction du FIQ en fonction du PIQ et VIQ : 
res.lm.FIQ <- lm(FIQ~PIQ+VIQ, data=abide_total)
res.lm.FIQ
summary(res.lm.FIQ)
# Multiple R-squared:  0.9487,	Adjusted R-squared:  0.9486 p-value: < 2.2e-16, donc bon modele 
abide_total$FIQ2=predict(res.lm.FIQ, abide_total)
# derniere etape : creation d'une colonne FIQ_total qui correspond a FIQ sauf si NA et a FIQ2 sinon + ajout de cette colonne
abide_total$FIQ_total <- abide_total$FIQ
abide_total[is.na(abide_total$FIQ_total), "FIQ_total"] <- abide_total[is.na(abide_total$FIQ_total), "FIQ2"]
summary(abide_total$FIQ_total)

# # ajouter 00 devant les SUB_ID 
# abide_total$SUB_ID <-sprintf("%07d", as.numeric(abide_total$SUB_ID))

# on veut comme reference pour le diagnostic : ASD et non controle :
abide_total$DX_GROUP<- relevel(abide_total$DX_GROUP, ref="Control")

# on enleve les colonnes "unknown" car ne correspondent pas a une region cerebrale et ajoutent du bruit :
abide_total <- abide_total[grep("unknown", names(abide_total), invert=T)]

# conversion de la colonne SITE_ID de factor a character
abide_total$SITE_ID <- as.character(abide_total$SITE_ID)

# split KKI for the different acquisitions
acq_kki <- read.csv(file=file.path(raw.dir, "acq-KKI.tsv"), sep='\t', header=F, col.names=c('fsid', 'acquisition'))
# acquisitions which were visually seen as different
kki3 <- readLines(file.path(raw.dir, "kki3.txt"))
abide_total$SITE_ID[abide_total$fsid %in% subset(acq_kki, acquisition=='acq-rc32chan')$fsid] <- "KKI_2"
abide_total$SITE_ID[abide_total$fsid %in% kki3] <- "KKI_3"

# bons nombres de sites : 
table(abide_total$SITE_ID)
# merge when acquisitions are the same for the same site
abide_total$SITE_ID[abide_total$SITE_ID=="ABIDEII-KKI_1"] <- "KKI"
abide_total$SITE_ID[abide_total$SITE_ID=="ABIDEII-NYU_1"] <- "NYU"
abide_total$SITE_ID[abide_total$SITE_ID=="ABIDEII-OHSU_1"] <- "OHSU"
abide_total$SITE_ID[abide_total$SITE_ID=="ABIDEII-SDSU_1"] <- "SDSU"
abide_total$SITE_ID[abide_total$SITE_ID=="UCLA_2"] <- "UCLA_1"
abide_total$SITE_ID[abide_total$SITE_ID=="ABIDEII-USM_1"] <- "USM"
abide_total$SITE_ID[abide_total$SITE_ID=="LEUVEN_2"] <- "LEUVEN_1"
abide_total$SITE_ID[abide_total$SITE_ID=="UM_2"] <- "UM_1"

# mettre les bons ages (age pour session 2) pour les sujets ses-2 restants dans abide_final :
phenot_long <- read.csv(file.path(raw.dir, "ABIDEII_Long_Composite_Phenotypic.csv"))
ses_2 <- abide_total$SUB_ID[abide_total$SES_ID == 2]
# 5 sujets, le sujet ONRC 28682 est en fait une session 1
ses_2 <- ses_2[ses_2 %in% phenot_long$SUB_ID]
for (subject in ses_2) {
  abide_total[abide_total$SUB_ID==subject, "AGE_AT_SCAN"] <- subset(phenot_long, SUB_ID == subject & SESSION == 'Followup_1', AGE_AT_SCAN, drop=T)
}

# abide_total = pas de retrait des mauvais QC pour voir si rend les resultats positifs
write.table(abide_total, file=file.path(derived.dir, "abide_total.txt"), sep="\t", quote=FALSE, row.names = FALSE)

# ne garder que les  QC avec : NA, 1 et 2 pour l'analyse : 
table (abide_total$QC_Left_Lateral, useNA='always')
table (abide_total$QC_Left_Medial, useNA='always')
table (abide_total$QC_Right_Lateral, useNA='always')
table (abide_total$QC_Right_Medial, useNA='always')
table (abide_total$QC_Segmentation, useNA='always')

sub_qc_LL <- abide_total$QC_Left_Lateral %in% c(NA, 1, 2)
sub_qc_LM <- abide_total$QC_Left_Medial %in% c(NA, 1, 2)
sub_qc_RL <- abide_total$QC_Right_Lateral %in% c(NA, 1, 2)
sub_qc_RM <- abide_total$QC_Right_Medial %in% c(NA, 1, 2)
sub_qc_Seg <- abide_total$QC_Segmentation %in% c(NA, 1, 2)
# il faut donc enlever les lignes avec des 0, 3 et 4 ET 10 dans l'un de ces 5 QC :

abide_final <- abide_total[sub_qc_LL & sub_qc_LM & sub_qc_RL & sub_qc_RM & sub_qc_Seg,]
table(abide_final$DX_GROUP)

# exclus pour QC :
QC3_0_4_10 <- abide_total[!(sub_qc_LL & sub_qc_LM & sub_qc_RL & sub_qc_RM & sub_qc_Seg),]
table(QC3_0_4_10$DX_GROUP)
write.table(QC3_0_4_10, file=file.path(derived.dir, "QC3_0_4_10.txt"), sep="\t", quote=FALSE, row.names = FALSE)

# donne le fsid comme nom de ligne
# row.names(abide_total) <- abide_total$fsid
row.names(abide_final) <- abide_final$fsid

# pour analyse sur l'ensemble des noeuds : cr?ation d'un tableau court (abide_final_short) avec seulement les variables d'interet
abide_final_short <- subset(abide_final[, c("SUB_ID", "SITE_ID", "DX_GROUP", "AGE_AT_SCAN", "SEX", "FIQ_total", "dataset", "fsid")])
abide_final_short <- na.omit(abide_final_short)
table(abide_final[na.action(abide_final_short), "DX_GROUP"])

# elimination des sujets avec donnees manquantes dans abide_final:
abide_final <- abide_final[row.names(abide_final_short),]
table(abide_final$DX_GROUP)

# number of subjects with imputed FIQ:
sum(!is.na(abide_final$FIQ_total)) - sum(!is.na(abide_final$FIQ))

# enlever les lignes si un SITE_ID n'a que des ASD ou que des control (vrai pour NYU_2 : 22 ASD et 0 control, KUL_3: 20 ASD et 0 control et SBL: 11 ASD et 0 control)
for (site in unique(abide_final$SITE_ID)) {
  if (length(unique(subset(abide_final, SITE_ID==site, DX_GROUP, drop = TRUE))) == 1) {
    cat(sprintf("%s:", site))
    print(table(subset(abide_final, SITE_ID==site)$DX_GROUP))
    abide_final <- subset(abide_final, SITE_ID!=site)
  }
}

#on enleve les facteurs qui sont entierement nuls de la base de donnee (enlever les SITE pour lesquels il ne reste aucun sujet, pour ne pas les compter comme un niveau de facteur) : 
abide_final$SITE_ID <- factor(abide_final$SITE_ID)
table(abide_final$DX_GROUP)

# abide_final = restant apr?s QC
write.table(abide_final, file=file.path(derived.dir, "abide_final.txt"), sep="\t", quote=FALSE, row.names = FALSE)


#creation pour analyse de tous les points irm de la matrice avec modele 1 : 
matrix.mod1 <- model.matrix(~ DX_GROUP + SEX + SITE_ID + AGE_AT_SCAN + FIQ_total + DX_GROUP*SEX, data = abide_final)
# creation d'un vecteur avec 1 pour colonne d'interet (pour afficher les couleurs des t valeurs ou p valeurs), cette colonne d'int?r?t s'appelle le contraste
ncol(matrix.mod1)
ASD_as_variable_of_interest <- as.numeric(colnames(matrix.mod1) == "DX_GROUPASD")
# creation d'un vecteur avec les noms initaux (= fsid) des sujets dans le bon ordre :
subject_names <- rownames(matrix.mod1)
# creation d'un vecteur avec le nom des colonnes
col_names <- colnames(matrix.mod1)

glmdir <- file.path(derived.dir, "glm-freesurfer")
mod1dir <- file.path(glmdir, "mod1")
dir.create(mod1dir, show=F, rec=T)

# on extrait la matrice dans matlab :
library(R.matlab)
writeMat(con=file.path(mod1dir, "matrix_mod.mat"),  X=matrix.mod1)
#on extrait la matrice, le vecteur de noms de sujets et le vecteur avec variable d'interet :
writeLines(as.character(as.numeric(colnames(matrix.mod1) == "DX_GROUPASD")), con=file.path(mod1dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod1) == "SEXFemale")), con=file.path(mod1dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod1) == "AGE_AT_SCAN")), con=file.path(mod1dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod1) == "FIQ_total")), con=file.path(mod1dir, "contrast_FIQ"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod1) == "DX_GROUPASD:SEXFemale")), con=file.path(mod1dir, "contrast_ASD_Female"), sep=" ")
writeLines(subject_names, con=file.path(mod1dir, "subject_names.txt"), sep="\n")
writeLines(col_names, con=file.path(mod1dir, "col_names.txt"), sep="\n")


#creation pour analyse de tous les points irm de la matrice avec modele 2 : 
matrix.mod2 <- model.matrix(~ DX_GROUP + SEX + SITE_ID + AGE_AT_SCAN + FIQ_total, data = abide_final)

subject_names2 <- rownames(matrix.mod2)
col_names2 <- colnames(matrix.mod2)

mod2dir <- file.path(glmdir, "mod2")
dir.create(mod2dir, show=F, rec=T)
writeMat(con=file.path(mod2dir, "matrix_mod.mat"),  X=matrix.mod2)
writeLines(as.character(as.numeric(colnames(matrix.mod2) == "DX_GROUPASD")), con=file.path(mod2dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod2) == "SEXFemale")), con=file.path(mod2dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod2) == "AGE_AT_SCAN")), con=file.path(mod2dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod2) == "FIQ_total")), con=file.path(mod2dir, "contrast_FIQ"), sep=" ")
contrast_Site <- t(sapply(grep("^SITE_", col_names2, value=T), function(x) as.numeric(colnames(matrix.mod2) == x)))
write.table(contrast_Site, file.path(mod2dir, "contrast_Site"), row.names=F, col.names=F)
writeLines(subject_names2, con=file.path(mod2dir, "subject_names.txt"), sep="\n")
writeLines(col_names2, con=file.path(mod2dir, "col_names.txt"), sep="\n")


# modele avec l'interaction diagnostic - site
matrix.mod3 <- model.matrix(~ DX_GROUP + SEX + SITE_ID + AGE_AT_SCAN + FIQ_total + DX_GROUP*SITE_ID, data = abide_final)

subject_names3 <- rownames(matrix.mod3)
col_names3 <- colnames(matrix.mod3)

mod3dir <- file.path(glmdir, "mod3")
dir.create(mod3dir, show=F, rec=T)
writeMat(con=file.path(mod3dir, "matrix_mod.mat"),  X=matrix.mod3)
writeLines(as.character(as.numeric(colnames(matrix.mod3) == "DX_GROUPASD")), con=file.path(mod3dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod3) == "SEXFemale")), con=file.path(mod3dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod3) == "AGE_AT_SCAN")), con=file.path(mod3dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod3) == "FIQ_total")), con=file.path(mod3dir, "contrast_FIQ"), sep=" ")
contrast_Dx_Site <- t(sapply(grep("DX_GROUPASD:SITE_", col_names3, value=T), function(x) as.numeric(colnames(matrix.mod3) == x)))
write.table(contrast_Dx_Site, file.path(mod3dir, "contrast_Dx_Site"), row.names=F, col.names=F)
writeLines(subject_names3, con=file.path(mod3dir, "subject_names.txt"), sep="\n")
writeLines(col_names3, con=file.path(mod3dir, "col_names.txt"), sep="\n")


# creation d'un sous groupe par site pour anlalyse des intensites sur un environnment homogene :
site_list <- levels(abide_final$SITE_ID)
for (site in site_list) {
    data_site <- subset(abide_final, abide_final$SITE_ID==site)
    n_sexes <- length(unique(data_site$SEX))
    if (n_sexes == 1) {
        matrix.mod_site <- model.matrix(~ DX_GROUP + AGE_AT_SCAN + FIQ_total, data = data_site)
    } else {
        matrix.mod_site <- model.matrix(~ DX_GROUP + SEX + AGE_AT_SCAN + FIQ_total, data = data_site)
    }
    subject_names_site <- rownames(matrix.mod_site)
    col_names_site <- colnames(matrix.mod_site)
    mod_dir_site <- file.path(glmdir, paste0("mod_", site))
    dir.create(mod_dir_site, show=F, rec=T)
    writeMat(con=file.path(mod_dir_site, "matrix_mod.mat"),  X=matrix.mod_site)
    writeLines(as.character(as.numeric(col_names_site == "DX_GROUPASD")), con=file.path(mod_dir_site, "contrast_ASD"), sep=" ")
    if (n_sexes > 1) {
        writeLines(as.character(as.numeric(col_names_site == "SEXFemale")), con=file.path(mod_dir_site, "contrast_Female"), sep=" ")
    }
    writeLines(as.character(as.numeric(col_names_site == "AGE_AT_SCAN")), con=file.path(mod_dir_site, "contrast_age"), sep=" ")
    writeLines(as.character(as.numeric(col_names_site == "FIQ_total")), con=file.path(mod_dir_site, "contrast_FIQ"), sep=" ")
    writeLines(subject_names_site, con=file.path(mod_dir_site, "subject_names.txt"), sep="\n")
    writeLines(col_names_site, con=file.path(mod_dir_site, "col_names.txt"), sep="\n")
}
writeLines(site_list, con=file.path(glmdir, "sites.txt"))


# creation d'un fichier abide_final que avec les QC 10 enleves pour voir si change les resultats : 
# enlever les lignes si un SITE_ID n'a que des ASD ou que des control (vrai pour SBL uniquement : n'a que 4 ASD et 0 control)
abide_final_Sauf_10 <- subset(abide_total, QC_Segmentation %in% c(NA, 1, 2, 3))

# enlever les lignes si un SITE_ID n'a que des ASD ou que des control
for (site in unique(abide_final_Sauf_10$SITE_ID)) {
  if (length(unique(subset(abide_final_Sauf_10, SITE_ID==site, DX_GROUP, drop = TRUE))) == 1)
    abide_final_Sauf_10 <- subset(abide_final_Sauf_10, SITE_ID!=site)
}

#on enleve les facteurs qui sont entierement nuls de la base de donnee (enlever les SITE pour lesquels il ne reste aucun sujet, pour ne pas les compter comme un niveau de facteur) : 
abide_final_Sauf_10$SITE_ID <- factor(abide_final_Sauf_10$SITE_ID)

# matrice pour abide_final_Sauf_10
matrix.mod10 <- model.matrix(~ DX_GROUP + SEX + AGE_AT_SCAN + FIQ_total + SITE_ID, data = abide_final_Sauf_10)
subject_names10 <- rownames(matrix.mod10)
col_names10 <- colnames(matrix.mod10)
mod10dir <- file.path(glmdir, "sauf_10")
dir.create(mod10dir, show=F, rec=T)
writeMat(con=file.path(mod10dir, "matrix_mod.mat"),  X=matrix.mod10)
writeLines(as.character(as.numeric(colnames(matrix.mod10) == "DX_GROUPASD")), con=file.path(mod10dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod10) == "SEXFemale")), con=file.path(mod10dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod10) == "AGE_AT_SCAN")), con=file.path(mod10dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod10) == "FIQ_total")), con=file.path(mod10dir, "contrast_FIQ"), sep=" ")
writeLines(subject_names10, con=file.path(mod10dir, "subject_names.txt"), sep="\n")
writeLines(col_names10, con=file.path(mod10dir, "col_names.txt"), sep="\n")


# modele avec l'interaction diagnostic - age
matrix.mod4 <- model.matrix(~ DX_GROUP + SEX + SITE_ID + AGE_AT_SCAN + FIQ_total + DX_GROUP*AGE_AT_SCAN, data = abide_final)

subject_names4 <- rownames(matrix.mod4)
col_names4 <- colnames(matrix.mod4)

mod4dir <- file.path(glmdir, "mod4")
dir.create(mod4dir, show=F, rec=T)
writeMat(con=file.path(mod4dir, "matrix_mod.mat"),  X=matrix.mod4)
writeLines(as.character(as.numeric(colnames(matrix.mod4) == "DX_GROUPASD")), con=file.path(mod4dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod4) == "SEXFemale")), con=file.path(mod4dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod4) == "AGE_AT_SCAN")), con=file.path(mod4dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod4) == "FIQ_total")), con=file.path(mod4dir, "contrast_FIQ"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod4) == "DX_GROUPASD:AGE_AT_SCAN")), con=file.path(mod4dir, "contrast_ASD_age"), sep=" ")
writeLines(subject_names4, con=file.path(mod4dir, "subject_names.txt"), sep="\n")
writeLines(col_names4, con=file.path(mod4dir, "col_names.txt"), sep="\n")


# modele sans le site en covariable
matrix.mod5 <- model.matrix(~ DX_GROUP + SEX + AGE_AT_SCAN + FIQ_total, data = abide_final)

subject_names5 <- rownames(matrix.mod5)
col_names5 <- colnames(matrix.mod5)

mod5dir <- file.path(glmdir, "mod5")
dir.create(mod5dir, show=F, rec=T)
writeMat(con=file.path(mod5dir, "matrix_mod.mat"),  X=matrix.mod5)
writeLines(as.character(as.numeric(colnames(matrix.mod5) == "DX_GROUPASD")), con=file.path(mod5dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod5) == "SEXFemale")), con=file.path(mod5dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod5) == "AGE_AT_SCAN")), con=file.path(mod5dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod5) == "FIQ_total")), con=file.path(mod5dir, "contrast_FIQ"), sep=" ")
writeLines(subject_names5, con=file.path(mod5dir, "subject_names.txt"), sep="\n")
writeLines(col_names5, con=file.path(mod5dir, "col_names.txt"), sep="\n")


# modele avec les interactions variables-site (not working because too much variability between columns for FreeSurfer glm)
matrix.mod6 <- model.matrix(~ (DX_GROUP + SEX + AGE_AT_SCAN + FIQ_total) * SITE_ID, data = abide_final)
# suppression des colonnes avec presque toujours la même valeur
matrix.mod6 <- matrix.mod6[,-(which(apply(matrix.mod6[,-1], 2, sd)<0.05)+1)]

subject_names6 <- rownames(matrix.mod6)
col_names6 <- colnames(matrix.mod6)

mod6dir <- file.path(glmdir, "mod6")
dir.create(mod6dir, show=F, rec=T)
writeMat(con=file.path(mod6dir, "matrix_mod.mat"),  X=matrix.mod6)
writeLines(as.character(as.numeric(colnames(matrix.mod6) == "DX_GROUPASD")), con=file.path(mod6dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod6) == "SEXFemale")), con=file.path(mod6dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod6) == "AGE_AT_SCAN")), con=file.path(mod6dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod6) == "FIQ_total")), con=file.path(mod6dir, "contrast_FIQ"), sep=" ")
contrast_Dx_Site <- t(sapply(grep("DX_GROUPASD:SITE_", col_names6, value=T), function(x) as.numeric(colnames(matrix.mod6) == x)))
write.table(contrast_Dx_Site, file.path(mod6dir, "contrast_Dx_Site"), row.names=F, col.names=F)
contrast_sex_Site <- t(sapply(grep("SEXFemale:SITE_", col_names6, value=T), function(x) as.numeric(colnames(matrix.mod6) == x)))
write.table(contrast_sex_Site, file.path(mod6dir, "contrast_sex_Site"), row.names=F, col.names=F)
contrast_age_Site <- t(sapply(grep("AGE_AT_SCAN:SITE_", col_names6, value=T), function(x) as.numeric(colnames(matrix.mod6) == x)))
write.table(contrast_age_Site, file.path(mod6dir, "contrast_age_Site"), row.names=F, col.names=F)
contrast_FIQ_Site <- t(sapply(grep("FIQ_total:SITE_", col_names6, value=T), function(x) as.numeric(colnames(matrix.mod6) == x)))
write.table(contrast_FIQ_Site, file.path(mod6dir, "contrast_FIQ_Site"), row.names=F, col.names=F)
writeLines(subject_names6, con=file.path(mod6dir, "subject_names.txt"), sep="\n")
writeLines(col_names6, con=file.path(mod6dir, "col_names.txt"), sep="\n")


# model with motion as covariate
matrix.mod7 <- model.matrix(~DX_GROUP + SEX + AGE_AT_SCAN + FIQ_total + SITE_ID + motion, data = abide_final)

subject_names7 <- rownames(matrix.mod7)
col_names7 <- colnames(matrix.mod7)

mod7dir <- file.path(glmdir, "mod7")
dir.create(mod7dir, show=F, rec=T)
writeMat(con=file.path(mod7dir, "matrix_mod.mat"),  X=matrix.mod7)
writeLines(as.character(as.numeric(colnames(matrix.mod7) == "DX_GROUPASD")), con=file.path(mod7dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod7) == "SEXFemale")), con=file.path(mod7dir, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod7) == "AGE_AT_SCAN")), con=file.path(mod7dir, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod7) == "FIQ_total")), con=file.path(mod7dir, "contrast_FIQ"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod7) == "motion")), con=file.path(mod7dir, "contrast_motion"), sep=" ")
contrast_Site <- t(sapply(grep("^SITE_", col_names7, value=T), function(x) as.numeric(colnames(matrix.mod7) == x)))
write.table(contrast_Site, file.path(mod7dir, "contrast_Site"), row.names=F, col.names=F)
writeLines(subject_names7, con=file.path(mod7dir, "subject_names.txt"), sep="\n")
writeLines(col_names7, con=file.path(mod7dir, "col_names.txt"), sep="\n")

# modele en enlevant NYU
data_nonNYU <- subset(abide_final, abide_final$SITE_ID != "NYU")
data_nonNYU$SITE_ID <- factor(data_nonNYU$SITE_ID)

matrix.mod_nonNYU <- model.matrix(~ DX_GROUP + SEX + SITE_ID + AGE_AT_SCAN + FIQ_total, data = data_nonNYU)
subject_names_nonNYU <- rownames(matrix.mod_nonNYU)
col_names_nonNYU <- colnames(matrix.mod_nonNYU)
mod_dir_nonNYU <- file.path(glmdir, "mod_nonNYU")
dir.create(mod_dir_nonNYU, show=F, rec=T)
writeMat(con=file.path(mod_dir_nonNYU, "matrix_mod.mat"),  X=matrix.mod_nonNYU)
writeLines(as.character(as.numeric(col_names_nonNYU == "DX_GROUPASD")), con=file.path(mod_dir_nonNYU, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(col_names_nonNYU == "SEXFemale")), con=file.path(mod_dir_nonNYU, "contrast_Female"), sep=" ")
writeLines(as.character(as.numeric(col_names_nonNYU == "AGE_AT_SCAN")), con=file.path(mod_dir_nonNYU, "contrast_age"), sep=" ")
writeLines(as.character(as.numeric(col_names_nonNYU == "FIQ_total")), con=file.path(mod_dir_nonNYU, "contrast_FIQ"), sep=" ")
contrast_Site <- t(sapply(grep("^SITE_", col_names_nonNYU, value=T), function(x) as.numeric(colnames(matrix.mod_nonNYU) == x)))
write.table(contrast_Site, file.path(mod_dir_nonNYU, "contrast_Site"), row.names=F, col.names=F)
writeLines(subject_names_nonNYU, con=file.path(mod_dir_nonNYU, "subject_names.txt"), sep="\n")
writeLines(col_names_nonNYU, con=file.path(mod_dir_nonNYU, "col_names.txt"), sep="\n")

# modele avec effet de polynomial de l'age
np <- 4
matrix.mod8 <- model.matrix(~ DX_GROUP + SEX + SITE_ID + poly(AGE_AT_SCAN, np) + FIQ_total, data = abide_final)

subject_names8 <- rownames(matrix.mod8)
col_names8 <- colnames(matrix.mod8)

mod8dir <- file.path(glmdir, "mod8")
dir.create(mod8dir, show=F, rec=T)
writeMat(con=file.path(mod8dir, "matrix_mod.mat"),  X=matrix.mod8)
writeLines(as.character(as.numeric(colnames(matrix.mod8) == "DX_GROUPASD")), con=file.path(mod8dir, "contrast_ASD"), sep=" ")
writeLines(as.character(as.numeric(colnames(matrix.mod8) == "SEXFemale")), con=file.path(mod8dir, "contrast_Female"), sep=" ")
for (i in 1:np) {
	writeLines(as.character(as.numeric(grepl(paste0("AGE_AT_SCAN.*", i, "$"), colnames(matrix.mod8)))), con=file.path(mod8dir, paste0("contrast_age", i)), sep=" ")
}
writeLines(as.character(as.numeric(colnames(matrix.mod8) == "FIQ_total")), con=file.path(mod8dir, "contrast_FIQ"), sep=" ")
contrast_Site <- t(sapply(grep("^SITE_", col_names8, value=T), function(x) as.numeric(colnames(matrix.mod8) == x)))
write.table(contrast_Site, file.path(mod8dir, "contrast_Site"), row.names=F, col.names=F)
writeLines(subject_names8, con=file.path(mod8dir, "subject_names.txt"), sep="\n")
writeLines(col_names8, con=file.path(mod8dir, "col_names.txt"), sep="\n")

write.model <- function(formula, data, mod_name) {
    terms <- attr(terms(formula),"term.labels")
    terms2 <- sub(".*\\((.*?)[),].*", "\\1", terms)
    data2 <- na.omit(data[,terms2])
    
    for (term in terms2) {
        if (class(data2[[term]]) == "factor")
            data2[[term]] <- factor(data2[[term]])
    }
    
    matrix.mod <- model.matrix(formula, data = data2)
    subject_names <- rownames(matrix.mod)
    col_names <- colnames(matrix.mod)
    
    mod_dir <- file.path(glmdir, mod_name)
    dir.create(mod_dir, show=F, rec=T)
    writeMat(con=file.path(mod_dir, "matrix_mod.mat"),  X=matrix.mod)
    for (term in terms2) {
        col <- grepl(term, col_names)
        ncol <- sum(col)
        if (ncol == 1) {
            if (class(data2[[term]]) == "factor") {
                contrast.names <- sub(term, "", col_names[col])
            } else {
                contrast.names <- col_names[col]
            }
            contrasts <- list(t(as.character(as.numeric(col))))
        } else if (ncol > 1) {
            if (class(data2[[term]]) == "factor") {
                contrasts <- list(t(sapply(grep(term, col_names, value=T), function(x) as.numeric(col_names == x))))
                contrast.names <- term
            } else {
                nums <- sub("^.*?(\\d+)$" ,"\\1", col_names[col])
                contrasts <- lapply(nums, function(i) t(as.character(as.numeric(grepl(paste0(term, ".*", i, "$"), col_names)))))
                contrast.names <- paste0(term, nums)
            }
        }
        for (i in 1:length(contrasts)) {
            write.table(contrasts[i], file.path(mod_dir, paste0("contrast_", contrast.names[i])), row.names=F, col.names=F, quote=F)
        }
    }
    writeLines(subject_names, con=file.path(mod_dir, "subject_names.txt"), sep="\n")
    writeLines(col_names, con=file.path(mod_dir, "col_names.txt"), sep="\n")
}

np <- 4

# modele avec effet poly de l'age et motion
write.model(~ DX_GROUP + SEX + SITE_ID + poly(AGE_AT_SCAN, np) + FIQ_total + motion, abide_final, "mod2_full")

# modele avec VINELAND_ABC_STANDARD
write.model(~ VINELAND_ABC_STANDARD + SEX + poly(AGE_AT_SCAN, np) + FIQ_total + motion, data = abide_final, "mod_vineland")

# modele avec ADI_R_SOCIAL_TOTAL_A
write.model(~ ADI_R_SOCIAL_TOTAL_A + SEX + SITE_ID + poly(AGE_AT_SCAN, np) + FIQ_total + motion, data = abide_final, "mod_adi")

# modele avec ADOS_TOTAL
write.model(~ ADOS_TOTAL + SEX + SITE_ID + poly(AGE_AT_SCAN, np) + FIQ_total + motion, data = abide_final, "mod_ados")

# modele avec SRS_RAW_TOTAL
write.model(~ SRS_RAW_TOTAL + SEX + SITE_ID + poly(AGE_AT_SCAN, np) + FIQ_total + motion, data = abide_final, "mod_srs")

