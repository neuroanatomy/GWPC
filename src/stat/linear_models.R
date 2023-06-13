# Make linear model on grey white matter contrast
# Roberto Toro, January 2019
# Nicolas Traut, April 2019

library(R.matlab)
library(matrixStats)

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

fs.dir <- file.path(derived.dir, "fs-6.0.0")
glm.dir <- file.path(fs.dir, "glm_mod2")
pheno.dir <- file.path(derived.dir, "glm-freesurfer", "mod2")

# setwd("~/Documents/_03_Papers/2018_10Contrast-Fouquet/analysis/")
setwd(script.dir)

source("load.mgh.R")

# 1. Read data
#--------------

gwc <- do.call(rbind, lapply(c("lh", "rh"), function(hemi) {
  # get labels
  f <- file(file.path(fs.dir, "fsaverage", "label", paste0(hemi, ".aparc.annot")), "rb")
  n <- readBin(f, integer(), endian="big")
  d <- readBin(f, integer(), n=2*n, endian="big")
  close(f)
  v <- d[seq(1, n*2, 2)]
  l <- d[seq(2, n*2, 2)]
  # remove label 0
  no0 <- l>0
  v <- v[no0]
  l <- l[no0]
  
  # load contrast values
  tmp <- load.mgh(file.path(glm.dir, paste0(hemi, ".w-g.pct.30.fwhm10.mod"), paste0(hemi, ".w-g.pct.30.fwhm10.mod.mgh")))
  gwc <- tmp$x
  dim(gwc)<-c(n,tmp$nframes)
  
  # keep only vertices with label
  gwc<-gwc[no0,]
  
  gwc
}))

nv <- nrow(gwc)

abide_final<-read.table(file.path(derived.dir, "abide_final.txt"), sep = '\t',
                        check.names = FALSE, header = TRUE, stringsAsFactors = TRUE)
row.names(abide_final) <- abide_final$fsid

# on veut comme référence pour le diagnostic Control et pour le SEX Male :
abide_final$DX_GROUP<- relevel(abide_final$DX_GROUP, ref="Control")
abide_final$SEX<- relevel(abide_final$SEX, ref="Male")

# load demographic data
# dd<-read.csv(file=file.path(pheno.dir, "matrix_mod.mat"), header=FALSE, sep=",")
subject_names_site <- readLines(con=file.path(pheno.dir, "subject_names.txt"))
dd <- abide_final[subject_names_site,]

dx <- dd[, "DX_GROUP"]
sex <- dd[, "SEX"]
age <- dd[, "AGE_AT_SCAN"]
iq <- dd[, "FIQ_total"]
site <- dd[, "SITE_ID"]

# fit a model with age, sex and iq for each vertex
# get residuals
# get covariate-corrected grey and white matter values at mean(age), mean(iq) and mean(sex)
print("Covary age, sex and iq from Grey matter values")
rg=ng=rw=nw=rgwc=ngwc=g=c()

# 3. Test diagnostic group effect
#---------------------------------
# find most peak of significance
print("Test group effect on contrast")
epsq_df <- data.frame()
pepsq_df <- data.frame()
library(rbenchmark)

for (i in 1:nv) {
  if(i%%10000 == 0) { print(i) }
  gwcv <- gwc[i,]
  mod <- lm(gwcv~dx+age+sex+iq+site)
  mod.drop1 <- drop1(mod, test='F')
  SST <- sum((mod$model[[1]] - mean(mod$model[[1]]))^2)
  fstat <- mod.drop1[, "F value", drop=F]
  df1 <- mod.drop1$Df
  df2 <- mod$df.residual
  epsq <- (fstat - 1) / fstat * mod.drop1[, "Sum of Sq"] / SST
  pepsq <- (fstat - 1) / (fstat + df2 / df1)
  names(epsq) <- NULL
  names(pepsq) <- NULL

  epsq_df <- rbind(epsq_df, t(epsq[-1,,drop=F]))
  pepsq_df <- rbind(pepsq_df, t(pepsq[-1,,drop=F]))
}

apply(epsq_df, 2, summary)
apply(pepsq_df, 2, summary)

library(ggplot2)

ggplot(epsq_df, aes(x=1, y=site)) + 
  geom_violin()

library(reshape2)
mdat <- melt(t(as.matrix(epsq_df)))
ggplot(mdat,aes(x=Var1,y=value))+geom_violin()+
  labs(x="variable")
