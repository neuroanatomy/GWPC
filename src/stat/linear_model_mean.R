# Make linear model on grey white matter contrast
# Roberto Toro, January 2019
# Nicolas Traut, January 2023

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

abide_final<-read.table(file.path(derived.dir, "abide_final.txt"), sep = '\t', check.names = FALSE, header = TRUE, stringsAsFactors = TRUE)
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
# rg=ng=rw=nw=rgwc=ngwc=g=c()

# compute mean gwc per subject
dd$mgwc <- colMeans(gwc)

# do meta analysis on mean gwc
library(meta)

sites <- unique(site)
models <- sapply(sites, function(st){
  site_dd <- subset(dd, site==st)
  if (length(unique(site_dd$SEX)==1)) {
    mod <- lm(mgwc~DX_GROUP+AGE_AT_SCAN+FIQ_total, data=site_dd)
  } else{
    mod <- lm(mgwc~DX_GROUP+AGE_AT_SCAN+SEX+FIQ_total, data=site_dd)
  }
  return(summary(mod))
}, simplify = FALSE)
names(models) <- sites

data_meta <- data.frame(
  TE = sapply(models, function(mod) coef(mod)["DX_GROUPASD", "Estimate"]),
  seTE = sapply(models, function(mod) coef(mod)["DX_GROUPASD", "Std. Error"]),
  studlab = names(models)
)
res.meta <- metagen(TE=TE, seTE=seTE, studlab=studlab, data=data_meta)
res.meta$n.c <- sapply(sites, function(st){sum(subset(dd, site==st, DX_GROUP) == "Control")})
res.meta$n.e <- sapply(sites, function(st){sum(subset(dd, site==st, DX_GROUP) == "ASD")})
res.meta$label.e <- 'ASD'
forest(res.meta)

res.meta_nonNYU <- metagen(TE=TE, seTE=seTE, studlab=studlab,
                           data=subset(data_meta, studlab != 'NYU'))
res.meta_nonNYU$n.c <- sapply(sites, function(st){sum(subset(dd, site==st, DX_GROUP) == "Control")})
res.meta_nonNYU$n.e <- sapply(sites, function(st){sum(subset(dd, site==st, DX_GROUP) == "ASD")})
res.meta_nonNYU$label.e <- 'ASD'
forest(res.meta_nonNYU)

# look at residuals on nyu
dd_nyu <- subset(dd, site=='NYU')
mod_nyu <- lm(mgwc~DX_GROUP+AGE_AT_SCAN+SEX+FIQ_total, data=dd_nyu)
# plot(mod_nyu)

# global linear model
mod <- lm(mgwc~DX_GROUP+AGE_AT_SCAN+SEX+FIQ_total+site, data=dd)

# meta analysis on cohen's d from residuals
data_meta_cont <- as.data.frame(t(sapply(sites, function(st){
  site_dd <- subset(dd, site==st)
  if (length(unique(site_dd$SEX)==1)) {
    mod <- lm(mgwc~AGE_AT_SCAN+FIQ_total, data=site_dd)
  } else{
    mod <- lm(mgwc~AGE_AT_SCAN+SEX+FIQ_total, data=site_dd)
  }
  res <- resid(mod)
  res.e <- res[site_dd$DX_GROUP=='ASD']
  res.c <- res[site_dd$DX_GROUP=='Control']
  return(c(n.e=length(res.e), mean.e=mean(res.e), sd.e=sd(res.e),
           n.c=length(res.c), mean.c=mean(res.c), sd.c=sd(res.c)))
}, simplify = TRUE)))
data_meta_cont$study <- sites

res.meta_cont <- metacont(n.e=n.e, mean.e=mean.e, sd.e=sd.e,
                     n.c=n.c, mean.c=mean.c, sd.c=sd.c,
                     studlab=study, data=data_meta_cont)
forest(res.meta_cont)
