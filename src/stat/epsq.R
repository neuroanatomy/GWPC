# Get epsilon squared for each covariate and each vertex and compute mean
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
glm.dir <- file.path(fs.dir, "glm_nonNYU")
pheno.dir <- file.path(derived.dir, "glm-freesurfer", "mod_nonNYU")
measure <- "fwhm2.bsc.fwhm10"

setwd(script.dir)

source("load.mgh.R")

# 1. Read data
#--------------

no0 <- sapply(c("lh", "rh"), function(hemi) {
  # get labels
  f <- file(file.path(fs.dir, "fsaverage", "label", paste0(hemi, ".aparc.annot")), "rb")
  n <- readBin(f, integer(), endian="big")
  d <- readBin(f, integer(), n=2*n, endian="big")
  close(f)
  v <- d[seq(1, n*2, 2)]
  l <- d[seq(2, n*2, 2)]
  # remove label 0
  l>0
}, simplify=F)

epsq <- do.call(rbind, lapply(c("lh", "rh"), function(hemi) {
  mod.dir <- file.path(glm.dir, paste0(hemi, ".", measure, ".mod"))
  contrast_dirs <- dir(mod.dir, "contrast_*")
  contrasts <- sub("contrast_*", "", contrast_dirs)
  epsq <- sapply(contrasts, function(x) {
    # load epsq values
    tmp <- load.mgh(file.path(mod.dir, paste0("contrast_", x), "epsq.mgh"))
    tmp$x
  })
  
  # keep only vertices with label
  epsq <- epsq[no0[[hemi]],]
  
  epsq
}))

nv <- nrow(epsq)

apply(epsq, 2, summary)
apply(epsq, 2, describe.numeric)

pepsq <- do.call(rbind, lapply(c("lh", "rh"), function(hemi) {
  mod.dir <- file.path(glm.dir, paste0(hemi, ".", measure, ".mod"))
  contrast_dirs <- dir(mod.dir, "contrast_*")
  contrasts <- sub("contrast_*", "", contrast_dirs)
  pepsq <- sapply(contrasts, function(x) {
    # load epsq values
    tmp <- load.mgh(file.path(mod.dir, paste0("contrast_", x), "pepsq.mgh"))
    tmp$x
  })
  
  # keep only vertices with label
  pepsq <- pepsq[no0[[hemi]],]
  
  pepsq
}))

r2 <- 1 - epsq / pepsq + epsq
apply(r2, 2, summary)
apply(r2, 2, describe.numeric)
