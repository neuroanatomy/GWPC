#!/usr/bin/env Rscript

# Make linear model on grey white matter contrast
# Nicolas Traut, November 2020

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
out.dir <- file.path(derived.dir, "spin_test", "output")

dir.create(out.dir, showWarnings = F, recursive = T)

library(freesurferformats)

# 1. load demographic data
#---------------

# Note: demographic data should be the same for both hemispheres
hemi <- "lh"

X <- as.matrix(read.table(file.path(glm.dir, paste0(hemi, ".w-g.pct.30.fwhm10.mod"), "Xg.dat")))
contrast_ASD <- as.matrix(read.table(file.path(glm.dir, paste0(hemi, ".w-g.pct.30.fwhm10.mod"), "contrast_ASD", "C.dat")))

for (hemi in c("lh", "rh")) {

  # 2. Read data
  #--------------

  gwc <- read.fs.mgh(file.path(glm.dir, paste0(hemi, ".w-g.pct.30.fwhm10.mod"), paste0(hemi, ".w-g.pct.30.fwhm10.mod.mgh")))
  aparc.label <- read.fs.label(file.path(fs.dir, "fsaverage", "label", paste0(hemi, ".aparc.label")))
  aparc.mask <- logical(nrow(gwc))
  aparc.mask[aparc.label] <- TRUE
  gwc.aparc <- gwc[aparc.mask,,,]

  # 3. Test diagnostic group effect
  #---------------------------------

  print(sprintf("Test group effect on contrast for %s", hemi))

  nv <- nrow(gwc.aparc)
  ef.aparc <- double(nv)

  mod <- .lm.fit(X, t(gwc.aparc))
  ef.aparc <- contrast_ASD %*% coef(mod)

  ef <- double(nrow(gwc))
  ef[aparc.mask] <- ef.aparc

  write.fs.mgh(file.path(out.dir, sprintf("%s.ASD.mgh", hemi)), ef)
}
