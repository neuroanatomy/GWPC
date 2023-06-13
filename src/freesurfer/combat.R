#!/usr/bin/env Rscript

library(R.matlab)
library(matrixStats)
library(freesurferformats)
library(neuroCombat)

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

setwd(script.dir)


args = commandArgs(trailingOnly=TRUE)
if (length(args) != 6)
      stop("Usage: combat.R mod.dir hemi measure fwhm site.var out.file")

mod.dir <- args[1]
hemi <- args[2]
measure <- args[3]
fwhm <- args[4]
site.var <- args[5]
out.file <- args[6]


fs.dir <- Sys.getenv("SUBJECTS_DIR")
mod <- readMat(file.path(mod.dir, 'matrix_mod.mat'))$X
subject_names <- readLines(con=file.path(mod.dir, "subject_names.txt"))
colnames <- readLines(file.path(mod.dir, 'col_names.txt'))
site_cols <- grep(paste0("^", site.var), colnames)
site <- mod[, site_cols] %*% seq(length(site_cols))
mod.red <- mod[, -site_cols]

hemispheres <- c(lh="left", rh="right")

cat(sprintf("Harmonizing %s for %s hemisphere...\n", measure, hemispheres[hemi]))

surf.file <- file.path('surf', sprintf('%s.%s.fwhm%s.fsaverage.mgh', hemi, measure, fwhm))
y <- sapply(subject_names, function(sub) read.fs.mgh(file.path(fs.dir, sub, surf.file)))
nv <- nrow(y)
ns <- ncol(y)

# get labels
cortex.file <- file.path(fs.dir, "fsaverage", "label", paste0(hemi, ".cortex.label"))
cortex.label <- read.fs.label(cortex.file)
cortex.mask <- logical(nv)
cortex.mask[cortex.label] <- TRUE
y.cortex <- y[cortex.mask,]

combat.harmonized <- neuroCombat(dat=y.cortex, batch=site, mod=mod.red)

measure.harmonized <- matrix(0, nv, ns)
measure.harmonized[cortex.mask,] <- combat.harmonized$dat.combat
dim(measure.harmonized) <- c(nv, 1, 1, ns)

dir.create(dirname(out.file), showWarnings = FALSE)
write.fs.mgh(out.file, measure.harmonized)
