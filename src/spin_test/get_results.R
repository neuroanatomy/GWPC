#!/usr/bin/env Rscript

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
out.dir <- file.path(derived.dir, "spin_test", "random/output")

library(foreach)

result_files <- dir(out.dir, "ASDr.*\\.txt")
result_table <- foreach(result_file=result_files, .combine=rbind) %do%
  read.table(file.path(out.dir, result_file), header=T)
rownames(result_table) <- sub("ASDr(.*).txt", "\\1", result_files)

result_init_file <- file.path(out.dir, "ASD.txt")
result <- read.table(result_init_file, header=T)

plot(hist(result_table$pval_interp, breaks=20))
ks.test(result_table$pval_interp,"punif",0,1)

# 95% CI and p-value from the spin test
ci <- qnorm(c(0.025, 0.975), result$realrho, result$stdrho, lower.tail = T)
# two tailed p-val from one tailed p-val
pval2 <- 1- abs(result$pval_interp*2 - 1)

plot(density(result_table$realrho))
abline(v=result$realrho)

# p-value from the labels shuffling
# one tailed p-value
pval_shuff <- mean(result$realrho < result_table$realrho)
# two tailed p-value
pval_shuff2 <- 1- abs(pval_shuff*2 - 1)
# normality test for shuffled correlations
ks.test(result_table$realrho, "pnorm", 0, sd(result_table$realrho))
# interpolated pvalue
pval_shuff_interp <- pnorm(result$realrho, 0, sd(result_table$realrho), lower.tail = F)
# two-tailed
pval_shuff_interp2 <- 1 - abs(pval_shuff_interp*2 - 1)
ci_shuff <- qnorm(c(0.025, 0.975), result$realrho, sd(result_table$realrho), lower.tail = T)
