library(freesurferformats)

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2)
    stop("Usage: bsc.R sub.dir fwhm")

sub.dir <- args[1]
fwhm <- args[2]
bsc.dir <- file.path(sub.dir, "surf", "bsc")
frac <- c(seq(-0.25, 0.5, 0.0625)) * 100

hemispheres <- c(lh="left", rh="right")

for (hemi in names(hemispheres)) {
    cat(sprintf("Computing BSC for %s hemisphere...\n", hemispheres[hemi]))
    
    intensities <- sapply(frac, function(f) read.fs.mgh(file.path(bsc.dir, sprintf("%s.%.4f.fwhm%s.mgh", hemi, f/100, fwhm))))
    nv <- nrow(intensities)
    
    cortex.label = read.fs.label(file.path(sub.dir, "label", paste0(hemi, ".cortex.label")))
    cortex.mask = logical(nrow(intensities))
    cortex.mask[cortex.label] <- TRUE
    cortex.mask[apply(intensities==0, 1, any)] <- FALSE
    cortex.mask[apply(intensities, 1, function(x) length(unique(x[1:9]))) < 4] <- FALSE
    intensities.cortex <- intensities[cortex.mask,]
    
    system.time(bsc <- t(sapply(1:nrow(intensities.cortex), function(i) {
        int <- intensities.cortex[i,]
        fm1 <- suppressWarnings(nls(int ~ a + exp(k)/2 - exp(k)/(1+exp(-c*(frac-d))), algorithm="port",
                                    start=list(a=mean(int[1:9]), k=log(4*sd(int[1:9])), c=0.1, d=0),
                                    lower = list(a=min(int), k=0, c=0, d=-50),
                                    upper = list(a=2000, k=100, c=1, d=50),
                                    control = nls.control(warnOnly = T)))
        c(coef(fm1), SSE=sum(resid(fm1)^2), conv=fm1$convergence)
    })))
    
    cat(sprintf("Number of vertices not converged: %d/%d\n", sum(bsc[, "conv"]), nrow(bsc)))
    
    cat("SSE:\n")
    print(summary(bsc[,"SSE"]))
    
    dev <- bsc[,"SSE"] / rowSums(intensities.cortex^2)
    cat("Relative deviance:\n")
    print(summary(dev))
    
    data.bsc <- double(nv)
    data.bsc[cortex.mask] <- bsc[, "c"]
    bsc.file <- file.path(bsc.dir, sprintf("%s.fwhm%s.bsc.mgh", hemi, fwhm))
    cat(sprintf("Writing BSC in: %s\n", bsc.file))
    write.fs.mgh(bsc.file, data.bsc)
    
    data.sse <- double(nv)
    data.sse[cortex.mask] <- bsc[, "SSE"]
    sse.file <- file.path(bsc.dir, sprintf("%s.fwhm%s.sse.mgh", hemi, fwhm))
    cat(sprintf("Writing SSE in: %s\n", sse.file))
    write.fs.mgh(sse.file, data.sse)
}
