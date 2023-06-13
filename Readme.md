[![CircleCI](https://circleci.com/gh/neuroanatomy/GWPC.svg?style=svg)](https://circleci.com/gh/neuroanatomy/GWPC)
# GWPC

This repository contains the code used for the analyses described in our manuscript "Increased contrast of the grey-white matter boundary in the motor, visual and auditory areas in Autism Spectrum Disorders" (Preprint: https://doi.org10.1101/750117).

We used this code to analyse the grey-white contrast in Autism Spectrum Disorder using magnetic resonance imaging from ABIDE 1&2 datasets (http://fcon_1000.projects.nitrc.org/indi/abide/).

## Dependencies
[FreeSurfer](https://surfer.nmr.mgh.harvard.edu/), [surfstat](https://www.math.mcgill.ca/keith/surfstat/), [RFT_FDR](https://www.nitrc.org/projects/rft_fdr/), [R](https://www.r-project.org/), [Python](https://www.python.org/) and [Octave](https://octave.org/) are needed for the whole process.
One can create a directory `bin/modules` and put `surfstat` and `RFT_FDR` in.

### Octave
- add the modules to `~/.octaverc` (you may need to adapt `surfstat` and `RFT_FDR` paths to match yours):
```
echo "addpath('$PWD/bin/modules/surfstat');" >> ~/.octaverc
echo "addpath('$PWD/bin/modules/RFT_FDR');" >> ~/.octaverc
echo "addpath([getenv('FREESURFER_HOME') '/matlab']);" >> ~/.octaverc
```

- patch the surfstat module to run with octave:
```
patch -p0 -d bin/modules -i ../../src/freesurfer/surfstat.diff
```
## Preprocessing

Combine and format abide 1&2 tables for GLM analyses:
```
Rscript src/preprocessing/prepare_phenotypes.R
```

Run FreeSurfer recon-all pipeline on each subject:
```
recon-all -subjid $subjid -i $subjanat -autorecon-all
```

Compute grey/white matter contrast at different levels and cortical thickness for each subject:
```
bash src/preprocessing/resample.sh $subjdir
```

Compute BSC (boundary sharpness coefficient) values for each subject:
```
bash src/preprocessing/bsc.sh $subjdir
```

## Analyses

To run FreeSurfer GLM, adapt `src/freesurfer/run_glm_config.sh` and run:
```
bash src/freesurfer/run_glm.sh src/freesurfer/run_glm_config.sh
```

To get cluster-wise statistically significant clusters with RFT and Monte Carlo simulations, adapt `src/freesurfer/correct_glm_config.sh` and run:
```
bash src/freesurfer/correct_glm.sh src/freesurfer/correct_glm_config.sh
```

To plot the figures of the effect for each contrast, adapt `src/freesurfer/plot_figures_config.py` and run:
```
python3 src/freesurfer/plot_figures.py
```
