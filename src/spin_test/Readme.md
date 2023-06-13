# Spin tests

## Installation of spin-test module

- install the octave statistics package. Open an octave command prompt and run:
```
pkg install -forge statistics
```
- go to the `bin/modules` folder and clone the spin-test repo:
```
git clone https://github.com/spin-test/spin-test
```
- go to the `spin-test` folder and patch the spin-test module to give additional results and run with octave:
```
git apply ../../../src/spin_test/spin-test.patch
```
- add the spin-test module to `~/.octaverc`:
```
echo "addpath('$PWD');" >> ~/.octaverc
echo "pkg load statistics;" >> ~/.octaverc
```

## Workflow

- run the original linear model with `linear_models.init.R`. This will help to check that we are getting same results as in Freesurfer
- run the linear model with shuffled labels with `linear_models.par.R`
- adjust the folders in `spin_test.m` to match your config
- run the spin-tests on original and shuffled results with `compute_pvals.sh`
- compute p-values and confidence intervals for spin and permutation tests with `get_results.R`
