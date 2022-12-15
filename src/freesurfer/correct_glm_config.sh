# shellcheck shell=bash

function submit {
    mem=$1
    shift
    mkdir -p log
    prog=$(basename "$1")
    sbatch  -J      "$prog" \
            --qos   ghfc \
            -p      ghfc \
            --mem   "$mem" \
            -e      log/"$prog"-%j.out \
            -o      log/"$prog"-%j.out \
<<- EOF
#!/bin/bash
set -e
echo "$@"
eval "$@"
EOF
}

shopt -s extglob

which octave &> /dev/null || module load octave

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
#moddirpattern="$fsdir/glm_!(m*|*[bsk])"
#moddirpattern=$fsdir/glm_*_thickness
#moddirpattern=$fsdir/glm_@(mod8|non*)
moddirpattern=$fsdir/glm_mod2
glmpattern="w-g.pct.30.fwhm10.*"
#glmpattern="thickness.fwhm10.*"
#glmpattern="fwhm2.bsc.fwhm10*"

export moddirpattern
export glmpattern
