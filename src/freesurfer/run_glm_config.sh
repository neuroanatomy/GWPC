# shellcheck shell=bash

if command -v sbatch > /dev/null; then
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
"$@"
EOF
    }
else
    function submit {
        shift
        echo "$@"
        "$@"
    }
fi

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
mod_dir=$derived_dir/glm-freesurfer/mod2
#mod_dir=$1
#site=${mod_dir##*mod_}
odir=$fsdir/glm_mod2
#odir=$fsdir/glm_mod_${site}
subj_file=$mod_dir/subject_names.txt
matrix=$mod_dir/matrix_mod.mat
contrasts=( "$mod_dir"/contrast_* )
#pcts=(.00 .10 .20 .30 .40 .50 .60)
pcts=(.30)
measures=( "${pcts[@]/#/w-g.pct}" )
measures=( "${measures[@]/%/.mgh}" )
measures+=(thickness)
#measures+=(`printf "gm%s.mgh\n" "${pcts[@]}"`)
#measures+=(wm_1mm.mgh)
#measures=( "${measures[@]/%/.norm}" )
#widths=(5 10)
widths=(10)

# additionnal surface file to be used as covariate
measure_cov=
# measure_cov=thickness

export odir
export subj_file
export matrix
export contrasts
export measures
export widths
export measure_cov
