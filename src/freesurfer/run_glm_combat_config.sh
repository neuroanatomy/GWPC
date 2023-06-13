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
$@
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
combat=$script_dir/combat.R

#mod_dir=$1
mod_dir=$derived_dir/glm-freesurfer/mod2
#site=${mod_dir##*mod_}
fsdir=$derived_dir/fs-6.0.0
odir=$fsdir/glm_mod2_combat
#odir=$fsdir/glm_mod_${site}
#pcts=(.00 .10 .20 .30 .40 .50 .60)
pcts=(.30)
measures=( "${pcts[@]/#/w-g.pct}" )
measures=( "${measures[@]/%/.mgh}" )
measures+=(thickness)
#measures+=(`printf "gm%s.mgh\n" "${pcts[@]}"`)
#measures+=(wm_1mm.mgh)
#measures=( "${measures[@]/%/.norm}" )
measures+=(fwhm2.bsc)
#widths=(5 10)
widths=(10)

# additionnal surface file to be used as covariate
measure_cov=
# measure_cov=curv

site_var=SITE_ID

export odir
export mod_dir
export measures
export widths
export measure_cov
export combat
export site_var
