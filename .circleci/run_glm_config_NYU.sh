# shellcheck shell=bash

function submit {
	shift
	echo "$@"
	"$@"
}

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
odir=$fsdir/glm_NYU
mod_dir=$derived_dir/glm-freesurfer/mod_NYU
subj_file=$mod_dir/subject_names.txt
matrix=$mod_dir/matrix_mod.mat
contrasts=( "$mod_dir"/contrast_ASD )
#pcts=(.00 .10 .20 .30 .40 .50 .60)
pcts=(.30)
measures=( "${pcts[@]/#/w-g.pct}" )
measures=( "${measures[@]/%/.mgh}" )
# measures+=(thickness)
#measures+=(`printf "gm%s.mgh\n" "${pcts[@]}"`)
#measures+=(wm_1mm.mgh)
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
