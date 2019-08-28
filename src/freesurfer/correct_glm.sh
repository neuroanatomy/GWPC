#!/bin/bash
set -e

function submit {
	mem=$1
	shift
	mkdir -p log
	prog=`basename $1`
	sbatch	-J		$prog \
			--qos	ghfc \
			-p 		ghfc \
			--mem	$mem \
			-e		log/$prog-%j.out \
			-o		log/$prog-%j.out \
	<<- EOF
			#!/bin/bash
			set -e
			echo "$@"
			$@
	EOF
}

shopt -s extglob

script_dir=$(cd "$(dirname $0)" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
moddir=$fsdir/glm_!(m*|*[bsk])
#moddir=$fsdir/glm_*_thickness
#moddir=$fsdir/glm_mod2
#glmpattern=w-g.pct.30.fwhm10.*
#glmpattern=thickness.fwhm10.*
glmpattern=*

export SUBJECTS_DIR=$fsdir

for glmdir in $moddir/?h.$glmpattern; do
	[[ ! -d $glmdir ]] && echo $glmdir: not found && exit 1
	bn=`basename $glmdir`
	surf=$glmdir/$bn.mgh
	hemi=${bn:0:2} 
	submit 10G ./run_RFT_mod.m $surf $glmdir $fsdir/fsaverage $hemi.aparc.label 0.01 0.05 2 2
	submit 2G ./run_mri_glmfit-sim.sh --glmdir $glmdir --mczsim 2 abs --2spaces --cwp 0.05
done
