#!/usr/bin/env bash
set -e

if [[ $# -ne 1 ]]; then
    echo "Usage:"
    echo "$0 config_file.sh"
    exit 1
fi

config=$1
# shellcheck source=correct_glm_config.sh
source "$config"

export SUBJECTS_DIR=$fsdir

# shellcheck disable=SC2206
glmdirs=($moddirpattern/?h.$glmpattern)
for glmdir in "${glmdirs[@]}"; do
	[[ ! -d $glmdir ]] && echo "$glmdir": not found && exit 1
	bn=$(basename "$glmdir")
	surf=$glmdir/$bn.mgh
	hemi=${bn:0:2} 
	submit 10G $script_dir/run_RFT_mod.m "$surf" "$glmdir" "$fsdir"/fsaverage "$hemi".aparc.label 0.01 0.05 2 2
	submit 2G $script_dir/run_mri_glmfit-sim.sh --glmdir "$glmdir" --mczsim 2 abs --2spaces --cwp 0.05
done
