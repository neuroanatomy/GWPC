#!/usr/bin/env bash
set -e

if [[ $# -lt 1 ]]; then
    echo "Usage:"
    echo "$0 config_file.sh"
    exit 1
fi

config=$1
shift
# shellcheck source=run_glm_combat_config.sh
source "$config"

subj_file=$mod_dir/subject_names.txt
matrix=$mod_dir/matrix_mod.mat
contrasts=( "$mod_dir"/contrast_* )

export SUBJECTS_DIR=$fsdir

mkdir -p "$odir"

site_indices=($(grep -n "^$site_var" "$mod_dir"/col_names.txt | cut -d ":" -f 1))
for i in "${!contrasts[@]}"; do
    contrast="${contrasts[$i]}"
	bn=${contrast##*/}
    echo $contrast
	if [[ -n $measure_cov ]]; then
        # add a column of 0 in each contrast file
		perl -lpe 's/\s*$/ 0/' "$contrast"
	else
		cat "$contrast"
    fi | cut -f $(IFS=,; echo "${site_indices[*]}") > "$odir"/"$bn"
    if [[ $(tr ' ' '\n' < "$odir"/"$bn" | uniq | wc -l) -eq 1 ]]; then
        # contrast is no longer useful, remove it
        rm "$odir"/"$bn"
        unset "contrasts[$i]"
    fi
done

if [[ -n $measure_cov ]]; then
    # create a new contrast file with all 0 except the last column
    ncol=$(( $(head -1 "${contrasts[0]}" | wc -w) - ${#site_indices[@]} ))
    perl -e "print join(' ', (0) x $ncol, 1)" > "$odir"/contrast_$measure_cov
	contrasts+=("$odir"/contrast_"$measure_cov")
fi

for measure in "${measures[@]}"; do
    for width in "${widths[@]}"; do
        for hemi in lh rh; do
            model_name=$(basename "${matrix%.mat}")
            model_name=${model_name#matrix_}
            outname=$hemi.${measure%.mgh}.fwhm$width.$model_name
            glmdir=$odir/$outname
            mkdir -p "$glmdir"
            y_file=$glmdir/$outname.mgh
            $combat $mod_dir $hemi $measure $width $site_var $y_file
            if [[ -n $measure_cov ]]; then
                cov_file=$glmdir/$hemi.$measure_cov.fwhm$width.$model_name.mgh
                pvr_arg="--pvr $cov_file"
            else
                pvr_arg=
            fi

            unset c_arg
            for (( i=0; i<${#contrasts[*]}; ++i )); do c_arg+=(--C "${contrasts[$i]/*\//$odir/}"); done

            submit 5G mri_glmfit --surf fsaverage $hemi \
                                 --X "$matrix" \
                                 $pvr_arg \
                                 "${c_arg[@]}" \
                                 --label "$SUBJECTS_DIR/fsaverage/label/$hemi.aparc.label" \
                                 --y "$y_file" \
                                 --no-prune \
                                 --glmdir "$glmdir"
        done
    done
done
