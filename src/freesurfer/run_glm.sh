#!/usr/bin/env bash
set -e

if [[ $# -lt 1 ]]; then
    echo "Usage:"
    echo "$0 config_file.sh"
    exit 1
fi

config=$1
shift
# shellcheck source=run_glm_config.sh
source "$config"

export SUBJECTS_DIR=$fsdir
readarray -t subj_list < "$subj_file"

mkdir -p "$odir"

for contrast in "${contrasts[@]}"; do
	bn=${contrast##*/}
	if [[ -n $measure_cov ]]; then
        # add a column of 0 in each contrast file
		perl -lpe 's/\s*$/ 0/' "$contrast" > "$odir"/"$bn"
	else
		cp "$contrast" "$odir"/"$bn"
	fi
done
contrasts=("${contrasts[@]/*\//$odir/}")

if [[ -n $measure_cov ]]; then
    # create a new contrast file with all 0 except the last column
	perl -lpe 's/\S+/0/g; s/\S+\s*$/1/; exit if $.>1' "${contrasts[0]}" > "$odir"/contrast_$measure_cov
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
            file_list=("${subj_list[@]/#/$SUBJECTS_DIR/}")
            file_list=("${file_list[@]/%//surf/$hemi.$measure.fwhm$width.fsaverage.mgh}")
            y_file=$glmdir/$outname.mgh
            mri_concat "${file_list[@]}" --o "$y_file"
            if [[ -n $measure_cov ]]; then
                file_list2=("${subj_list[@]/#/$SUBJECTS_DIR/}")
                file_list2=("${file_list2[@]/%//surf/$hemi.$measure_cov.fwhm$width.fsaverage.mgh}")
                cov_file=$glmdir/$hemi.$measure_cov.fwhm$width.$model_name.mgh
                mri_concat "${file_list2[@]}" --o "$cov_file"
                pvr_arg="--pvr $cov_file"
            else
                pvr_arg=
            fi

            unset c_arg
            for (( i=0; i<${#contrasts[*]}; ++i )); do echo ${contrasts[$i]}; c_arg+=(--C "${contrasts[$i]}"); done

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
