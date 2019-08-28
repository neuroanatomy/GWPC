#!/bin/bash
set -e

function submit {
    mem=$1
    shift
    mkdir -p log
    prog=`basename $1`
    sbatch  -J      $prog \
            --qos   ghfc \
            -p      ghfc \
            --mem   $mem \
            -e      log/$prog-%j.out \
            -o      log/$prog-%j.out \
<<- EOF
#!/bin/bash
set -e
echo "$@"
$@
EOF
}

site=$1

script_dir=$(cd "$(dirname $0)" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
odir=$fsdir/glm_mod2_thickness
mod_dir=$derived_dir/glm-freesurfer/mod2
subj_file=$mod_dir/subject_names.txt
matrix=$mod_dir/matrix_mod.mat
contrasts=($mod_dir/contrast_*)
pcts=(.00 .10 .20 .30 .40 .50 .60)
#pcts=(.30)
measures=(`printf "w-g.pct%s.mgh\n" "${pcts[@]}"`)
# measures+=(thickness)
#measures+=(`printf "gm%s.mgh\n" "${pcts[@]}"`)
#measures+=(wm_1mm.mgh)
#widths=(5 10)
widths=(10)
# measure_cov=
measure_cov=thickness

export SUBJECTS_DIR=$fsdir
subj_list=`cat $subj_file`

mkdir -p $odir

for contrast in ${contrasts[@]}; do
	bn=${contrast##*/}
	if [[ -n $measure_cov ]]; then
		perl -lpe 's/\s*$/ 0/' $contrast > $odir/$bn
	else
		cp $contrast $odir/$bn
	fi
done

if [[ -n $measure_cov ]]; then
	perl -lpe 's/\S+/0/g; s/\s*$/ 1/; exit if $.>1' $contrasts > $odir/contrast_$measure_cov
	contrasts+=($odir/contrast_$measure_cov)
fi

for measure in ${measures[@]}; do
    for width in ${widths[@]}; do
        for hemi in lh rh; do
            model_name=`basename ${matrix%.mat}`
            model_name=${model_name#matrix_}
            outname=$hemi.${measure%.mgh}.fwhm$width.$model_name
            glmdir=$odir/$outname
            mkdir -p $glmdir
            file_list=`printf $SUBJECTS_DIR"/%s/surf/$hemi.$measure.fwhm$width.fsaverage.mgh\n" $subj_list`
            y_file=$glmdir/$outname.mgh
            mri_concat $file_list --o $y_file
            if [[ -n $measure_cov ]]; then
                file_list2=`printf $SUBJECTS_DIR"/%s/surf/$hemi.$measure_cov.fwhm$width.fsaverage.mgh\n" $subj_list`
                cov_file=$glmdir/$hemi.$measure_cov.fwhm$width.$model_name.mgh
                mri_concat $file_list2 --o $cov_file
                pvr_arg="--pvr $cov_file"
            else
                pvr_arg=
            fi

            submit 5G mri_glmfit --surf fsaverage $hemi \
                                 --X $matrix \
                                 $pvr_arg \
                                 ${contrasts[@]/*\//--C $odir\/} \
                                 --label $SUBJECTS_DIR/fsaverage/label/$hemi.aparc.label \
                                 --y $y_file \
                                 --no-prune \
                                 --glmdir $glmdir
        done
    done
done
