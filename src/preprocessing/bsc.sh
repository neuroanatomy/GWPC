#!/bin/bash
set -e

sub_dir=$1
sub=$(basename $sub_dir)
export SUBJECTS_DIR=$(cd $sub_dir/.. && pwd)
sub_dir=$SUBJECTS_DIR/$sub # to ensure that sub_dir is an absolute path

script_dir=$(cd $(dirname $0); pwd)

fwhm=2
fwhm2=10

bsc_dir=$sub_dir/surf/bsc
mkdir -p $bsc_dir
for hemi in lh rh; do
	for frac in $(seq -0.2500 0.0625 0.5000); do
		# extract cortical intensities
		cmd="mri_vol2surf --src $sub_dir/mri/rawavg.mgz --hemi $hemi \
			--noreshape --projfrac $frac --out $bsc_dir/$hemi.$frac.mgh \
			--interp trilinear --regheader $sub --cortex"
		echo $cmd
		$cmd

		# apply smoothing
		cmd="mri_surf2surf --prune --s $sub --hemi $hemi --fwhm $fwhm \
			--sval $bsc_dir/$hemi.$frac.mgh --tval $bsc_dir/$hemi.$frac.fwhm$fwhm.mgh \
			--cortex"
		echo $cmd
		$cmd
	done
done

# comupte bsc
cmd="Rscript $script_dir/bsc.R $sub_dir $fwhm"
echo $cmd
$cmd

# resampling to fsaverage
for hemi in lh rh; do
	cmd="mri_surf2surf --hemi $hemi --srcsubject $sub --trgsubject fsaverage \
		--sval $bsc_dir/$hemi.fwhm$fwhm.bsc.mgh \
		--tval $bsc_dir/$hemi.fwhm$fwhm.bsc.fsaverage.mgh \
		--sfmt curv --noreshape --cortex"
	echo $cmd
	$cmd

	# apply smoothing
	cd $sub_dir/surf
	cmd="mri_surf2surf --prune --s fsaverage --hemi $hemi --fwhm $fwhm2 \
		--sval $bsc_dir/$hemi.fwhm$fwhm.bsc.fsaverage.mgh \
		--tval $bsc_dir/$hemi.fwhm$fwhm.bsc.fwhm$fwhm2.fsaverage.mgh \
		--cortex"
	echo $cmd
	$cmd
done

# remove intermediate files and move output to surf dir
shopt -s extglob
rm $bsc_dir/?h.+([-0-9.])?(.fwhm$fwhm).mgh
mv $bsc_dir/?h.fwhm$fwhm.@(bsc|sse)*.mgh $sub_dir/surf
rmdir $bsc_dir
