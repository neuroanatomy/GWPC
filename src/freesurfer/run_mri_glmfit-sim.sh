#!/bin/bash
set -e

mri_glmfit-sim "$@"

for ((i=1;i<=$#;i++)); do
	if [[ ${!i} == "--glmdir" ]]; then
		j=$((i+1))
		glmdir=${!j}
		break
	fi
done

for mask in "$glmdir"/*/cache.th*.sig.cluster.mgh; do
	contrastdir=$(dirname "$mask")
	mri_mask "$contrastdir"/gamma.mgh "$mask" "${mask%.cluster.mgh}".gamma.mgh
	mri_segstats --seg "${mask%.cluster.mgh}".ocn.mgh --i "$contrastdir"/gamma.mgh --exclude 0 \
		--sum "${mask%.cluster.mgh}".gamma.dat
done
