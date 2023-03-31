#!/bin/bash
set -e

# first argument is the subject directory to be resampled

if [[ -z $FREESURFER_HOME ]]; then
	export FREESURFER_HOME=$HOME/bin/freesurfer
	source "$FREESURFER_HOME/SetUpFreeSurfer.sh"
fi

sub_dir=$1
sub=$(basename "$sub_dir")
SUBJECTS_DIR=$(cd "$sub_dir"/.. && pwd)
export SUBJECTS_DIR

echo "Compute white/grey contrast"
for frac in .00 .10 .20 .30 .40 .50 .60; do
	exec 5>&1
	tmpdir=$(pctsurfcon --s "$sub" --gm-proj-frac "$frac" --b "w-g.pct$frac" --nocleanup | tee >(cat >&5) | perl -lne 'print $1 if /^mri_vol2surf .*\s(.*tmp.*)\/lh.wm.mgh/')
	mv -f "$tmpdir/lh.gm.mgh" "${tmpdir%/*}/lh.gm$frac.mgh"
	mv -f "$tmpdir/rh.gm.mgh" "${tmpdir%/*}/rh.gm$frac.mgh"
	mv -f "$tmpdir/lh.wm.mgh" "${tmpdir%/*}/lh.wm_1mm.mgh"
	mv -f "$tmpdir/rh.wm.mgh" "${tmpdir%/*}/rh.wm_1mm.mgh"
	rmdir "$tmpdir"
done

echo "Resampling white/grey contrast"
for frac in .00 .10 .20 .30 .40 .50 .60; do
	recon-all -s "$sub" -qcache -measure "w-g.pct$frac.mgh"
	recon-all -s "$sub" -qcache -measure "gm$frac.mgh"
done
recon-all -s "$sub" -qcache -measure wm_1mm.mgh

echo "Resampling cortical thickness"
recon-all -s "$sub" -qcache -measure thickness
