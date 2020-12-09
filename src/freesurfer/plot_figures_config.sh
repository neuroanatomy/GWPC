# shellcheck shell=bash

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

fsdir=$project_dir/data/derived/fs-6.0.0

export SUBJECTS_DIR=$fsdir

moddir=$fsdir/glm_mod2

# image=contrast_age/cache.th20.abs.sig.gamma.mgh
# image=contrast_age/cw_ef.mgh
# pour contraste
# threshold=1e-6,0.3
# pour thickness
# threshold=1e-6,0.02

# image=contrast_ASD/cache.th20.abs.sig.gamma.mgh
# image=contrast_ASD/cw_ef.mgh
image=contrast_ASD/ef.mgh
# pour contraste
threshold=1e-6,1
# pour thickness
# threshold=1e-6,0.06
# threshold=1e-6,0.12
# for bsc
# threshold=1e-6,0.002

# image=contrast_Female/cache.th20.abs.sig.gamma.mgh
# image=contrast_Female/cw_ef.mgh
# pour contraste
# threshold=1e-6,1.5
# pour thickness
# threshold=1e-6,0.1

#image=contrast_FIQ/cache.th20.abs.sig.gamma.mgh
#image=contrast_FIQ/cw_ef.mgh
# pour contraste
#threshold=0.01,0.04
# pour thickness ?
# threshold=0.01,0.04

# image=contrast_motion/cw_ef.mgh
# for contrast
# threshold=1e-6,9
# for thickness
# threshold=1e-6,0.7

# image=contrast_VINELAND_ABC_STANDARD/cw_ef.mgh
# for contrast
# threshold=1e-6,0.08

surflabel=pial
glmdirpattern="*w-g*.fwhm10*"
# glmdirpattern="*thickness.*fwhm10*"
# glmdirpattern="*.bsc.*"

if command -v xvfb-run > /dev/null; then
	freeview() {
		xvfb-run --auto-servernum -s "-screen 0 1920x1080x24" $(which freeview) $@
	}
fi

export moddir
export image
export threshold
export surflabel
export glmdirpattern
export -f freeview
