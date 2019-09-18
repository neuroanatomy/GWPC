# shellcheck shell=bash

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

fsdir=$project_dir/data/derived/fs-6.0.0

export SUBJECTS_DIR=$fsdir

moddir=$fsdir/glm_NYU

# image=contrast_age/cache.th20.abs.sig.gamma.mgh
# image=contrast_age/cw_ef.mgh
# pour contraste
# threshold=1e-6,0.3
# pour thickness
# threshold=1e-6,0.02

# image=contrast_ASD/cache.th20.abs.sig.gamma.mgh
image=contrast_ASD/cw_ef.mgh
# pour contraste
threshold=1e-6,2
# pour thickness
# threshold=1e-6,0.06

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

surflabel=pial
glmdirpattern="*w-g*.fwhm10*"
# glmdirpattern="*thickness.*fwhm10*"

export moddir
export image
export threshold
export surflabel
export glmdirpattern
