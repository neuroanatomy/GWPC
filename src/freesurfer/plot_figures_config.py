"""config for plot_figures.py"""
# pylint: disable=invalid-name

import os
import subprocess

# config
script_dir = os.path.dirname(os.path.abspath(__file__))
project_dir = subprocess.run(["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE,
    cwd=script_dir, check=True).stdout.decode().strip()

fsdir=os.path.join(project_dir, "data/derived/fs-6.0.0")

# export SUBJECTS_DIR=$fsdir

moddir=os.path.join(fsdir, "glm_nonNYU")

# image="contrast_age/cache.th20.abs.sig.gamma.mgh"
# image="contrast_age/cw_ef.mgh"
# pour contraste
# threshold=0.3
# pour thickness
# threshold=0.02

# image="contrast_ASD/cache.th20.abs.sig.gamma.mgh"
# image="contrast_ASD/cw_ef.mgh"
image="contrast_ASD/ef.mgh"
# pour contraste
threshold=1
# pour thickness
# threshold=0.06
# threshold=0.12
# for bsc
# threshold=0.002

# image="contrast_Female/cache.th20.abs.sig.gamma.mgh"
# image="contrast_Female/cw_ef.mgh"
# pour contraste
# threshold=1.5
# pour thickness
# threshold=0.1

# image="contrast_FIQ/cache.th20.abs.sig.gamma.mgh"
# image="contrast_FIQ/cw_ef.mgh"
# pour contraste
# threshold=0.04
# pour thickness ?
# threshold=0.04

# image="contrast_motion/cw_ef.mgh"
# for contrast
# threshold=9
# for thickness
# threshold=0.7

# image="contrast_VINELAND_ABC_STANDARD/cw_ef.mgh"
# for contrast
# threshold=0.08

surflabel="pial"
glmdirpattern="*w-g*.fwhm10*"
