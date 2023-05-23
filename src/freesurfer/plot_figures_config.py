"""config for plot_figures.py"""
# pylint: disable=invalid-name

import os
import sys
import subprocess

# config
script_dir = os.path.dirname(os.path.abspath(__file__))
project_dir = subprocess.run(["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE,
                             cwd=script_dir, check=True).stdout.decode().strip()

fsdir = os.path.join(project_dir, "data/derived/fs-6.0.0")

if len(sys.argv) == 2:
    moddir = sys.argv[1]
else:
    moddir = os.path.join(fsdir, "glm_NYU")

surflabel = "pial"
glmdirpatterns = ["*w-g*.fwhm10*", "*thickness.fwhm10*", "*fwhm2.bsc.fwhm10*"]
surfvalues = ["cw_ef", "ef", "cache.th20.abs.sig.gamma"]

thresholds = {
    "w-g": {
        "age": 0.3,
        "ASD": 1,
        "Female": 1.5,
        "FIQ": 0.04,
        "motion": 9,
        "VINELAND_ABC_STANDARD": 0.08
    },
    "thickness": {
        "age": 0.02,
        "ASD": 0.06,
        "Female": 0.1,
        "FIQ": 0.01,  # not checked
        "motion": 0.7
    },
    "bsc": {
        "age": 0.0006,
        "ASD": 0.002,
        "Female": 0.003,
        "FIQ": 0.00008,
        "motion": 0.02
    },
}
