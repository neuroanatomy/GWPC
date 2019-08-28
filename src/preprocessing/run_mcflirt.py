#!/usr/bin/env python3
"""
ntraut 2019
run mcflirt on resting state fMRIs
"""

# pylint: disable=C0103
import os
from glob import glob
import subprocess
import re
# import shutil

cinq = "/Volumes/cinq"

# Abide 1
# idir = os.path.join(cinq, "rto/data/abide/_orig/raw_nii")
# odir = os.path.join(cinq, "rto/data/abide/mcflirt")
# siteTemplate = "*"
# sessionTemplate = "ses*"
# functionalTemplate = "rest_1/rest.nii.gz"
# anatomicalTemplate = "anat_1/mprage.nii.gz"

# Abide 2
idir = os.path.join(cinq, "rto/data/abide2all/data/RawData")
odir = os.path.join(cinq, "rto/data/abide2all/data/mcflirt")
siteTemplate = "*"
sessionTemplate = "ses*"
functionalTemplate = "func/*_task-rest_*bold.nii.gz"
anatomicalTemplate = "anat/*_T1w.nii.gz"

if not os.path.isdir(idir):
    raise NotADirectoryError(idir)
site_dirs = sorted(glob(os.path.join(idir, siteTemplate)))
for site_dir in site_dirs:
    site_id = os.path.basename(site_dir)
    print(site_id)
    subject_dirs = sorted(glob(os.path.join(site_dir, "*")))
    for subject_dir in subject_dirs:
        subject_id = os.path.basename(subject_dir)
        print(subject_id)
        subject_path = os.path.join(odir, subject_id)
        session_dirs = sorted(glob(os.path.join(subject_dir, sessionTemplate)))
        for session_dir in session_dirs:
            session_index = int(re.search(r'\d+$', session_dir).group())
            if session_index == 1:
                continue
            # Give a priority to images without acquisition label
            functional_MRIs = sorted(glob(os.path.join(session_dir, functionalTemplate)),
                                     key=lambda x: x.replace("_run", "1run"))
            if not functional_MRIs:
                continue
            for run_index, functional_MRI in enumerate(functional_MRIs):
                run_path = os.path.join(subject_path, "session_" +
                                        str(session_index), "run_" + str(run_index + 1))
                os.makedirs(run_path, exist_ok=True)
                ofile = os.path.join(run_path, "rest.nii.gz")
                subprocess.run(["fslmaths", functional_MRI, ofile, "-odt", "float"], check=True)
                # os.link(functional_MRI, ofile)

                subprocess.run(["mcflirt", "-in", ofile, "-rmsrel"], check=True)
