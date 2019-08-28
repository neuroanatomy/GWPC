#!/usr/bin/env python3
"""
ntraut 2019
extract mean movement from mcflirt output
"""

# pylint: disable=C0103
import os
from glob import glob
import subprocess
import statistics
import pandas as pd
# import shutil

cinq = "/Volumes/cinq"

# Abide 1
# mcdir = os.path.join(cinq, "rto/data/abide/mcflirt")
# ofile = os.path.join(cinq, "rto/data/abide/motion.tsv")

# Abide 2
mcdir = os.path.join(cinq, "rto/data/abide2all/data/mcflirt")
ofile = os.path.join(cinq, "rto/data/abide2all/data/motion.tsv")

rms_table = pd.DataFrame(columns=["rms"])
if not os.path.isdir(mcdir):
    raise NotADirectoryError(mcdir)
subject_dirs = sorted(glob(os.path.join(mcdir, "*")))
print("Reading rms files...")
for subject_dir in subject_dirs:
    subject_id = os.path.basename(subject_dir)
    session_dirs = sorted(glob(os.path.join(subject_dir, "session_*")))
    for session_dir in session_dirs:
        session_id = os.path.basename(session_dir)
        session_index = int(session_id[8:])
        run_dirs = sorted(glob(os.path.join(session_dir, "run_*")))
        rms = [float(open(os.path.join(run_dir, "rest_mcf_rel_mean.rms")).read()) for run_dir in run_dirs]
        row_name = subject_id + "_ses-" + str(session_index)
        rms_table.loc[row_name] = statistics.mean(rms)

print("Saving summary to {}".format(ofile))
rms_table.to_csv(ofile, sep="\t", header=False)
