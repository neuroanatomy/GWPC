#!/usr/bin/env python3

"""script to plot figures of brain surface overlays"""
# pylint: disable=invalid-name

import os
import sys
from glob import glob
import re
from mayavi import mlab
from surfer import Brain, io
# import matplotlib.pyplot as plt
import matplotlib.pylab as pl
from matplotlib.colors import ListedColormap
import numpy as np
from plot_figures_config import moddir, image, threshold, surflabel, glmdirpattern


surfaces=glob(f"{moddir}/?h.{glmdirpattern}/{image}")
if len(surfaces) == 0:
    # echo "${surfaces[0]}": not found && exit 1
    print("surfaces not found")
    sys.exit(1)
# remove duplicates

surfaces = {re.sub(rf"{moddir}/.?h\.", "", s) for s in surfaces}


# Choose colormap
cmap = pl.cm.coolwarm

# Get the colormap colors
my_cmap = cmap(np.arange(cmap.N))
if os.path.basename(image) not in ['ef.mgh', 'gamma.mgh']:
    my_cmap[int(cmap.N / 2), -1] = 0. # put transparancy on zero values

# Create new colormap
my_cmap = ListedColormap(my_cmap)



for surf in surfaces:
    data = {}
    fig = {}
    print(surf)
    odir=os.path.join(moddir, "images2", re.sub(".mgh", "", surf))
    os.makedirs(odir, exist_ok=True)
    for hemi in ['lh', 'rh']:
        overlay_file = os.path.join(moddir, f"{hemi}.{surf}")
        data[hemi] = io.read_scalar_data(overlay_file.format(hemi))
        brain = Brain("fsaverage", hemi, surflabel, background='white')
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap, min=-threshold, max=threshold,
            colorbar=False)
        fig[hemi] = mlab.screenshot()
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.lat.png"))
        brain.show_view('medial')
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.med.png"))


    brain = Brain("fsaverage", "both", "pial", background='white')
    for hemi in ['lh', 'rh']:
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap, min=-threshold, max=threshold,
            colorbar=False)

    brain.show_view('caudal')
    mlab.savefig(os.path.join(odir, f"combined.{surflabel}.pos.png"))

    # make one image with colorbar
    brain = Brain("fsaverage", "lh", surflabel, background='white')
    brain.add_data(data["lh"], hemi="lh", colormap=my_cmap, min=-threshold, max=threshold,
        colorbar=True)
    mlab.savefig(os.path.join(odir, f"lh.{surflabel}.colorbar.png"))

mlab.close()
