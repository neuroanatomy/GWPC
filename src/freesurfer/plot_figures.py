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
from PIL import Image
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
        brain = Brain("fsaverage", hemi, surflabel, background='white', size=[512, 512])
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap, min=-threshold, max=threshold,
            colorbar=True)
        fig[hemi + '.lat'] = mlab.screenshot()
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.lat.png"))
        brain.show_view('medial')
        fig[hemi + '.med'] = mlab.screenshot()
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.med.png"))

    brain = Brain("fsaverage", "both", "pial", background='white', size=[512, 512])
    for hemi in ['lh', 'rh']:
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap, min=-threshold, max=threshold,
            colorbar=True)

    brain.show_view('caudal')
    fig['caudal'] = mlab.screenshot()
    mlab.savefig(os.path.join(odir, f"combined.{surflabel}.pos.png"))

    # resize images
    os.makedirs(os.path.join(odir, "resized"), exist_ok=True)
    piclabels=["lh.lat", "lh.med", "rh.lat", "rh.med", "caudal"]

    for i, pl in enumerate(piclabels, 1):
        img = Image.fromarray(fig[pl])
        img = img.crop((0, img.height*1/8, img.width, img.height*7/8))
        img.save(os.path.join(odir, "resized", f"{surflabel}-{i}.png"))

# mlab.close()
