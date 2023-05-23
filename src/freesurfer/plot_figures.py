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
from plot_figures_config import moddir, surfvalues, thresholds, surflabel, glmdirpatterns  # pylint: disable=no-name-in-module


surfaces = []
for glmdirpattern in glmdirpatterns:
    for surfvalue in surfvalues:
        pattern = f"{moddir}/?h.{glmdirpattern}/contrast_*/{surfvalue}.mgh"
        surfaces += glob(pattern)
if len(surfaces) == 0:
    print(f"surfaces not found: {pattern}")
    sys.exit(1)

# remove duplicates
surfaces = list({re.sub(rf"{moddir}/.?h\.", "", s) for s in surfaces})

# Choose colormap
cmap = pl.cm.coolwarm

# Get the colormap colors
my_cmap0 = cmap(np.arange(cmap.N))
my_cmap = np.copy(my_cmap0)
my_cmap[int(cmap.N / 2), -
        1] = 0.  # put transparancy on zero values (not for ef.mgh and gamma.mgh)

# Create new colormaps
my_cmap = ListedColormap(my_cmap)
my_cmap0 = ListedColormap(my_cmap0)


for surf in surfaces:
    data = {}
    fig = {}
    match = re.match("(.+)/contrast_(.+?)/(.+).mgh", surf)
    glm_dir, contrast, surfvalue = match.groups()  # surfvalue not used there
    # pylint: disable-next=cell-var-from-loop
    matching_keys = list(filter(lambda x: x in glm_dir, thresholds.keys()))
    if len(matching_keys) == 0:
        print(f"warning: no threshold defined for {glm_dir}")
        continue
    matching_contrasts = list(
        # pylint: disable-next=cell-var-from-loop
        filter(lambda x: x.lower() in contrast.lower(), thresholds[matching_keys[0]].keys()))
    if len(matching_contrasts) == 0:
        continue
    threshold = thresholds[matching_keys[0]][matching_contrasts[0]]
    odir = os.path.join(moddir, "images2", re.sub(".mgh", "", surf))
    if os.path.exists(odir):
        continue
    os.makedirs(odir, exist_ok=True)
    for hemi in ['lh', 'rh']:
        overlay_file = os.path.join(moddir, f"{hemi}.{surf}")
        data[hemi] = io.read_scalar_data(overlay_file.format(hemi))
        brain = Brain("fsaverage", hemi, surflabel,
                      background='white', size=[512, 512])
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap0 if surfvalue in [
                       'ef', 'gamma'] else my_cmap, min=-threshold, max=threshold, colorbar=True)
        fig[hemi + '.lat'] = mlab.screenshot()
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.lat.png"))
        brain.show_view('medial')
        fig[hemi + '.med'] = mlab.screenshot()
        mlab.savefig(os.path.join(odir, f"{hemi}.{surflabel}.med.png"))

    brain = Brain("fsaverage", "both", "pial",
                  background='white', size=[512, 512])
    for hemi in ['lh', 'rh']:
        brain.add_data(data[hemi], hemi=hemi, colormap=my_cmap, min=-threshold, max=threshold,
                       colorbar=True)

    brain.show_view('caudal')
    fig['caudal'] = mlab.screenshot()
    mlab.savefig(os.path.join(odir, f"combined.{surflabel}.pos.png"))

    # resize images
    os.makedirs(os.path.join(odir, "resized"), exist_ok=True)
    piclabels = ["lh.lat", "lh.med", "rh.lat", "rh.med", "caudal"]

    for i, pl in enumerate(piclabels, 1):
        img = Image.fromarray(fig[pl])
        img = img.crop((0, img.height*1/8, img.width, img.height*7/8))
        img.save(os.path.join(odir, "resized", f"{surflabel}-{i}.png"))

mlab.close()
