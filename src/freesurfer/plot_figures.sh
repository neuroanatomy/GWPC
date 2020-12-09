#!/usr/bin/env bash
set -e

if [[ $# -ne 1 ]]; then
    echo "Usage:"
    echo "$0 config_file.sh"
    exit 1
fi

config=$1
# shellcheck source=plot_figures_config.sh
source "$config"

# shellcheck disable=SC2206
surfaces=("$moddir"/?h.$glmdirpattern/"$image")
[[ ! -f "${surfaces[0]}" ]] && echo "${surfaces[0]}": not found && exit 1
surfaces=("${surfaces[@]#$moddir/?h.}")
# remove duplicates
readarray -t surfaces < <( printf '%q\n' "${surfaces[@]}" | sort -u )

# freeview () { xvfb-run -e /dev/stdout --auto-servernum -s "-screen 0 1920x1080x24" freeview -verbose; }
type freeview

for surf in "${surfaces[@]}"; do
	echo "$surf"
	odir=$moddir/images/${surf%.mgh}
	mkdir -p "$odir"
	#cd $odir
	# associative array, requires bash 4
	declare -A cmd=()
	for hemi in lh rh; do
		cmd[$hemi]=$SUBJECTS_DIR/fsaverage/surf/$hemi.$surflabel:overlay=$moddir/$hemi.$surf:overlay_threshold=$threshold:edgethickness=0:color=grey
	done
	# left hemisphere
	echo "-ss $odir/lh.$surflabel.lat.png -noquit" > "$script_dir"/cmd.txt
	echo "-cam azimuth 180 -ss $odir/lh.$surflabel.med.png -quit" >> "$script_dir"/cmd.txt
	freeview -f "${cmd[lh]}" -layout 1 -viewport 3d -colorscale -cmd "$script_dir"/cmd.txt
	# right hemisphere
	echo "-ss $odir/rh.$surflabel.med.png -noquit" > "$script_dir"/cmd.txt
	echo "-cam azimuth 180 -ss $odir/rh.$surflabel.lat.png -quit" >> "$script_dir"/cmd.txt
	freeview -f "${cmd[rh]}" -layout 1 -viewport 3d -colorscale -cmd "$script_dir"/cmd.txt
	# both hemisphere
	echo "-cam azimuth 90 -ss $odir/combined.$surflabel.pos.png -quit" > "$script_dir"/cmd.txt
	freeview -f "${cmd[lh]}" -f "${cmd[rh]}" -layout 1 -viewport 3d -ras 0 -18 15 -cc -colorscale -cmd "$script_dir"/cmd.txt

	# resize images
	echo "resizing images..."
	mkdir -p "$odir"/resized
	piclabels=(lh."$surflabel".lat lh."$surflabel".med rh."$surflabel".lat rh."$surflabel".med \
		combined."$surflabel".pos)
	max_width=0
	max_height=0
	for (( i=0; i<${#piclabels[@]}; i++ )) ; do
		img="$odir"/"${piclabels[$i]}".png
		timg="$odir"/resized/$surflabel-$((i+1)).trimmed.png
		# remove colorscale (blur parameter may need to be adjusted to find the minimum value that removes the colorscale)
		convert "$img" -crop "$(convert "$img" -virtual-pixel edge -blur 0x120 -fuzz 15% \
            -trim -format '%wx%h%O' info:)" -trim  +repage "$timg"
		width=$(identify -ping -format '%w' "$timg")
		[[ $width -gt $max_width ]] && max_width=$width
		height=$(identify -ping -format '%h' "$timg")
		[[ $height -gt $max_height ]] && max_height=$height
	done
	if [[ $max_width -gt $((max_height * 4/3)) ]]; then
		wi=$max_width
		hi=$((max_width * 3/4))
	else
		wi=$((max_height * 4/3))
		hi=$max_height
	fi
	wo=512
	ho=384
	for (( i=0; i<${#piclabels[@]}; i++ )) ; do
		timg="$odir"/resized/$surflabel-$((i+1)).trimmed.png
		rimg="$odir"/resized/$surflabel-$((i+1)).png
		convert "$timg" -gravity center -extent ${wi}x"${hi}" -resize ${wo}x${ho} "$rimg"
	done
	echo "removing intermediate images"
	rm "$odir"/resized/*.trimmed.png
done
