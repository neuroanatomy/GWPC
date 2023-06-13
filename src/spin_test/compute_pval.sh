#!/bin/bash
set -e

script_dir=$(cd "$(dirname "$0")"; pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
output_dir=$derived_dir/spin_test/output

spin_test=spin_test

shopt -s extglob

lhfiles=()
rhfiles=()
for lhfile in "$output_dir"/lh.!(*fsaverage5).mgh; do
	rhfile=$(dirname "$lhfile")/rh.${lhfile##*/lh.}
	outfile=$(dirname "$lhfile")/$(basename "${lhfile##*/lh.}" .mgh).txt
	if [[ -f $rhfile ]] && [[ ! -f $outfile ]]; then
		lhfiles+=("$lhfile")
		rhfiles+=("$rhfile")
	fi
done

# lhfiles=("${lhfiles[@]:0:3}")
numfiles=${#lhfiles[@]}
echo "$numfiles" files

if command -v sbatch > /dev/null; then
	sbatch -D "$script_dir"/log \
		-a 1-"$numfiles" \
		<<EOF
#!/bin/sh
cd $script_dir
j=\$SLURM_ARRAY_TASK_ID
lhfile=\${lhfiles[\$j-1]}
rhfile=\${rhfiles[\$j-1]}
echo $lhfile
octave --eval \"origleft1='\$lhfile';origright1='\$rhfile';$spin_test\"
# matlab -r \"origleft1='\$lhfile';origright1='\$rhfile';$spin_test;exit\"
EOF
else
	for ((i=0; i<numfiles; i++)); do
		lhfile=${lhfiles[$i]}
		rhfile=${rhfiles[$i]}
		# matlab -nodesktop -r "origleft1='$lhfile';origright1='$rhfile';$spin_test;exit"
		octave --eval "origleft1='$lhfile';origright1='$rhfile';$spin_test"
	done
fi