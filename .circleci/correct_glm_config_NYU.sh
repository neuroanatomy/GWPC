# shellcheck shell=bash

function submit {
	shift
	echo "$@"
	"$@"
}

shopt -s extglob

script_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$script_dir" && git rev-parse --show-toplevel)

derived_dir=$project_dir/data/derived
fsdir=$derived_dir/fs-6.0.0
#moddirpattern="$fsdir/glm_!(m*|*[bsk])"
#moddirpattern=$fsdir/glm_*_thickness
moddirpattern=$fsdir/glm_NYU
glmpattern="w-g.pct.30.fwhm10.*"
#glmpattern=thickness.fwhm10.*
#glmpattern="*"

export moddirpattern
export glmpattern
