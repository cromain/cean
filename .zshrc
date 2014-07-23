# zshrc for CEAN
# This script is evaluated for every shell instance called by cean command

# Load shell configuration and CEAN functions
for cfg in ~/.cean*/cfg/*; source $cfg
for fun in ~/.cean*/functions; fpath=($fun $fpath)
for mod in ~/.cean*/functions/*/*; autoload -U ${mod#*functions/}

# Remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Give way to reload function (while developping cean scripts)
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

# Start CEAN shell
[ -f .ceanrc ] && source .ceanrc
cean/init
