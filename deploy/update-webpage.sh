#!/usr/bin/env bash
# Pull, build, and publish the webpage on web.eecs.umich.edu.
# Requires a one-time run of deploy/setup-ocaml.sh.
#
#   REPO  checkout of this repository   (default: ~/webpage)
#   DEST  directory served as the site  (default: ~/public_html)
#
# Like the old update script, this copies over DEST without deleting
# anything already there that the build does not produce.
set -euo pipefail

REPO="${REPO:-$HOME/webpage}"
DEST="${DEST:-$HOME/public_html}"
SWITCH=webpage

export PATH="$HOME/bin:$PATH"
cd "$REPO"
git pull --ff-only

eval "$(opam env --switch="$SWITCH" --set-switch)"
opam install . --deps-only -y
dune build
rm -rf _site
dune exec -- site build --prefix /~comar --out _site --extra public --extra omar-cv.pdf

rsync -a --exclude='.*' _site/ "$DEST/"
