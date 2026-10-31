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
# ~/.config and ~/.cache are root-owned on web.eecs, and bushel's contact
# store (sortal) creates XDG directories when it loads.
export XDG_CONFIG_HOME="$HOME/.local/config" XDG_CACHE_HOME="$HOME/.local/cache"
cd "$REPO"
git pull --ff-only

eval "$(opam env --switch="$SWITCH" --set-switch)"
opam install . --deps-only -y --assume-depexts
dune build
rm -rf _site
dune exec -- site build --prefix /~comar --out _site --extra public --extra omar-cv.pdf

rsync -a --exclude='.*' _site/ "$DEST/"
