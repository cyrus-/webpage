#!/usr/bin/env bash
# One-time setup on web.eecs.umich.edu: a user-local opam + OCaml 5.2
# switch for building the webpage. Installs only under ~/bin and ~/.opam,
# and is safe to re-run.
set -euo pipefail

OPAM_VERSION=2.3.0
SWITCH=webpage

mkdir -p "$HOME/bin"
export PATH="$HOME/bin:$PATH"

if ! command -v opam >/dev/null; then
  curl -fsSL -o "$HOME/bin/opam" \
    "https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-x86_64-linux"
  chmod +x "$HOME/bin/opam"
fi

# bwrap can't create directories in the networked home dir on web.eecs,
# so opam's build sandbox doesn't work there.
[ -d "$HOME/.opam" ] || opam init -y --bare --no-setup --disable-sandboxing

if ! opam switch list --short | grep -qx "$SWITCH"; then
  opam switch create "$SWITCH" ocaml-base-compiler.5.2.0 -y
fi

eval "$(opam env --switch="$SWITCH" --set-switch)"
opam install -y dune

echo "Done: $(ocaml -version)"
echo "If ~/bin is not on your PATH, add this to ~/.bashrc:"
echo '  export PATH="$HOME/bin:$PATH"'
