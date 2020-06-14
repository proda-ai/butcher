
set -ex

ROOTSDIR="nix/gcroots"

function create-root {
  local ATTRPATH=$1
  nix-build -o "${ROOTSDIR}/${ATTRPATH}-shell" nix/all.nix -A "\"$ATTRPATH\".shell"
  nix-build -o "${ROOTSDIR}/${ATTRPATH}-test" nix/all.nix -A "\"$ATTRPATH\".butcher.components.tests.tests"
  nix-build -o "${ROOTSDIR}/${ATTRPATH}-plan" nix/all.nix -A "\"$ATTRPATH\".butcher-plan"
  nix-build -o "${ROOTSDIR}/${ATTRPATH}-nix" nix/all.nix -A "\"$ATTRPATH\".butcher-nix" || true
}

mkdir -p "$ROOTSDIR"

nix-build -o "${ROOTSDIR}/haskell-nix-roots" nix/all.nix -A "roots"

create-root "stackage-8.4"
create-root "stackage-8.6"
create-root "stackage-8.8"

create-root "hackage-8.4"
create-root "hackage-8.6"
create-root "hackage-8.8"
create-root "hackage-8.10"
