FROM nixos/nix

COPY scripts ./scripts

RUN apk update \
    && /bin/ash ./scripts/common-alpine.sh

RUN nix-env --install --attr cachix -f https://cachix.org/api/v1/install \
    && cachix use iohk \
    && cachix use ghcide-nix