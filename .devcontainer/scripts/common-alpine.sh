#!/bin/ash

set -e

if [ "$(id -u)" -ne 0 ]; then
    echo 'Script must be run a root. Use sudo or set "USER root" before running the script.'
    exit 1
fi

# Install git, bash, dependencies
apk add --no-cache \
    git \
    openssh-client \
    less \
    bash \
    libgcc \
    libstdc++ \
    curl \
    wget \
    unzip \
    nano \
    jq \
    gnupg \
    procps \
    coreutils \
    ca-certificates \
    krb5-libs \
    libintl \
    libssl1.1 \
    lttng-ust \
    tzdata \
    make \
    userspace-rcu \
    zlib \
    shadow \
    zsh

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "export PATH=\$PATH:\$HOME/.local/bin" >> /root/.zshrc
