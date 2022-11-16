#!/bin/bash

LSP_ROOT_TOOLS=~/opt/lsp-tools

mkdir -p $LSP_ROOT_TOOLS/bin

# markdown
cd ${LSP_ROOT_TOOLS} || exit
curl -L https://github.com/artempyanykh/marksman/releases/download/2022-10-30/marksman-linux -o bin/marksman && chmod +x bin/marksman

# docker
cd ${LSP_ROOT_TOOLS} || exit
npm install --prefix ${LSP_ROOT_TOOLS} dockerfile-language-server-nodejs
cd bin || exit
ln -sf ../node_modules/dockerfile-language-server-nodejs/bin/docker-langserver .

# cmake
cd ${LSP_ROOT_TOOLS} || exit
pip install --prefix=${LSP_ROOT_TOOLS} cmake-language-server
cd bin || exit
ln -sf ../local/bin/cmake-language-server .

#bash
cd ${LSP_ROOT_TOOLS} || exit
npm install --prefix ${LSP_ROOT_TOOLS} bash-language-server
cd bin || exit
ln -sf ../node_modules/bash-language-server/bin/main.js bash-language-server

# bash too
cd ${LSP_ROOT_TOOLS} || exit
mkdir -p ${LSP_ROOT_TOOLS}/shellcheck
SHELLCHECK_RELEASE=v0.8.0
curl -L https://github.com/koalaman/shellcheck/releases/download/${SHELLCHECK_RELEASE}/shellcheck-${SHELLCHECK_RELEASE}.linux.x86_64.tar.xz | tar x -J -C ${LSP_ROOT_TOOLS}/shellcheck --strip-components=1
cd bin || exit
ln -sf ../shellcheck/shellcheck .
