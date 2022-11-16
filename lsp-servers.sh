#!/bin/bash

LSP_ROOT_TOOLS=~/opt/lsp-tools

mkdir -p $LSP_ROOT_TOOLS/bin

# markdown
if [ ! -x "$(readlink -f ${LSP_ROOT_TOOLS}/bin/marksman)" ]; then
    cd ${LSP_ROOT_TOOLS} || exit
    curl -L https://github.com/artempyanykh/marksman/releases/download/2022-10-30/marksman-linux -o bin/marksman && chmod +x bin/marksman;
fi

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
if [ ! -x "$(readlink -f ${LSP_ROOT_TOOLS}/bin/shellcheck)" ]; then
    cd ${LSP_ROOT_TOOLS} || exit
    mkdir -p ${LSP_ROOT_TOOLS}/shellcheck
    SHELLCHECK_RELEASE=v0.8.0
    curl -L https://github.com/koalaman/shellcheck/releases/download/${SHELLCHECK_RELEASE}/shellcheck-${SHELLCHECK_RELEASE}.linux.x86_64.tar.xz | tar x -J -C ${LSP_ROOT_TOOLS}/shellcheck --strip-components=1
    cd bin || exit
    ln -sf ../shellcheck/shellcheck .;
fi

#clangd
if [ ! -x "$(readlink -f ${LSP_ROOT_TOOLS}/bin/clangd-indexer)" ]; then
    cd ${LSP_ROOT_TOOLS} || exit
    mkdir -p ${LSP_ROOT_TOOLS}/clangd
    CLANGD_RELEASE=15.0.3
    curl -L https://github.com/clangd/clangd/releases/download/${CLANGD_RELEASE}/clangd_indexing_tools-linux-${CLANGD_RELEASE}.zip | bsdtar x -C ${LSP_ROOT_TOOLS}/clangd --strip-components=1
    cd bin || exit
    for exe in ../clangd/bin/clangd-* ; do
        chmod +x "${exe}"
        ln -sf "${exe}" .;
    done;
fi
