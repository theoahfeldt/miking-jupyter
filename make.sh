#!/bin/sh

# Forces the script to exit on error
set -e

build_kernel() {
    dune build
}

install_kernel() {
    bin_path=$HOME/.local/bin/
    lib_path=$HOME/.local/lib/mcore/kernel/
    mkdir -p $bin_path $lib_path
    cp -f _build/default/src/kernel.exe $bin_path/mcore_kernel; chmod +x $bin_path/mcore_kernel
    cp -f src/mpl_backend.py $lib_path
    jupyter-kernelspec install mcore_kernel --user
    jupyter-nbextension install src/mcore-syntax --user --log-level=WARN
    jupyter-nbextension enable mcore-syntax/main --user
}

case $1 in
    install)
        build_kernel
        install_kernel
        ;;
    clean)
        rm -rf _build
        ;;
    all | *)
        build_kernel
        ;;
esac
