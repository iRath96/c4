#!/usr/bin/env bash
set -eu

mkdir -p llvm
cd llvm

CUR=`pwd`

if [ ! -e  "${CUR}/llvm" ]; then
        wget http://releases.llvm.org/5.0.0/llvm-5.0.0.src.tar.xz
        tar xf llvm-5.0.0.src.tar.xz
        rm llvm-5.0.0.src.tar.xz
        mv llvm-5.0.0.src llvm

        # use this to also build clang
        #cd llvm/tools
        #wget http://releases.llvm.org/5.0.0/cfe-5.0.0.src.tar.xz
        #tar xf cfe-5.0.0.src.tar.xz
        #rm cfe-5.0.0.src.tar.xz
        #mv cfe-5.0.0.src clang

        cd ${CUR}
fi

mkdir -p build
cd build
cmake ../llvm \
        -DCMAKE_BUILD_TYPE:STRING=Debug \
        -DCMAKE_INSTALL_PREFIX:PATH="${CUR}/install" \
        -DLLVM_ENABLE_ASSERTIONS:BOOL=ON \
        -DLLVM_ENABLE_EH:BOOL=ON \
        -DLLVM_ENABLE_EXPENSIVE_CHECKS:BOOL=ON \
        -DLLVM_ENABLE_RTTI:BOOL=ON \
        -DLLVM_INCLUDE_TESTS:BOOL=OFF \
        -DLLVM_TARGETS_TO_BUILD="X86" 

make install
