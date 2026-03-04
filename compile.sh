#!/bin/sh

lib=$(echo $LIBRARY_PATH | cut -d':' -f1)
header=$(echo $C_INCLUDE_PATH | cut -d':' -f1)/osqp

lib_dir=$(dirname $lib)
gcc -I$header -L$lib_dir -losqp test.c

