#!/bin/bash

function test {
    paz=$1
    input=${paz%.paz}.in
    output=${paz%.paz}.out
    if [ ! -f "$input" ]; then
        input="/dev/null"
    fi
    echo $paz
    ../Paz $paz > test.taz
    ../taz_emulator/taz test.taz < $input > test.out
}

for paz in vis/*.paz; do
    test $paz
done

for paz in contrib/*.paz; do
    test $paz
done

rm test.taz test.out
