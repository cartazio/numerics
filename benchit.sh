#! /bin/bash


thedate=$(date "+%Y-%m-%d-%Hh-%Mm-%Ss")


./dist/build/benchmarks/benchmarks  -o "BenchmarkLog/${thedate}-bench.html"
