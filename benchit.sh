#! /bin/bash


date "+%Y-%m-%d-%H:%M:%S" > NOW

./dist/build/benchmarks/benchmarks  -o "BenchmarkLog/$`cat NOW`-bench.html"
