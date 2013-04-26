#! /bin/bash


thedate=$(date "+%Y-%m-%d-%Hh-%Mm-%Ss")
thecommit=$(git log --format=%H HEAD^1..HEAD)

if git status | grep "Changes"; then 
    echo "the are uncommited changes in the repo. We can't benchmark that.";
    exit 1; 

fi

./dist/build/benchmarks/benchmarks  -o "BenchmarkLog/${thedate}-bench-commit=${thecommit}.html"
