#! /bin/bash

mkdir -p output

for f in tests/*.md; do
  echo $f
  g=${f%%.md}
  echo $g
  h=${g##tests/}
  echo $h
  pandoc -f markdown -t html -s -o output/${h}.html -F pandoc-narrative-charts $f
done
