#!/bin/bash

rm stats2
for j in {1..10}; do 
	echo "run ${j}..."; 
	for i in {1..4}; do 
		./hgraph -t bmark1haskell.N4000E80000adj bmark1haskell.N4000E80000adj.atr sampledata.p +RTS -N`echo "$i"` > graphs 2>> stats2 && tail -n 2 graphs >> stats2;
		echo -e "\n\n" >> stats2; 
	done; 
	echo -e "run ${j}:\n" >> stats2; 
done
