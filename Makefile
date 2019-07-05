.Phony: build run buildAndRun

build:
	ghc --make ./src/*
run:
	cd ./src/
	ghci -i ./src/Hijara.hs
buildAndRun:
	ll