.Phony: build run buildAndRun

build:
	ghc --make ./src/*
run: src/
	ghc -c -dynamic ./src/Hijara.hs
buildAndRun:
	ll