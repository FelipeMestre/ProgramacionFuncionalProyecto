.Phony: build run buildAndRun

build:
	ghc --make ./src/*
run:
	ghci -i ./src/Hijara.hs
buildAndRun:
	ll