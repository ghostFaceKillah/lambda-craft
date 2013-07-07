all: lambda-craft

lambda-craft: Main.hs
	ghc Main.hs -o lambda-craft
