all: lambda-craft

lambda-craft: Main.hs GameState.hs Events.hs
	ghc Main.hs -o lambda-craft

clean:
	rm *.o *.hi lambda-craft
