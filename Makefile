all: lambda-craft

lambda-craft: Main.hs GameState.hs Events.hs Cube.hs
	ghc Main.hs -o lambda-craft

clean:
	rm *.o *.hi lambda-craft
