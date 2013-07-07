module State where

data State = State {
  stateLine :: Bool,
  wasPressed :: Bool,
  shouldExit :: Bool
}

initialState :: State
initialState = State {
  stateLine  = False,
  wasPressed = False,
  shouldExit = False
}
