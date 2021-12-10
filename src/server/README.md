# Server Code

 - Sets up game sessions between the two players
 - Informs the players of their colors
 - Communicates moves between players
 - Fault Tolerance: Invalid Turns, Client Leaving Game
 - Invalid Turns: Server keeps track of player turn and does not forward moves sent out of turn
 - Leaving Game: If one of the clients leaves the game in the middle, the other client is notified

## Instructions

 - `Build:` stack ghc Main.hs
 - `Run:` ./Main
 - `Test:` stack ghc Test.hs && ./Test
 - Run the server first before running the clients
