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
 - NOTE: Since server is built and run separately, make sure to install the dependencies mentioned in the root README.

## Tests

 - `Basic Test:` Verifies basic back and forth forwarding of messages between clients
 - `Leave Test:` If a client leaves during a game, verifies that other client receives end of game message
 - `Turn Test:` Verifies that if a client sends a move out of turn, that move is not propogated. While client side code also checks this, having the server perform this check adds extra fault tolerance 
