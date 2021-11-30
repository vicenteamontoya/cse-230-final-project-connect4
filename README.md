# CSE 230 Final Project: Connect 4

## Team Members
 - Shubham Bhargava
 - Teja Dhondu
 - Mihir Kekkar
 - Vicente Montoya

## Overview
A Haskell implementation of the popular Connect 4 board game with support for up to 2 players connected online.

## Game
The Connect Four is a two-player board game played on a grid with seven columns and six rows. First, each player picks a color. On each player's turn, the player chooses a column to drop a disc of their color. The disc will drop to the lowest available row within the column. The objective of the game is to be the first player to form a horizontal, vertical, or diagonal line of four of one's own discs.

## Tasks/Goals
- Design a TUI for the game, consisting of a board, and colored pieces.
- Implement the game engine, containing the state of the board, whose turn is it, etc.
- Connecting the game engine to the TUI.
- Attach to our application websockets and network support.
- Implement a menu interface to start a game session, end the game session, scoreboard, etc.

## Packages
 - Brick: Used for creating a terminal user interface (TUI) so that game feels interactive to the user.
 - WebSocket: Used for creating sockets and a network connection between the two players.
 - Graphics.Vty: Used for display graphics in the terminal and handling events.
 - QuickCheck: Used for creating unit tests.

## Possible Extensions
- Add more game modes that involve changing board size, length of connections and maybe even a 3D version.
- Make the game extensible to more than 2 players.
- Make any players not on the same network be able to play together.
- Add animations for dropping tiles, winning, and losing.
- Make tile designs and colors customizable.
- Add music and sound effects.
- Add a single player mode.
- Add local multiplayer (two players playing on the same computer).

## Architecture
- The client program running the game consists of a Model, View, Controller architecture.
    - Model.hs: Defines the state of the game: the board, player turn, current screen and selected column.
        - The board is defined as a Map from Position to Disc.
        - The current screen defines whether we are in the main menu, waiting for network connection, playing the game, or in game over.
    - Control.hs: Defines the functions to manipulate the state of the game.
        - Updates the state of the game based on game ticks, network events, keyboard events and mouse events.
        - Defines how to move during the game, and how to change screens and navigate menu using keyboard.
    - View.hs: Defines how the state of the game is rendered in the terminal. This will compile the views of each of the game states (current screen):
        - Menu.hs: Renders the main menu screen.
        - Game.hs: Renders the game screen (board, controls).
        - GameOver.hs: Renders the game over screen.
        - Loading.hs: Render the loading screen while waiting for connection to the server and the other player.

- The server program that helps synchronize data between the two clients. This logic is defined in Server.hs.

## Challenges
- Deciding whether to include a server to synchronize data between two clients vs. having one of the player serving as a host and one as a client.
    - Solution: work on the server option first to separate from model implementation, if this doesn’t work out then shift towards the more integrated approach of having a host and client. 
- Rendering the game on a small terminal screen as Brick doesn’t display large grids on small terminals properly. 
    - Solution: We decided to use a large fixed size terminal and design small size tiles for Connect 4.
- Deciding how to organise our code since we are using brick for the first time. Solution: 
    - Solution: We went with a model view controller approach inspired by the starter code and we decide to make separate files for each screen to separate their logic and so it is easier to split and parallelize our work. 
- How to divide the work efficiently so we can parallelize the tasks.
    - Solution: We have a team working on the Model and the Controller, another team working on the View and another team working on the Network/Server function. 
- How to do version control and do work separately. 
    - Solution: Use git but we all use separate branches and do the merging at our meetings.

## Updates on Goals (11/29)
- We have completed implementing the model, and made progress on the controller and the view.
- We expect to complete all of the goals and tasks :)
