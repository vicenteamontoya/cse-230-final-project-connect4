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
 - Brick: We will use this package for creating a terminal user interface (TUI) so that game feels interactive to the user.
 - WebSocket: We will use this package for creating sockets and a network connection between the two players.

## Possible Extensions
- Add more game modes that involve changing board size, length of connections and maybe even a 3D version.
- Make the game extensible to more than 2 players.
- Make any players not on the same network be able to play together.
- Add animations for dropping tiles, winning, and losing.
- Make tile designs and colors customizable.
- Add music and sound effects.
- Add a single player mode.
- Add local multiplayer (two players playing on the same computer).


