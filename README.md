# Mastermind Solver 

A long, long time ago back in 2002 (which in computer terms equals to an eternity and when my internet connection ran at a whooping 4KB/s), I was asked to write a solver for the Mastermind game.

It uses the Greenwell keys, whose answers allowed to readily solve the game after the 6th answer. It was written fully in *Haskell* and it was The "first" real project that I have done for my BSc. 

This is the main reason I include it here, not for its quality, but for what it personally represents. Also all the code is written with Portuguese function names which adds more weight to the memorabilia factor.

## Mastermind Game
For more information take a look in https://en.wikipedia.org/wiki/Mastermind_(board_game)

## Greenwell Keys
The Mastermind answers provided by the Greenwell keys, provide unique non-ambigous information used to indentify the secret code of the game.
These are the main "trick" for the whole project.

```hs
[ [1,2,2,1], [2,3,5,4], [3,3,1,1], [4,5,2,4], [5,6,5,6], [6,6,4,3] ]
```
## Run this project
Install the Haskell interpreter and point to the MM module. 


## Licence
The MIT License (MIT)
Copyright (c) 2015

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
