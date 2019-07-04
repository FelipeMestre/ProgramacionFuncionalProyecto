# ProgramacionFuncionalProyecto

Desarrollo del juego Hikara programacion funcionall 2019

```bash
*Main> :l hikara.hs
[1 of 1] Compiling Main             ( hikara.hs, interpreted )
Ok, modules loaded: Main.

*Main> juego= beginning 
*Main> activePlayer juego
BluePlayer

*Main> actions juego 
[(BluePlayer,[NewAction 0 0 1,NewAction 0 1 1,NewAction 0 2 1,NewAction 0 3 1,NewAction 1 0 1,NewAction 1 1 1,NewAction 1 2 1,NewAction 1 3 1,NewAction 2 0 1,NewAction 2 1 1,NewAction 2 2 1,NewAction 2 3 1,NewAction 3 0 1,NewAction 3 1 1,NewAction 3 2 1,NewAction 3 3 1]),(YellowPlayer,[])]
*Main> juego1 = next juego (BluePlayer, NewAction 3 0 1)

*Main> showHijaraPretty juego1
|1 2|   |1 2|   |1 2|   |1 2|
|3 4|   |3 4|   |3 4|   |3 4|

|1 2|   |1 2|   |1 2|   |b 2|
|3 4|   |3 4|   |3 4|   |3 4|

|1 2|   |1 2|   |1 2|   |1 2|
|3 4|   |3 4|   |3 4|   |3 4|

|1 2|   |1 2|   |1 2|   |1 2|
|3 4|   |3 4|   |3 4|   |3 4|

*Main> activePlayer juego1
YellowPlayer
```