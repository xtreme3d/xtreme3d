Xtreme3D for GameMaker
======================
This is a [GameMaker](https://gamemaker.io/) (aka GameMaker: Studio 2) binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is compatible with 64-bit GameMaker version 2022.800.0.161 and higher. Only Windows is supported at the moment.

Assets
------
All the files used by the engine (like models and textures) should go to the `datafiles` folder (local paths in Xtreme3D are relative to this folder). These are simply files that are included with your game. Currently there is no possibility to utilize GameMaker's native asset system in Xtreme3D.

Peculiarities
-------------
GameMaker is constantly evolving, and unfortunately, as of 2026, it is not longer possible to do external rendering to GM's window. For such cases, Xtreme3D supports creating a custom window. GM's own window should be disabled, which is done by calling `draw_enable_drawevent(false)`. This means that all user input should be processed with Xtreme3D functions (`KeyIsPressed`, `MouseIsPressed`, `MouseGetPositionX`/`MouseGetPositionY`).
