Xtreme3D for GameMaker
======================
This is a [GameMaker](https://gamemaker.io/) (aka GameMaker: Studio 2) binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is compatible with 64-bit GameMaker version 2022.800.0.161 and higher. Only Windows is supported at the moment.

Assets
------
All the files used by the engine (like models and textures) should go to the `datafiles` folder (local paths in Xtreme3D are relative to this folder). These are simply files that are included with your game. Currently there is no possibility to utilize GameMaker's native asset system in Xtreme3D.

Peculiarities
-------------
* Since Xtreme3D does its own rendering, the GameMaker's built-in graphics engine should be disabled. This can be done with `draw_enable_drawevent`, but this function should be called after the game window initialization (otherwise the window will not appear). We recommend adding an alarm in Create event:

```gml
alarm[0] = room_speed;
```

And in Alarm 0 event:

```gml
draw_enable_drawevent(false);
```
