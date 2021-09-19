Xtreme3D for GameMaker Studio 2
===============================
This is a GameMaker Studio binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). `demo` folder contains sample project ready to run.

Notes
-----
Rendering with Xtreme3D won't work properly with native GMS graphics. If two engines render to the same window, there will be flickering. To prevent this, GMS draw event should be disabled with `draw_enable_drawevent(false)`. This should be done after first frame, so we recommend setting up an alarm and calling `draw_enable_drawevent` in Alarm event. See the demo for details.
