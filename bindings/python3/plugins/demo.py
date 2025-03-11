# -*- coding: utf-8 -*-

# An example plugin. You can add your own logics here.

x3d = None

def setup(app):
    global x3d
    x3d = app.x3d
    app.cube = x3d.CubeCreate(1, 1, 1, app.scene)
    x3d.ObjectSetPosition(app.cube, 0, 0.5, 0)
    print('demo.py plugin has created a cube')

def update(app, dt):
    global x3d
    pass
