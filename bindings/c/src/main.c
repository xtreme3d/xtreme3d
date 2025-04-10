﻿/*
 * Xtreme3D example in C
 */

#include <stdio.h>
#include <windows.h>
#include <xtreme3d.h>

double RightAscension(double aHours, double aMinutes, double aSeconds)
{
    return 15.0 * aHours + (15.0 / 60.0) * aMinutes + (15.0 / 3660.0) * aSeconds;
}

double Declination(double aDegrees, double aMinutes, double aSeconds)
{
    return aDegrees + (1.0 / 60.0) * aMinutes + (1.0 / 3660.0) * aSeconds;
}

int main(int argc, char** argv)
{
    /* If you don't need the console window, uncomment the following: */
    /* ShowWindow(GetConsoleWindow(), SW_HIDE); */

    printf("Xtreme3D example\n");

    EngineCreate();
    EngineShowLoadingErrors(1);
    EngineSetCulling(vcNone);
    EngineSetObjectsSorting(osNone);
    EngineSetMaxLights(8);

    double windowWidth = 1280;
    double windowHeight = 720;

    double window = WindowCreate(0, 0, windowWidth, windowHeight, 0);
    WindowCenter(window);
    WindowSetTitle(window, "Xtreme3D 4.0");

    double viewer = ViewerCreate(0, 0, windowWidth, windowHeight, WindowGetHandle(window));
    ViewerSetBackgroundColor(viewer, c_gray);
    ViewerSetLighting(viewer, 1);
    ViewerEnableFog(viewer, 1);
    ViewerSetFogColor(viewer, 8421504.0);
    ViewerSetFogDistance(viewer, 0, 50);
    ViewerSetAntiAliasing(viewer, csa8xHQ);
    ViewerEnableVSync(viewer, vsmSync);
    ViewerSetAutoRender(viewer, 0);

    double matlib = MaterialLibraryCreate();
    MaterialLibraryActivate(matlib);

    double back = DummycubeCreate(0);
    double scene = DummycubeCreate(0);
    double front = DummycubeCreate(0);

    double camPos = DummycubeCreate(scene);
    double camera = CameraCreate(camPos);

    ViewerSetCamera(viewer, camera);
    ObjectSetPosition(camPos, 0, 1.8, 3);
    CameraSetViewDepth(camera, 1000);
    CameraSetFocal(camera, 100);
    CameraSetNearPlaneBias(camera, 0.2);

    double light1 = LightCreate(lsOmni, scene);
    LightSetAmbientColor(light1, c_black);
    LightSetDiffuseColor(light1, c_white);
    LightSetSpecularColor(light1, c_white);
    ObjectSetPosition(light1, 1, 3, 1);

    double plane = PlaneCreate(0, 20, 20, 10, 10, scene);
    ObjectPitch(plane, 90);
    MaterialCreate("mPlane", "data/ground.jpg");
    ObjectSetMaterial(plane, "mPlane");

    double cube = CubeCreate(1, 1, 1, scene);
    ObjectSetPosition(cube, 0, 0.5, 0);

    int mouselookActive = 1;
    int mbLeftReleased = 1;

    double pmx = MouseGetPositionX();
    double pmy = MouseGetPositionY();
    double mx = WindowGetPosition(window, 0) + windowWidth * 0.5;
    double my = WindowGetPosition(window, 1) + windowHeight * 0.5;
    MouseSetPosition(mx, my);
    MouseShowCursor(!mouselookActive);

    int running = 1;
    double timer = 0.0;
    double dt = 1.0 / 60.0;

    while (running)
    {
        WindowDispatch();

        double timeStep = EngineGetTimeStep();
        timer += timeStep;
        if (timer >= dt)
        {
            timer -= dt;

            int escPressed = 0;
            if (mouselookActive && WindowIsActive(window))
            {
                mx = WindowGetPosition(window, 0) + windowWidth * 0.5;
                my = WindowGetPosition(window, 1) + windowHeight * 0.5;
                double deltax = (mx - MouseGetPositionX()) / 6;
                double deltay = (my - MouseGetPositionY()) / 6;

                ObjectRotate(camera, deltay, 0, 0);
                ObjectRotate(camPos, 0, -deltax, 0);
                MouseSetPosition(mx, my);
            }

            if (KeyIsPressed('W')) ObjectMove(camPos, -5 * dt);
            if (KeyIsPressed('A')) ObjectStrafe(camPos, 5 * dt);
            if (KeyIsPressed('D')) ObjectStrafe(camPos, -5 * dt);
            if (KeyIsPressed('S')) ObjectMove(camPos, 5 * dt);

            if (KeyIsPressed(vk_escape)) escPressed = 1;

            if (MouseIsPressed(mb_left) && WindowIsActive(window))
            {
                if (mbLeftReleased)
                {
                    mbLeftReleased = 0;
                    mouselookActive = !mouselookActive;
                    if (!mouselookActive)
                    {
                        MouseSetPosition(pmx, pmy);
                        MouseShowCursor(1);
                    }
                    else
                    {
                        pmx = MouseGetPositionX();
                        pmy = MouseGetPositionY();
                        mx = WindowGetPosition(window, 0) + windowWidth * 0.5;
                        my = WindowGetPosition(window, 1) + windowHeight * 0.5;
                        MouseSetPosition(mx, my);
                        MouseShowCursor(0);
                    }
                }
            }
            else mbLeftReleased = 1;

            if (escPressed)
                running = 0;
            else
                running = (int)WindowIsShowing(window);

            EngineUpdate(dt);
            ViewerRender(viewer);
        }
    }

    return 0;
}
