var dt = 1.0 / 60;

if (mouselookActive and window_has_focus()) {
    var deltax = (mx - MouseGetPositionX())/6;
    var deltay = (my - MouseGetPositionY())/6;
    ObjectRotate(camera, deltay, 0, 0);
    ObjectRotate(camPos, 0, -deltax, 0);
    MouseSetPosition(mx, my);

    if (keyboard_check(ord("W"))) ObjectMove(camPos, -5 * dt);
    if (keyboard_check(ord("A"))) ObjectStrafe(camPos, 5 * dt);
    if (keyboard_check(ord("D"))) ObjectStrafe(camPos, -5 * dt);
    if (keyboard_check(ord("S"))) ObjectMove(camPos, 5 * dt);
}

if (MouseIsPressed(mb_left) and window_has_focus()) {
    if (mb_left_released) {
        mb_left_released = false;
        mouselookActive = not mouselookActive;
        mx = display_get_width() / 2;
        my = display_get_height() / 2
        MouseSetPosition(mx, my);
        MouseShowCursor(not mouselookActive);
    }
}
else {
    mb_left_released = true;
}

if ObjectSceneRaycast(camera, raycastObjects)
{
    var rx = EngineGetLastRaycastPosition(0);
    var ry = EngineGetLastRaycastPosition(1);
    var rz = EngineGetLastRaycastPosition(2);
    ObjectSetPosition(sphereMarker, rx, ry, rz);
}

KraftStep(kraft, dt);

EngineUpdate(dt);

HUDTextSetText(text, "FPS: " + string(fps));

ShadowMapUpdate(shadowMap);
ViewerRender(viewer);
