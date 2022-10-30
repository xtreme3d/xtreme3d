if (!global.canRender)
	exit;

var dt = delta_time / 1000000;

/*
if (mouselookActive and window_has_focus()) {
	var deltax = (mx - MouseGetPositionX())/3;
	var deltay = (my - MouseGetPositionY())/3;
	ObjectRotate(camera, deltay, 0, 0);
	ObjectRotate(camPos, 0, -deltax, 0);
	MouseSetPosition(mx, my);

	if (keyboard_check(ord("W"))) ObjectMove(camPos, -10 * dt);
	if (keyboard_check(ord("A"))) ObjectStrafe(camPos, 10 * dt);
	if (keyboard_check(ord("D"))) ObjectStrafe(camPos, -10 * dt);
	if (keyboard_check(ord("S"))) ObjectMove(camPos, 10 * dt);
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
*/

EngineUpdate(dt);
//ObjectHide(global.front);
//ShadowMapRender(shadowMap);
//ObjectShow(global.front);
ViewerRender(viewer);
