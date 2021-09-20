var dt = delta_time / 1000000;

if (mouselookActive and window_has_focus()) {
	var deltax = (mx - display_mouse_get_x())/3;
	var deltay = (my - display_mouse_get_y())/3;
	ObjectRotate(camera, deltay, 0, 0);
	ObjectRotate(camPos, 0, -deltax, 0);
	display_mouse_set(mx, my);

	if (keyboard_check(ord("W"))) ObjectMove(camPos, -10 * dt);
	if (keyboard_check(ord("A"))) ObjectStrafe(camPos, 10 * dt);
	if (keyboard_check(ord("D"))) ObjectStrafe(camPos, -10 * dt);
	if (keyboard_check(ord("S"))) ObjectMove(camPos, 10 * dt);
}

if (keyboard_check_direct(mb_left) and window_has_focus()) {
	if (mb_left_released) {
		mb_left_released = false;
		mouselookActive = not mouselookActive;
		mx = display_get_width() / 2;
		my = display_get_height() / 2
		display_mouse_set(mx, my);
		MouseShowCursor(not mouselookActive);
	}
}
else {
	mb_left_released = true;
}

Update(dt);
ObjectHide(global.front);
ShadowMapRender(shadowMap);
ObjectShow(global.front);
ViewerRender(viewer);
