var dt = 1.0 / 60; //delta_time / 1000000;

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

/*
VerletConstraintSetPosition(playerCollider,
    ObjectGetAbsolutePosition(camera, 0),
	ObjectGetAbsolutePosition(camera, 1) - 1,
	ObjectGetAbsolutePosition(camera, 2));
*/

EngineUpdate(dt);
//VerletWorldUpdate(verlet, current_time / 1000);

HUDTextSetText(text, string(fps));

if (global.canRender) {
	ShadowMapUpdate(shadowMap);
	ViewerRender(viewer);
	//ObjectHide(global.front);
	//ViewerRenderObject(viewer, fbo);
	//ObjectShow(global.front);
	//ObjectHide(fbo);
	//ViewerRenderObject(viewer, global.front);
	//ObjectShow(fbo);
}
