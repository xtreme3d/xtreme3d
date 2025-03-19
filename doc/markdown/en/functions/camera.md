# Camera

The purpose of the Camera object is clear from its name - it is a point of view of the scene, which, like other objects, has a position and orientation in space. You can create several cameras and switch between them at any time using the `ViewerSetCamera` function. You can also bind the camera to an object or control it with the mouse.

Remember that in order for the camera to show something, it must be assigned to the previously created viewer using the `ViewerSetCamera` function.

---

## CameraCreate

`real CameraCreate(real parent);`

Creates a new camera and returns a reference to it.

- `parent` - a reference to the parent for the camera (0 - no parent).

---

## CameraSetStyle

`real CameraSetStyle(real cam, real cs);`

Sets the so-called camera style (`cs`) - the method of projection on the screen. The following `cs` values ​​are available:
- `csPerspective = 0` - perspective (default value);
- `csOrthogonal = 1` - orthographic (parallel) projection (without perspective reduction). Orthographic projection at 45 degrees is called isometric;
- `csOrtho2D = 2` - infinite orthographic projection. The value set by the `CameraSetViewDepth` function (view distance) is ignored;
- `csInfinitePerspective = 3` - infinite perspective. The value set by the `CameraSetViewDepth` function (view distance) is ignored.

- `cam` - camera reference
- `cs` - camera style.

---

## CameraSetFocal

`real CameraSetFocal(real cam, real fov);`

Sets the camera angle. By changing this value, you can achieve the effect of zooming in and out.

- `cam` - camera reference
- `fov` - angle of view (default value: 50).

---

## CameraSetSceneScale

`real CameraSetSceneScale(real cam, real scale);`

Sets the camera image scale. Essentially, an analogue of `CameraSetFocal`, but does not affect perspective reduction.

- `cam` - camera reference
- `scale` - scale (default value: 1).

---

## CameraScaleScene

`real CameraScaleScene(real cam, real scale);`

Sets the camera image scale, like `CameraSetSceneScale`, but relative to the current scale value.

- `cam` - link to camera
- `scale` - added to current scale value.

---

## CameraSetViewDepth

`real CameraSetViewDepth(real cam, real depth);`

Sets the camera view distance - all objects further than this distance will not be drawn. This allows to increase rendering speed, but may lead to some artifacts. For example, if the view distance is too small, some objects may be partially "cut off", and if it is too large, they may "overlap" each other.

- `cam` - reference to the camera
- `depth` - view distance (default value: 100).

---

## CameraSetTargetObject

`real CameraSetTargetObject(real cam, real obj);`

Assigns the camera target object. In this case, the camera will always be directed at the target, wherever it is, and the usual rotation functions are ignored. The target can be any object - for example, a character controlled by the player.

Note that assigning a target object does not bind the camera position to the object - when you move the target, the camera will remain in place, and you need to move it yourself.

- `cam` - reference to the camera
- `obj` - reference to the target object.

---

## CameraMoveAroundTarget

`real CameraMoveAroundTarget(real cam, real pitch, real turn)

Rotates the camera around its target object. Rotation is performed along two axes - X and Y.

- `cam` - reference to the camera
- `pitch` - rotation angle along the X axis
- `turn` - rotation angle along the Y axis.

---

## CameraSetDistanceToTarget

`real CameraSetDistanceToTarget(real cam, real distance);`

Changes the distance between the camera and its target object (moves the camera closer or farther away from the target).

- `cam` - link to camera
- `distance` - distance.

---

## CameraGetDistanceToTarget

`real CameraGetDistanceToTarget(real cam);`

Returns the distance between the camera and its target object.

- `cam` - reference to the camera.

---

## CameraCopyToTexture

`real CameraCopyToTexture(real cam, string material, real width, real height);`

Copies the camera contents (rendered image) to the material texture.

- `cam` - reference to the camera
- `material` - name of the material
- `width`, `height` - width and height of the generated texture in pixels. Must be a power of two (i.e. 128, 256, 512, etc.).

---

## CameraGetNearPlane

`real CameraGetNearPlane(real cam);`

Returns the distance to the camera's near clipping plane (that is, the plane closer to which nothing is drawn).

- `cam` - a reference to the camera.

--

## CameraSetNearPlaneBias

`real CameraSetNearPlaneBias(real cam, real bias);`

Sets the coefficient of the camera's near clipping plane. It is impossible to change the near plane directly (as is done for the far plane by the CameraSetViewDepth function) - it is calculated using a special formula taking into account the viewing angle and the view resolution. Using this coefficient, you can shift the plane forward (values ​​greater than 1) or backward (values ​​less than 1).

- `cam` - a reference to the camera.
- `bias` - coefficient (default value: 1).

---

## CameraAbsoluteVectorToTarget

`real CameraAbsoluteVectorToTarget(real cam, real ind);`

Returns the Z vector in the orthonomial basis of the camera oriented to its target object (in other words, the absolute direction from the camera to the target object). If the camera has no target, its own absolute Direction vector is returned.

- `cam` is a reference to the camera.
- `ind` is the index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteRightVectorToTarget

`real CameraAbsoluteRightVectorToTarget(real cam, real ind);`

Returns the X vector in the orthonomial basis of the camera oriented to its target object (in other words, the absolute direction to the right of the camera relative to its target object). If the camera has no target, its own absolute Right vector is returned.

- `cam` is a reference to the camera
- `ind` is the index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteUpVectorToTarget

`real CameraAbsoluteUpVectorToTarget(real cam, real ind);`

Returns the Y vector in the orthogonal basis of the camera oriented to its target object (in other words, the absolute up direction from the camera relative to its target object). If the camera has no target, its own absolute Up vector is returned.

- `cam` is a reference to the camera
- `ind` is the index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraZoomAll

`real CameraZoomAll(real cam, real viewer);`

Positions the camera in space so that the view displaying the scene covers all objects.

- `cam` is a reference to the camera
- `viewer` is a reference to the view.

--

## CameraScreenDeltaToVector

`real CameraScreenDeltaToVector(real cam, real deltax, real deltay, real ratio, real normx, real normy, real normz, real ind);

Calculates the absolute translation vector corresponding to the on-screen translation vector of the camera. Useful function for implementing user interface elements on the scene (3D manipulators).

- `cam` - camera reference
- `deltax`, `deltay` - integer screen translation vector
- `ratio` - coefficient by which the screen vector is multiplied
- `normx`, `normy`, `normz` - normal of the plane in which the three-dimensional translation should be calculated
- `ind` - vector coordinate index (0 = X, 1 = Y, 2 = Z).

---

## CameraScreenDeltaToVectorXY

`real CameraScreenDeltaToVectorXY(real cam, real deltax, real deltay, real ratio, real ind);`

The function does the same as `CameraScreenDeltaToVector`, but is optimized for working in the XY plane.

- `cam` - reference to the camera
- `deltax`, `deltay` - integer screen translation vector
- `ratio` - coefficient by which the screen vector is multiplied
- `ind` - index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraScreenDeltaToVectorXZ

`real CameraScreenDeltaToVectorXZ(real cam, real deltax, real deltay, real ratio, real ind);`

The function does the same as `CameraScreenDeltaToVector`, but is optimized for working in the XZ plane.

- `cam` - reference to the camera
- `deltax`, `deltay` - integer screen translation vector
- `ratio` - coefficient by which the screen vector is multiplied
- `ind` - index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraScreenDeltaToVectorYZ

`real CameraScreenDeltaToVectorYZ(real cam, real deltax, real deltay, real ratio, real ind);`

The function does the same as `CameraScreenDeltaToVector`, but is optimized for working in the YZ plane.

- `cam` - reference to the camera
- `deltax`, `deltay` - integer screen translation vector
- `ratio` - coefficient by which the screen vector is multiplied
- `ind` - index of the vector coordinate (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteEyeSpaceVector

`real CameraAbsoluteEyeSpaceVector(real cam, real fordist, real rightdist, real updist, real ind);`

Calculates the absolute translation vector corresponding to the given translation vector in the camera view space.

- `cam` - camera reference
- `fordist`, `rightdist`, `updist` - translation forward, right and up
- `ind` - vector coordinate index (0 = X, 1 = Y, 2 = Z).

--

## CameraSetAutoLeveling

`real CameraSetAutoLeveling(real cam, real factor);`

Changes the camera's Direction vector so that the Up vector points up (0, 1, 0).

- `cam` - camera reference
- `factor` - the higher this value, the slower the vectors will be adjusted.

---

## CameraMoveInEyeSpace

`real CameraMoveInEyeSpace(real cam, real fordist, real rightdist, real updist);`

Moves the camera in its view space.

- `cam` - reference to the camera
- `fordist`, `rightdist`, `updist` - moves forward, right, and up.

--

## CameraMoveTargetInEyeSpace

`real CameraMoveTargetInEyeSpace(real cam, real fordist, real rightdist, real updist);`

Moves the camera target in its view space.

`cam` - reference to the camera
- `fordist`, `rightdist`, `updist` - moves forward, right, and up.

---

## CameraPointInFront

`real CameraPointInFront(real cam, real x, real y, real z);`

Returns true if the given point is in the camera's field of view.

- `cam` - camera reference
- `x`, `y`, `z` - point coordinates.

--

## CameraGetFieldOfView

`real CameraGetFieldOfView(real cam, real vpdim);`

Returns the camera's field of view angle in degrees.

`cam` - camera reference
`vpdim` - view dimension (the smaller of width and height).
