# Урок 10. Камера от третьего лица

Многие игры используют вид от третьего лица, где камера показывает персонажа "со спины" - это, например, многие игры жанров Action и RPG, 3D-платформеры типа Spyro или Crash Bandicoot, спортивные симуляторы и т.д. При этом, как правило, камера не является жестко зафиксированной на определенном расстоянии от персонажа - она обычно перемещается плавно, с некоторым запаздыванием, что добавляет реализма и кинематографичности.

На Xtreme3D подобную механику реализовать ненамного сложнее, чем вид от первого лица. Нижеследующий код создаст иерархию из персонажа, которым игрок будет управлять, и камеры, которая будет за ним следить. В качестве условного персонажа используется простой куб.

Код в событии Create:

```gml
camera = CameraCreate(global.scene);
CameraSetViewDepth(camera, 800);
CameraSetFocal(camera, 80);
ViewerSetCamera(view1, camera);

actor = CubeCreate(1, 1, 1, global.scene);

target = DummycubeCreate(actor);
ObjectSetPosition(target, 0, 1, -4);
CameraSetTargetObject(camera, actor);
```

Код в событии Step:

```gml
if keyboard_check(vk_up) ObjectMove(actor, 10 * dt)
if keyboard_check(vk_down) ObjectMove(actor, -10 * dt);
if keyboard_check(vk_left) ObjectTurn(actor, -200 * dt);
if keyboard_check(vk_right) ObjectTurn(actor, 200 * dt);

cx = ObjectGetAbsolutePosition(camera, 0);
cy = ObjectGetAbsolutePosition(camera, 1);
cz = ObjectGetAbsolutePosition(camera, 2);
tx = ObjectGetAbsolutePosition(target, 0);
ty = ObjectGetAbsolutePosition(target, 1);
tz = ObjectGetAbsolutePosition(target, 2);
dx = tx - cx;
dy = ty - cy;
dz = tz - cz;
ObjectTranslate(camera, dx * 0.05, dy * 0.05, dz * 0.05);
```

Логика камеры устроена так, что ее наиболее дальняя дистанция от персонажа - при движении вперед (чтобы можно было хорошо видеть, что творится вокруг), а самая близкая - при движении назад. При повороте персонажа камера позволяет рассмотреть его сбоку. Примерно такой же прием используется в гоночных симуляторах, так что на основе этого кода вполне можно сделать и движок гонок.
