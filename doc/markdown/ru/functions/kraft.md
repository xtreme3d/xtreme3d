# Kraft

Функции физического движка Kraft. Это альтернатива ODE, впервые появившаяся в Xtreme3D 3.7 - современный физический движок, лишенный многих недостатков ODE, более простой и интуитивный. Для его использования вам не понадобятся дополнительные DLL, поскольку он встроен в xtreme3d.dll. Функционально он близок к ODE, хотя и имеет свои ограничения - например, в нем пока нет поддержки цилиндров и объектов Terrain.

Kraft разработан Benjamin Rosseaux aka BeRo.

---

## KraftCreate

`real KraftCreate();`

Создает физический мир Kraft и возвращает указатель на него. Миры предназначены для разделения всей игровой физики на отдельные, не связанные между собой "параллельные пространства". Все остальные объекты Kraft (твердые тела, геометрические формы и т.д.) создаются в рамках того или иного мира.

---

## KraftStep

`real KraftStep(real kraft, real dt);`

Совершает шаг интегрирования для мира. Эта процедура обновляет состояния тел, обнаруживает столкновения между ними и рассчитывает реакцию на столкновения. Ее следует вызывать в главном игровом цикле, перед функцией Update.

- `kraft` - id мира
- `dt` - шаг времени в секундах. При помощи этого параметра интегрируются ускорения и скорости тел. Обычно это время, прошедшее с момента прерыдущего обновления. Рекомендуется использовать шаг, равный 1/60 с (учитывая, что игра работает при 60 FPS).

---

## KraftRayCast

`real KraftRayCast(real kraft, real x, real y, real z, real dx, real dy, real dz, real maxDistance);`

Проверяет все тела мира на пересечение с заданным лучом. Возвращает true, если найдено пересечение, и false в противном случае.

- `kraft` - id мира
- `x`, `y`, `z` - точка начала луча
- `dx`, `dy`, `dz` - вектор направления луча
- `maxDistance` - максимальное расстояние луча (все тела, находящиеся дальше этого расстояния, не подлежат проверке).

---

## KraftGetRayHitPosition

`real KraftGetRayHitPosition(real index);`

Возвращает точку ближайшего пересечения, обнаруженного последним вызовом функции KraftRayCast.

- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftGetRayHitNormal

`real KraftGetRayHitNormal(real index);`

Возвращает нормаль поверхности в точке ближайшего пересечения, обнаруженного последним вызовом функции KraftRayCast.

- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftCreateRigidBody

`real KraftCreateRigidBody(real kr, real krbt);`

Создает твердое тело и возвращает его id.

- `kraft` - указатель на мир
- `krbt` - тип тела. Поддерживаются следующие значения `krbt`:
    - `krbtUnknown` = 0 - неизвестный тип;
    - `krbtStatic` = 1 - статическое тело (всегда имеет нулевую скорость, не реагирует на столкновения);
    - `krbtDynamic` = 2 - динамическое тело (может иметь ненулевую скорость, реагирует на столкновения);
    - `krbtKinematic` = 3 - кинематическое тело (может иметь ненулевую скорость, не реагирует на столкновения).

---

## KraftRigidBodyFinish

`real KraftRigidBodyFinish(real body);`

Завершает построение твердого тела, вычисляя массу, тензор инерции и некоторые другие внутренние параметры. Эту функцию необходимо вызвать после присвоения телу всех форм.

- `body` - указатель на тело.

---

## KraftRigidBodySetGravity

`real KraftRigidBodySetGravity(real body, real x, real y, real z, real scale);`

Задает телу вектор и величину гравитации.

- `body` - указатель на тело
- `x`, `y`, `z` - вектор гравитации
- `scale` - величина гравитации.

---

## KraftRigidBodySetPosition

`real KraftRigidBodySetPosition(real body, real x, real y, real z);`

Задает позицию тела.

- `body` - указатель на тело
- `x`, `y`, `z` - позиция.

---

## KraftRigidBodyGetPosition

`real KraftRigidBodyGetPosition(real body, real index);`

Возвращает позицию тела.

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodySetRotation

`real KraftRigidBodySetRotation(real body, real x, real y, real z);`

Задает поворот тела.

- `body` - указатель на тело
- `x`, `y`, `z` - углы поворота.

---

## KraftRigidBodyGetDirection

`real KraftRigidBodyGetDirection(real body, real index);`

Возвращает вектор направления тела.

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodyGetUp

`real KraftRigidBodyGetUp(real body, real index);`

Возвращает вектор Up тела (вертикальный вектор, перпендикулярный направлению).

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodyGetRight

`real KraftRigidBodyGetRight(real body, real index);`

Возвращает вектор Right тела (горизонтальный вектор, перпендикулярный направлению).

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodySetLinearVelocity

`real KraftRigidBodySetLinearVelocity(real body, real x, real y, real z);`

Задает поступательную скорость тела.

- `body` - указатель на тело
- `x`, `y`, `z` - скорость.

---

## KraftRigidBodyGetLinearVelocity

`real KraftRigidBodyGetLinearVelocity(real body, real index);`

Возвращает поступательную скорость тела.

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodySetAngularVelocity

`real KraftRigidBodySetAngularVelocity(real body, real x, real y, real z);`

Задает угловую скорость тела.

- `body` - указатель на тело
- `x`, `y`, `z` - угловая скорость.

---

## KraftRigidBodyGetAngularVelocity

`real KraftRigidBodyGetAngularVelocity(real body, real index);`

Возвращает угловую скорость тела.

- `body` - указатель на тело
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftRigidBodyAddForce

`real KraftRigidBodyAddForce(real body, real x, real y, real z);`

Прикладывает к телу силу в абсолютных координатах.

- `body` - указатель на тело
- `x`, `y`, `z` - сила.

---

## KraftRigidBodyAddForceAtPos

`real KraftRigidBodyAddForceAtPos(real body, real x, real y, real z, real px, real py, real pz);`

Прикладывает к телу силу в абсолютных координатах в заданной точке, вызывая вращение тела.

- `body` - указатель на тело
- `x`, `y`, `z` - сила
- `px`, `py`, `pz` - координаты точки.

---

## KraftRigidBodyAddRelForce

`real KraftRigidBodyAddRelForce(real body, real x, real y, real z);`

Прикладывает к телу силу в локальных координатах тела.

- `body` - указатель на тело
- `x`, `y`, `z` - сила.

---

## KraftObjectSetRigidBody

`real KraftObjectSetRigidBody(real object, real body);`

Присваивает объекту Xtreme3D тело Kraft. После вызова этой функции объект будет подчиняться физическому движку, и обычные функции трансформации для него перестанут работать.

- `object` - указатель на объект
- `body` - указатель на тело.

---

## KraftCreateShapeSphere

`real KraftCreateShapeSphere(real body, real radius);`

Добавляет телу форму сферы и возвращает указатель на нее.

- `body` - указатель на тело
- `radius` - радиус.

---

## KraftCreateShapeBox

`real KraftCreateShapeBox(real body, real x, real y, real z);`

Добавляет телу форму параллелепипеда и возвращает указатель на нее.

- `body` - указатель на тело
- `x`, `y`, `z` - полуразмер параллелепипеда по трем осям.

---

## KraftCreateShapePlane

`real KraftCreateShapePlane(real body, real x, real y, real z, real d);`

Добавляет телу форму бесконечной плоскости и возвращает указатель на нее.

- `body` - указатель на тело
- `x`, `y`, `z` - нормаль плоскости 
- `d` - расстояние от начала координат вдоль нормали.

---

## KraftCreateShapeCapsule

`real KraftCreateShapeCapsule(real body, real radius, real height);`

Добавляет телу форму капсулы и возвращает указатель на нее.

- `body` - указатель на тело
- `radius` - радиус
- `height` - высота.

---

## KraftCreateShapeMesh

`real KraftCreateShapeMesh(real body, real freeform);`

Добавляет телу форму меша и возвращает ее id. Эта функция применима только к статическим телам.

- `body` - указатель на тело
- `freeform` - указатель на объект свободной формы, чья геометрия должна быть скопирована для создания меша. Учитывается трансформация объекта.

---

## KraftShapeSetDensity

`real KraftShapeSetDensity(real shape, real density);`

Задает плотность формы, которая используется для вычисления массы.

- `shape` - указатель на форму
- `density` - плотность.

---

## KraftShapeSetFriction

`real KraftShapeSetFriction(real shape, real friction);`

Задает коэффициент трения формы. Чем больше этот параметр, тем больше требуется приложить силы для скольжения формы вдоль других поверхностей.

- `shape` - указатель на форму
- `friction` - коэффициент трения.

---

## KraftShapeSetRestitution

`real KraftShapeSetRestitution(real shape, real restitution);`

Задает коэффициент упругости формы. Чем больше этот параметр, тем сильнее форма отскакивает от других поверхностей.

- `shape` - указатель на форму
- `restitution` - коэффициент упругости.

---

## KraftShapeSetPosition

`real KraftShapeSetPosition(real shape, real x, real y, real z);`

Задает позицию формы в локальном пространстве тела.

- `shape` - указатель на форму
- `x`, `y`, `z` - позиция.

---

## KraftShapeGetPosition

`real KraftShapeGetPosition(real shape, real index);`

Возвращает позицию формы.

- `shape` - указатель на форму
- `index` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## KraftShapeSetRayCastable

`real KraftShapeSetRayCastable(real shape, real mode);`

Переключает учет формы при проверке пересечения с лучом. Эта функция полезна, если нужно выпустить луч из центра формы и при этом игнорировать ее саму. По умолчанию эта опция включена.

- `shape` - указатель на форму
- `mode` - true или false (1 и 0 соответственно).

---

## KraftCreateJointDistance

`real KraftCreateJointDistance(real body1, real body2);`

Создает сочленение расстояния, которое удерживает два тела на фиксированном расстоянии друг от друга, позволяя им свободно вращаться, и возвращает указатель на сочленение. Это похоже на виртуальную палку, на концах которой свободно болтаются тела.

- `body1` - указатель на тело 1
- `body2` - указатель на тело 2.

---

## KraftCreateJointRope

`real KraftCreateJointRope(real body1, real body2, real maxdistance);`

Создает веревочное сочленение, которое ограничивает максимально допустимое расстояние между двумя телами, позволяя им свободно вращаться, и возвращает указатель на сочленение. Это похоже на виртуальную веревку или цепь между двумя телами.

- `body1` - указатель на тело 1
- `body2` - указатель на тело 2
- `maxdistance` - максимально допустимое расстояние между телами.

---

## KraftCreateJointBallSocket

`real KraftCreateJointBallSocket(real body1, real body2);`

Создает шарнирное сочленение, которое ограничивает движение и поворот двух тел так, что якорная точка одного тела совпадает с якорной точкой второго, и возвращает указатель на сочленение.

- `body1` - указатель на тело 1
- `body2` - указатель на тело 2.

---

## KraftCreateJointFixed

`real KraftCreateJointFixed(real body1, real body2);`

Создает фиксированное сочленение, которое не позволяет двум телам двигаться и вращаться относительно друг друга, и возвращает указатель на сочленение. Фактически, это сочленение "склеивает" два тела в единое целое.

- `body1` - указатель на тело 1
- `body2` - указатель на тело 2.

---

## KraftCreateJointHinge

`real KraftCreateJointHinge(real body1, real body2);`

Создает сгибательное сочленение, которое ограничивает движение и поворот двух тел так, что они могут только вращаться вокруг заданной оси относительно друг друга, и возвращаетуказатель на сочленение. Это похоже на стену и дверь, а также колесо и ось.

- `body1` - указатель на тело 1
- `body2` - указатель на тело 2.

---

## KraftJointSetAnchor1

`real KraftJointSetAnchor1(real joint, real x, real y, real z);`

Задает первую якорную точку сочленения. Это точка в локальном пространстве первого тела сочленения. В шарнирном, сгибательном и фиксированном сочленениях тела ограничиваются так, что в абсолютном пространстве первая якорная точка совпадает со второй.

- `joint` - указатель на сочленение
- `x`, `y`, `z` - координаты точки.

---

## KraftJointSetAnchor2

`real KraftJointSetAnchor2(real joint, real x, real y, real z);`

Задает вторую якорную точку сочленения. Это точка в локальном пространстве второго тела сочленения. В шарнирном, сгибательном и фиксированном сочленениях тела ограничиваются так, что в абсолютном пространстве первая якорная точка совпадает со второй.

- `joint` - указатель на сочленение
- `x`, `y`, `z` - координаты точки.

---

## KraftJointSetHingeAxis1

`real KraftJointSetHingeAxis1(real joint, real x, real y, real z);`

Задает первую ось в сгибательном сочленении. Это вектор в локальном пространстве первого тела сочленения. Тела ограничиваются так, что в абсолютном пространстве первая ось совпадает со второй.

- `joint` - указатель на сочленение
- `x`, `y`, `z` - вектор оси.

---

## KraftJointSetHingeAxis2

`real KraftJointSetHingeAxis2(real joint, real x, real y, real z);`

Задает вторую ось в сгибательном сочленении. Это вектор в локальном пространстве второго тела сочленения. Тела ограничиваются так, что в абсолютном пространстве первая ось совпадает со второй.

- `joint` - указатель на сочленение
- `x`, `y`, `z` - вектор оси.
