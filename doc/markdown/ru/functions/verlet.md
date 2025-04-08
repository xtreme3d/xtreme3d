# Verlet

VerletWorld - это физический движок для симуляции мягких тел (таких, как ткань, веревка, волосы, желе и др.). Мягкие тела видут себя, как в реальной жизни: например, ткань может колыхаться от ветра с учетом силы гравитациии и других свойств (прыгучесть, вес, инерция). Также мягкие тела могут сталкиваться со статическими твердыми поверхностями (Constraint) различных форм: плоскость, параллелепипед, сфера, цилиндр, капсула (к сожалению, поддержки Freeform нет). VerletNode - это узлы (вершины меша) мягких тел, с помощью которых можно частично манипулировать мягким телом.

VerletWorld - очень конфликтный движок. Когда присутствуют слишком большие значения в аргументах функций, движок начнет "ломаться". Например, если задать узлам мягкого тела маленький вес и при этом активировать сильный ветер, мягкие тела начнут вести себя неестественно, и произойдет ошибка, поэтому не прибегайте к поспешным выводам, если движок зависнет. Поэкспериментируйте с параметрами, и проблема устранится.

---

## VerletWorldCreate

`real VerletWorldCreate(real iterations, real usp, real drag);`

Создает новый физический мир Verlet и возвращает указатель на него.

- `iterations` - качество симуляции мира. Рекомендуется выставлять значение в диапазоне от 0.0 до 30
- `usp` - режим обновления пространства. Поддерживаются следующие значения usp:
    - `uspEveryIteration` =
    - `uspEveryFrame` =
    - `uspNever` =
- `drag` - сведения отсутствуют.

---

## VerletWorldCreateOctree

`real VerletWorldCreateOctree(real verletWorld, real xmin, real ymin, real zmin, real xmax, real ymax, real zmax, real leafThreshold, real maxTreeDepth);`

Создает октарное дерево для мира Verlet.

- `verletWorld` - указатель на мир
- `xmin`, `ymin`, `zmin` - сведения отсутствуют
- `xmax`, `ymax`, `zmax` - сведения отсутствуют
- `leafThreshold` - сведения отсутствуют
- `maxTreeDepth` - сведения отсутствуют.

---

## VerletWorldGravityCreate

`real VerletWorldGravityCreate(real verletWorld, real xDir, real yDir, real zDir);`

Добавляет силу гравитации в Verlet-мир и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `xDir`, `yDir`, `zDir` - вектор гравитации.

---

## VerletWorldGravitySetDirection

`real VerletWorldGravitySetDirection(real gravity, real xDir, real yDir, real zDir);`

Задает направление гравитации, созданной функцией `VerletWorldGravityCreate`.

- `gravity` - указатель на силу гравитации
- `xDir`, `yDir`, `zDir` - вектор гравитации.

---

## VerletWorldUpdate

`real VerletWorldUpdate(real verletWorld, real newTime);`

Обновляет состояние Verlet-мира.

- `verletWorld` - указатель на мир
- `newTime` - сведения отсутствуют.

---

## VerletWorldSetMaxDeltaTime

`real VerletWorldSetMaxDeltaTime(real verletWorld, real maxTime);`

Задает скорость обновления мира.

- `verletWorld` - указатель на мир
- `maxTime` - скорость обновления мира.

---

## VerletWorldSetSimTime

`real VerletWorldSetSimTime(real verletWorld, real time);`

Сведения отсутствуют.

- `verletWorld` - указатель на мир
- `time` - сведения отсутствуют.

---

## VerletAirResistanceCreate

`real VerletAirResistanceCreate(real verletWorld, real magnitudе, real chaos);`

Добавляет ветер в Verlet-мир, возвращая указатель на него.

- `verletWorld` - указатель на мир
- `magnitudе` - скорость ветра. Значение от 0 до 10
- `chaos` - хаотичность ветра.

---

## VerletAirResistanceSetWindDirection

`real VerletAirResistanceSetWindDirection(real airResistance, real x, real y, real z);`

Задает направление ветра.

- `airResistance` - указатель на ветер
- `x`, `y`, `z` - направление ветра.

---

## VerletAirResistanceSetWindMagnitude

`real VerletAirResistanceSetWindMagnitude(real airResistance, real magnitudе);`

Задает скорость ветра.

- `airResistance` - указатель на ветер
- `magnitudе` - скорость.

---

## VerletAirResistanceSetWindChaos

`real VerletAirResistanceSetWindChaos(real airResistance, real chaos);`

Задает хаотичность ветра.

- `airResistance` - указатель на ветер
- `chaos` - хаотичность ветра.

---

## VerletEdgeDetectorCreate

`real VerletEdgeDetectorCreate(real verletWorld, real object);`

Задает мягкое тело в Verlet-мире и возвращает указатель на него. После этого объект начнет ввести себя, как ткань.

- `verletWorld` - указатель на мир
- `object` - указатель на объект.

---

## VerletEdgeDetectorSetWeldDistance

`real VerletEdgeDetectorSetWeldDistance(real edgeDetector, real d);`

Сведения отсутствуют.

- `edgeDetector` - указатель на мягкое тело
- `d` - сведения отсутствуют.

---

## VerletConstraintFloorCreate

`real VerletConstraintFloorCreate(real verletWorld, real bounceRatio, real floorLevel);`

Создает твердую поверхность в виде плоскости и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `bounceRatio` - пружинистость поверхности
- `floorLevel` - уровень высоты столкновения мягких тел. Например, если видно, как ткань проходит сквозь поверхность, этим параметром можно убрать подобный артефакт, указав значение > 1.

---

## VerletConstraintFloorSetNormal

`real VerletConstraintFloorSetNormal(real constraintFloor, real xn, real yn, real zn);`

Задает нормаль твердой поверхности.

- `constraintFloor` - указатель на поверхность
- `xn`, `yn`, `zn` - вектор нормали.

---

## VerletConstraintFloorSetObjectLocations

`real VerletConstraintFloorSetObjectLocations(real constraintFloor, real object);`

Применяет к твердой поверхности данные положения и вращения объекта.

- `constraintFloor` - указатель на поверхность
- `object` - указатель на объект.

---

## VerletConstraintSphereCreate

`real VerletConstraintSphereCreate(real verletWorld, real radius);`

Создает твердую поверхность в виде сферы и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `radius` - радиус сферы.

---

## VerletConstraintCylinderCreate

`real VerletConstraintCylinderCreate(real verletWorld, real radius);`

Создает твердую поверхность в виде цилиндра и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `radius` - радиус цилиндра.

---

## VerletConstraintCylinderSetAxis

`real VerletConstraintCylinderSetAxis(real constraintCylinder, real xd, real yd, real zd);`

Задает направление цилиндра.

- `constraintCylinder` - указатель на цилиндр
- `xd`, `yd`, `zd` - направление.

---

## VerletConstraintCubeCreate

`real VerletConstraintCubeCreate(real verletWorld, real x, real y, real z);`

Создает твердую поверхность в виде куба и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `x`, `y`, `z` - размер куба.

---

## VerletConstraintCubeCreateSetCube

`real VerletConstraintCubeCreateSetCube(real verletWorld, real cube);`

Создает твердую поверхность в виде куба, копируя данные куба примитива (положение, размер), и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `cube` - указатель на куб.

---

## VerletConstraintCubeSetDirection

`real VerletConstraintCubeSetDirection(real constraintCube, real x, real y, real z);`

Задает направление куба.

- `constraintCube` - указатель на куб
- `x`, `y`, `z` - направление.

---

## VerletConstraintCapsuleCreate

`real VerletConstraintCapsuleCreate(real verletWorld, real radius, real length);`

Создает твердую поверхность в виде капсулы и возвращает указатель на нее.

- `verletWorld` - указатель на мир
- `radius` - радиус капсулы
- `length` - длина капсулы.

---

## VerletConstraintCapsuleSetAxis

`real VerletConstraintCapsuleSetAxis(real constraintCapsule, real x, real y, real z);`

Задает направление капсулы.

- `constraintCapsule` - указатель на капсулу
- `x`, `y`, `z` - направление.

---

## VerletConstraintSetPosition

`real VerletConstraintSetPosition(real constraint, real x, real y, real z);`

Перемещает твердую поверхность.

- `constraint` - указатель на поверхность
- `x`, `y`, `z` - положение в пространстве.

---

## VerletConstraintSetFrictionRatio

`real VerletConstraintSetFrictionRatio(real constraint, real frictionRatio);`

Задает силу трения поверхности - насколько сильно мягкие тела будут скользить по этой поверхности.

- `constraint` - указатель на поверхность
- `frictionRatio` - сила трения.

---

## VerletConstraintSetEnabled

`real VerletConstraintSetEnabled(real constraint, real mode);`

Включает или выключает твердую поверхность в Verlet-мире.

- `constraint` - указатель на поверхность
- `mode` - `true` или `false`.

---

## VerletConstraintSetSlack

`real VerletConstraintSetSlack(real verletWorld, real constraint, real slack);`

Сведения отсутствуют.

- `verletWorld` - указатель на мир
- `constraint` - указатель на поверхность
- `slack` - сведения отсутствуют.

---

## VerletGetNodeCount

`real VerletGetNodeCount(real verletWorld);`

Возвращает общее количество узлов в Verlet-мире.

- `verletWorld` - указатель на мир.

---

## VerletNodeNailedDown

`real VerletNodeNailedDown(real verletWorld, real nodeIndex, real mode);`

Закрепляет или открепляет узел мягкого тела в пространстве.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `enable` - `true` или `false`.

---

## VerletNodeSetPosition

`real VerletNodeSetPosition(real verletWorld, real nodeIndex, real x, real y, real z);`

Тянет узел мягкого тела в позицию x, y, z.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `x`, `y`, `z` - положение в пространстве.

---

## VerletNodeSetRadius

`real VerletNodeSetRadius(real verletWorld, real nodeIndex, real radius);`

Задает радиус столкновения узла с твердыми поверхностями.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `radius` - радиус.

---

## VerletNodeSetFriction

`real VerletNodeSetFriction(real verletWorld, real nodeIndex, real friction);`

Задает силу трения узла по твердым поверхностям.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `friction` - сила трения.

---

## VerletNodeSetWeight

`real VerletNodeSetWeight(real verletWorld, real nodeIndex, real weight);`

Задает вес узла.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `weight` - вес.

---

## VerletNodeApplyFriction

`real VerletNodeApplyFriction(real verletWorld, real nodeIndex, real friction, real penetrationDepth, real x, real y, real z);`

Применяет силу толчка к узлу.

- `verletWorld` - указатель на мир
- `nodeIndex` - индекс узла (вершины)
- `friction` - сила трения
- `penetrationDepth` - сведения отсутствуют
- `x`, `y`, `z` - направление толчка.
