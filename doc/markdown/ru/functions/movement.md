# Movement

Объект Movement (движение) позволяет задавать объектам траектории движения. Траектории называются путями (Path). Путь может представлять собой набор отрезков, либо сплайн (кривую) - кубический, сплайн Безье или NURBS. Собственное движение объекта и движение по пути взаимоисключающи - то есть, пока объект движется по пути, его нельзя двигать обычными функциями ObjectMove и др. Объект Movement включает один или несколько путей, и вы можете переключаться между ними. Также возможен последовательный проход всех путей друг за другом.

---

## MovementCreate

`real MovementCreate(real object);`

Создает объект Movement, который будет управлять движением заданного объекта, и возвращает его id.

- `object` - указатель на подконтрольный объект.

---

## MovementAddPath

`real MovementAddPath(real movement);`

Добавляет объекту Movement новый путь и возвращает указатель на него (не индекс!).

- `movement` - указатель на объект Movement.

---

## MovementSetActivePath

`real MovementSetActivePath(real movement, real index);`

Задает текущий (активный) путь объекта Movement. Функция `MovementStart` всегда начинает движение с первого узла этого пути.

- `movement` - указатель на объект Movement
- `index` - индекс пути, начиная с 0. Значение по умолчанию - 0.

---

## MovementStart

`real MovementStart(real movement)

Запускает движение подконтрольного объекта. Позиция и поворот объекта при этом всегда сбрасываются на начальный узел текущего пути.

- `movement` - указатель на объект Movement.

---

## MovementStop

`real MovementStop(real movement);`

Останавливает движение. После остановки подконтрольный объект можно двигать обычными функциями.

- `movement` - указатель на объект Movement.

---

## MovementAutoStartNextPath

`real MovementAutoStartNextPath(real movement, real mode);`

Определяет, переключаться ли автоматически на следующий путь, когда завершается текущий.

- `movement` - указатель на объект Movement
- `mode` - `true` или `false` (1 и 0 соответственно). По умолчанию опция включена.

---

## MovementPathSetSplineMode

`real MovementPathSetSplineMode(real path, real lsm);`

Задает тип сплайна пути. Подробнее о типах сплайнов вы можете прочитать в разделе [Lines](lines.html).

- `path` - указатель на путь
- `lsm` - тип сплайна. Доступны следующие значения `lsm`:
    - `lsmLines` = 0 - набор отрезков
    - `lsmCubicSpline` = 1 - кубический сплайн (значение по умолчанию)
    - `lsmBezierSpline` = 2 - сплайн Безье
    - `lsmNURBSCurve` = 3 - NURBS.

---

## MovementPathAddNode

`real MovementPathAddNode(real path);`

Добавляет в путь новый узел и возвращает его id. Узел - это контрольная точка, определяющая направление пути. Кроме позиции, узел определяет также и поворот - таким образом, объект во время перемещения может плавно вращаться от одного угла к другому. Кроме того, узел определяет скорость движения объекта вдоль отрезка пути.

- `path` - указатель на путь.

---

## MovementPathDeleteNode

`real MovementPathDeleteNode(real path, real node);`

Удаляет узел из пути.

- `path` - указатель на путь
- `node` - указатель на узел.

---

## MovementPathNodeSetPosition

`real MovementPathNodeSetPosition(real node, real x, real y, real z);`

Задает позицию узла.

- `node` - указатель на узел
- `x`, `y`, `z` - позиция.

---

## MovementPathNodeSetRotation

`real MovementPathNodeSetRotation(real node, real pitch, real turn, real roll);`

Задает поворот узла.

- `node` - указатель на узел
- `pitch`, `turn`, `roll` - углы поворота в градусах по трем осям.

---

## MovementPathNodeSetSpeed

`real MovementPathNodeSetSpeed(real node, real speed);`

Задает скорость узла.

- `node` - указатель на узел
- `speed` - скорость.

---

## MovementPathShow

`real MovementPathShow(real path, real show);`

Переключает отображение пути в виде линий.

- `path` - указатель на узел
- `show` - видимость (`true` или `false`).

---

## MovementPathSetLoop

`real MovementPathSetLoop(real path, real loop);`

Переключает зацикленность пути.

- `path` - указатель на узел
- `loop` - `true` или `false`.
