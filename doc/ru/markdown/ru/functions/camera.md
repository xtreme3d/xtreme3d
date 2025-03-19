# Camera

Назначение объекта камера (Camera) ясно из названия - это точка обзора сцены, которая, как и другие объекты, имеет позицию и ориентацию в пространстве. Вы можете создать несколько камер и переключаться между ними в любое время функцией `ViewerSetCamera`. Можно также привязать камеру к объекту или управлять ей при помощи мыши.

Помните, что для того, чтобы камера что-то показывала, ее необходимо назначить созданному ранее виду функцией `ViewerSetCamera`.

---

## CameraCreate

`real CameraCreate(real parent);`

Создает новую камеру и возвращает ссылку на нее.

- `parent` - ссылка на родителя для камеры (0 - отсутствие родителя). 

---

## CameraSetStyle

`real CameraSetStyle(real cam, real cs);`

Задает так называемый стиль камеры (`cs`) - метод проекции на экран. Доступны следующие значения `cs`:
- `csPerspective = 0` - перспектива (значение по умолчанию);
- `csOrthogonal = 1` - ортографическая (параллельная) проекция (без перспективного сокращения). Ортографическая проекция под углом 45 градусов называется изометрией;
- `csOrtho2D = 2` - бесконечная ортографическая проекция. Значение, задаваемое функцией `CameraSetViewDepth` (дальность обзора) игнорируется;
- `csInfinitePerspective = 3` - бесконечная перспектива. Значение, задаваемое функцией `CameraSetViewDepth` (дальность обзора) игнорируется.

- `cam` - ссылка на камеру
- `cs` - стиль камеры.

---

## CameraSetFocal

`real CameraSetFocal(real cam, real fov);`

Задает угол зрения камеры. Изменяя это значение, можно добиться эффекта приближения/отдаления.

- `cam` - ссылка на камеру
- `fov` - угол зрения (значение по умолчанию: 50).

---

## CameraSetSceneScale

`real CameraSetSceneScale(real cam, real scale);`

Задает масштаб изображения камеры. По сути, аналог `CameraSetFocal`, но не затрагивающий перспективное сокращение.

- `cam` - ссылка на камеру
- `scale` - масштаб (значение по умолчанию: 1).

---

## CameraScaleScene

`real CameraScaleScene(real cam, real scale);`

Задает масштаб изображения камеры, как и `CameraSetSceneScale`, но относительно текущего значения масштаба. 

- `cam` - ссылка на камеру
- `scale` - добавляется к текущему значению масштаба.

---

## CameraSetViewDepth

`real CameraSetViewDepth(real cam, real depth);`

Задает дальность обзора камеры - все объекты дальше этого расстояния не будут отрисованы. Это позволяет увеличить скорость рендеринга, но может привести к некоторым артефактам. Например, при слишком маленькой дальности обзора некоторые объекты могут частично "обрезаться", а при слишком большой - "наползать" друг на друга.

- `cam` - ссылка на камеру
- `depth` - дальность обзора (значение по умолчанию: 100).

---

## CameraSetTargetObject

`real CameraSetTargetObject(real cam, real obj);`

Назначает объект-цель камеры. При этом камера будет всегда направлена на цель, где бы она ни находилась, а обычные функции поворота игнорируются. Целью может быть любой объект - например, персонаж, которым управляет игрок.

Обратите внимание, что назначение объекта-цели не привязывает позицию камеры к объекту - при перемещении цели камера останется на своем месте, и вам нужно двигать ее самостоятельно.

- `cam` - ссылка на камеру
- `obj` - ссылка на объект-цель.

---

## CameraMoveAroundTarget

`real CameraMoveAroundTarget(real cam, real pitch, real turn)

Вращает камеру вокруг ее объекта-цели. Вращение осуществляется по двум осям - X и Y.

- `cam` - ссылка на камеру
- `pitch` - угол поворота по оси X
- `turn` - угол поворота по оси Y.

---

## CameraSetDistanceToTarget

`real CameraSetDistanceToTarget(real cam, real distance);`

Изменяет расстояние между камерой и ее объектом-целью (приближает или отдаляет камеру относительно цели).

- `cam` - ссылка на камеру
- `distance` - расстояние.

---

## CameraGetDistanceToTarget

`real CameraGetDistanceToTarget(real cam);`

Возвращает расстояние между камерой и ее объектом-целью.

- `cam` - ссылка на камеру.

---

## CameraCopyToTexture

`real CameraCopyToTexture(real cam, string material, real width, real height);`

Копирует содержимое камеры (отрисованное изображение) в текстуру материала.

- `cam` - ссылка на камеру
- `material` - имя материала
- `width`, `height` - ширина и высота генерируемой текстуры в пикселях. Должны быть степенью двойки (то есть, например, 128, 256, 512 и т.д.).

---

## CameraGetNearPlane

`real CameraGetNearPlane(real cam);`

Возвращает расстояние до ближней плоскости отсечения камеры (то есть, плоскости, ближе которой ничего не рисуется).

- `cam` - ссылка на камеру.

---

## CameraSetNearPlaneBias

`real CameraSetNearPlaneBias(real cam, real bias);`

Задает коэффициент ближней плоскости отсечения камеры. Напрямую изменить ближнюю плоскость (как это делается для дальней плоскости функцией CameraSetViewDepth) невозможно - она вычисляется по специальной формуле с учетом угла зрения и разрешения вида. С помощью данного коэффициента можно смещать плоскость вперед (значения больше 1) или назад (значения меньше 1).

- `cam` - ссылка на камеру.
- `bias` - коэффициент (значение по умолчанию: 1).

---

## CameraAbsoluteVectorToTarget

`real CameraAbsoluteVectorToTarget(real cam, real ind);`

Возвращает вектор Z в ортономированном базисе камеры, ориентированной на ее объект-цель (иными словами, абсолютное направление от камеры к объекту-цели). Если у камеры нет цели, возвращается ее собственный абсолютный вектор Direction.

- `cam` - ссылка на камеру.
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteRightVectorToTarget

`real CameraAbsoluteRightVectorToTarget(real cam, real ind);`

Возвращает вектор X в ортономированном базисе камеры, ориентированной на ее объект-цель (иными словами, абсолютное направление вправо от камеры относительно ее объекта-цели). Если у камеры нет цели, возвращается ее собственный абсолютный вектор Right.

- `cam` - ссылка на камеру
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteUpVectorToTarget

`real CameraAbsoluteUpVectorToTarget(real cam, real ind);`

Возвращает вектор Y в ортономированном базисе камеры, ориентированной на ее объект-цель (иными словами, абсолютное направление вверх от камеры относительно ее объекта-цели). Если у камеры нет цели, возвращается ее собственный абсолютный вектор Up.

- `cam` - ссылка на камеру
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraZoomAll

`real CameraZoomAll(real cam, real viewer);`

Располагает камеру в пространстве таким образом, чтобы вид, отображающий сцену, охватывал все объекты.

- `cam` - ссылка на камеру
- `viewer` - ссылка на вид.

---

## CameraScreenDeltaToVector

`real CameraScreenDeltaToVector(real cam, real deltax, real deltay, real ratio, real normx, real normy, real normz, real ind);

Вычисляет абсолютный вектор переноса, соответствующий экранному вектору переноса камеры. Полезная функция для реализации элементов пользовательского интерфейса на сцене (3D-манипуляторов).

- `cam` - ссылка на камеру
- `deltax`, `deltay` - целочисленный экранный вектор переноса
- `ratio` - коэфициент, на который умножается экранный вектор
- `normx`, `normy`, `normz` - нормаль плоскости, в которой следует вычислить трехмерный перенос
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraScreenDeltaToVectorXY

`real CameraScreenDeltaToVectorXY(real cam, real deltax, real deltay, real ratio, real ind);`

Функция делает то же, что `CameraScreenDeltaToVector`, но оптимизирована для работы в плоскости XY.

- `cam` - ссылка на камеру
- `deltax`, `deltay` - целочисленный экранный вектор переноса
- `ratio` - коэфициент, на который умножается экранный вектор
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraScreenDeltaToVectorXZ

`real CameraScreenDeltaToVectorXZ(real cam, real deltax, real deltay, real ratio, real ind);`

Функция делает то же, что `CameraScreenDeltaToVector`, но оптимизирована для работы в плоскости XZ.

- `cam` - ссылка на камеру
- `deltax`, `deltay` - целочисленный экранный вектор переноса 
- `ratio` - коэфициент, на который умножается экранный вектор 
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z). 

---

## CameraScreenDeltaToVectorYZ

`real CameraScreenDeltaToVectorYZ(real cam, real deltax, real deltay, real ratio, real ind);`

Функция делает то же, что `CameraScreenDeltaToVector`, но оптимизирована для работы в плоскости YZ.

- `cam` - ссылка на камеру
- `deltax`, `deltay` - целочисленный экранный вектор переноса
- `ratio` - коэфициент, на который умножается экранный вектор
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraAbsoluteEyeSpaceVector

`real CameraAbsoluteEyeSpaceVector(real cam, real fordist, real rightdist, real updist, real ind);`

Вычисляет абсолютный вектор переноса, соответствующий заданному вектору переноса в видовом пространстве камеры.

- `cam` - ссылка на камеру
- `fordist`, `rightdist`, `updist` - перенос вперед, вправо и вверх
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## CameraSetAutoLeveling

`real CameraSetAutoLeveling(real cam, real factor);`

Меняет вектор Direction камеры так, чтобы вектор Up указывал вверх (0, 1, 0).

- `cam` - ссылка на камеру
- `factor` - чем выше это значение, тем медленнее будут корректироваться векторы.

---

## CameraMoveInEyeSpace

`real CameraMoveInEyeSpace(real cam, real fordist, real rightdist, real updist);`

Двигает камеру в ее видовом пространстве.

- `cam` - ссылка на камеру
- `fordist`, `rightdist`, `updist` - перенос вперед, вправо и вверх.

---

## CameraMoveTargetInEyeSpace

`real CameraMoveTargetInEyeSpace(real cam, real fordist, real rightdist, real updist);`

Двигает целевой объект камеры в ее видовом пространстве.

`cam` - ссылка на камеру
- `fordist`, `rightdist`, `updist` - перенос вперед, вправо и вверх.

---

## CameraPointInFront

`real CameraPointInFront(real cam, real x, real y, real z);`

Возвращает истину, если заданная точка находится в поле зрения камеры.

- `cam` - ссылка на камеру
- `x`, `y`, `z` - координаты точки.

---

## CameraGetFieldOfView

`real CameraGetFieldOfView(real cam, real vpdim);`

Возвращает угол поля зрения камеры в градусах.

`cam` - ссылка на камеру
`vpdim` - размерность вида (меньшее из ширины и высоты).
