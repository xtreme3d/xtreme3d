# Object

Данные функции управляют любыми сценическими объектами, вне зависимости от типа.

---

## ObjectHide

`real ObjectHide(real object);`

Скрывает объект. Скрытые объекты не отрисовываются, однако могут участвовать в алгоритмах обнаружения столкновений.

- `object` - указатель на объект.

---

## ObjectShow

`real ObjectShow(real object);`

Показывает объект.

- `object` - указатель на объект.

---

## ObjectIsVisible

`boolean ObjectIsVisible(real object);`

Возвращает истину (1), если объект видимый, и ложь (0), если скрыт.

- `object` - указатель на объект.

----

## ObjectCopy

`real ObjectCopy(real object, real parent);`

Копирует объект, наследуя все его настройки, включая трансформацию в пространстве, материал и т. д.

- `object` - указатель на объект-оригинал
- `parent` - указатель на родителя для объекта-копии.

---

## ObjectDestroy

`real ObjectDestroy(real object);`

Уничтожает объект. Безопасно уничтожить можно только те объекты, у которых нет потомков. Кроме того, у некоторых типов объектов есть собственные функции уничтожения - в этом случае лучше использовать именно их. 

- `object` - указатель на объект.

---

## ObjectDestroyChildren

`real ObjectDestroyChildren(real object);`

Уничтожает потомков объекта.

- `object` - указатель на объект.

---

## ObjectSetPosition

`real ObjectSetPosition(real object, real x, real y, real z);`

Изменяет локальные координаты положения объекта в пространстве.

- `object` - указатель на объект
- `x`, `y`, `z` - координаты по трем осям.

---

## ObjectGetPosition

`real ObjectGetPosition(real object, real ind);`

Возвращает локальные координаты положения объекта в пространстве.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetAbsolutePosition

`real ObjectGetAbsolutePosition(real object, real ind);`

Возвращает абсолютные координаты положения объекта в пространстве.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectSetPositionOfObject

`real ObjectSetPositionOfObject(real object1, real object2);`

Синхронизирует абсолютные координаты объекта 1 с абсолютными координатами объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectAlignWithObject

`real ObjectAlignWithObject(real object1, real object2);`

Синхронизирует абсолютный поворот объекта 1 с абсолютным поворотом объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectSetPositionX

`real ObjectSetPositionX(real object, real x);`

Изменяет локальную координату положения объекта в пространстве по оси X.

- `object` - указатель на объект
- `x` - координата по оси X.

---

## ObjectSetPositionY

`real ObjectSetPositionY(real object, real y);`

Изменяет локальную координату положения объекта в пространстве по оси Y.

- `object` - указатель на объект
- `y` - координата по оси Y.

---

## ObjectSetPositionZ

`real ObjectSetPositionZ(real object, real z);`

Изменяет локальную координату положения объекта в пространстве по оси Z.

- `object` - указатель на объект
- `z` - координата по оси Z.

---

## ObjectGetPositionX

`real ObjectGetPositionX(real object);`

Возвращает локальную координату положения объекта в пространстве по оси X.

- `object` - указатель на объект.

---

## ObjectGetPositionY

`real ObjectGetPositionY(real object);`

Возвращает локальную координату положения объекта в пространстве по оси Y.

- `object` - указатель на объект.

---

## ObjectGetPositionZ

`real ObjectGetPositionZ(real object);`

Возвращает локальную координату положения объекта в пространстве по оси Z.

- `object` - указатель на объект.

---

## ObjectSetAbsolutePosition

`real ObjectSetAbsolutePosition(real object, real x, real y, real z);`

Изменяет абсолютные координаты положения объекта в пространстве.

- `object` - указатель на объект
- `x`, `y`, `z` - координаты по трем осям.

---

## ObjectSetDirection

`real ObjectSetDirection(real object, real x, real y, real z);`

Изменяет единичный вектор локального направления объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор.

---

## ObjectGetDirection

`real ObjectGetDirection(real object, real ind);`

Возвращает единичный вектор локального направления объекта.

- `object` - указатель на объект
- `ind` - индекс оси (0 = X, 1 = Y, 2 = Z).

---

## ObjectSetAbsoluteDirection

`real ObjectSetAbsoluteDirection(real object, real x, real y, real z);`

Изменяет единичный вектор абсолютного направления объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор.

---

## ObjectGetAbsoluteDirection

`real ObjectGetAbsoluteDirection(real object, real ind);`

Возвращает единичный вектор абсолютного направления объекта.

- `object` - указатель на объект
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetPitch

`real ObjectGetPitch(real object);`

Возвращает угол локального поворота объекта по оси X.

- `object` - указатель на объект.

---

## ObjectGetTurn

`real ObjectGetTurn(real object);`

Возвращает угол локального поворота объекта по оси Y.

- `object` - указатель на объект.

---

## ObjectGetRoll

`real ObjectGetRoll(real object);`

Возвращает угол локального поворота объекта по оси Z.

- `object` - указатель на объект.

---

## ObjectSetRotation

`real ObjectSetRotation(real object, real x, real y, real z);`

Изменяет локальный угол поворота объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - угол поворота по трем осям.

---

## ObjectMove

`real ObjectMove(real object, real speed);`

Двигает объект в направлении вектора Direction с заданной скоростью.

- `object` - указатель на объект
- `speed` - линейная скорость.

---

## ObjectLift

`real ObjectLift(real object, real speed);`

Двигает объект в направлении вектора Up с заданной скоростью.

- `object` - указатель на объект
- `speed` - линейная скорость.

---

## ObjectTranslate

`real ObjectTranslate(real object, real x, real y, real z);`

Перемещает объект по заданному вектору.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор.

---

## ObjectStrafe

`real ObjectStrafe(real object, real speed);`

Двигает объект в направлении вектора Left с заданной скоростью.

- `object` - указатель на объект
- `speed` - линейная скорость.

---

## ObjectRotate

`real ObjectRotate(real object, real x, real y, real z);`

Поворачивает объект локально на заданный угол по трем осям.

- `object` - указатель на объект
- `x`, `y`, `z` - углы по трем осям.

---

## ObjectScale

`real ObjectScale(real object, real x, real y, real z);`

Изменяет локальный масштаб объекта относительно текущего.

- `object` - указатель на объект
- `x`, `y`, `z` - масштаб по трем осям.

---

## ObjectSetScale

`real ObjectSetScale(real object, real x, real y, real z);`

Задает локальный масштаб объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - масштаб по трем осям.

---

## ObjectGetScale

`real ObjectGetScale(real object, real ind);`

Возвращает локальный масштаб объекта.

- `object` - указатель на объект
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---

## ObjectSetUpVector

`real ObjectSetUpVector(real object, real x, real y, real z);`

Задает единичный вектор Up объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор.

---

## ObjectPointToObject

`real ObjectPointToObject(real object1, real object2);`

Направляет объект 1 в сторону объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectShowAxes

`real ObjectShowAxes(real object, real mode);`

Включает или выключает отображение локальных осей объекта. Оси представлены бесконечными пунктирными линиями трех цветов: красного, зеленого, синего (соблюдается соответствие RGB - XYZ).

- `object` - указатель на объект
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ObjectGetGroundHeight

`real ObjectGetGroundHeight(real object, real target);`

Возвращает Y-координату наиболее высокой точки на поверхности целевого объекта (или его потомков) на абсолютной позиции XZ заданного объекта. Иными словами, высоту "земли" под объектом.

- `object` - указатель на объект
- `target` - целевой объект. В вычислениях также участвуют все потомки объекта, имеющие видимую геометрию.

---

## ObjectSceneRaycast

`real ObjectSceneRaycast(real object, real target);`

Выпускает луч вдоль абсолютного вектора Direction объекта и проверяет целевой объект и всех его потомков на факт пересечения с этим лучом. Возвращает id первого пересеченного объекта.

- `object` - указатель на объект
- `target` - целевой объект. В вычислениях также участвуют все потомки объекта, имеющие видимую геометрию.

---

## ObjectRaycast

`real ObjectRaycast(real object, real target);`

Выпускает луч вдоль абсолютного вектора Direction объекта и проверяет целевой объект на факт пересечения с этим лучом. Возвращает истину, если обнаружено пересечение, и ложь в противном случае.

- `object` - указатель на объект
- `target` - целевой объект.

---

## ObjectSetMaterial

`real ObjectSetMaterial(real object, string material);`

Применяет к объекту материал.

- `object` - указатель на объект
- `material` - имя материала.

---

## ObjectGetMaterial

`string ObjectGetMaterial(real object);`

Возвращает материал объекта.

- `object` - указатель на объект.

---

## ObjectGetDistance

`real ObjectGetDistance(real object1, real object2);`

Возвращает расстояние между двумя объектами.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectCheckCubeVsCube

`real ObjectCheckCubeVsCube(real object1, real object2);`

Возвращает истину при обнаружении столкновения между ориентированными ограничивающими параллелепипедами (OBB) двух объектов.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectCheckSphereVsSphere

`real ObjectCheckSphereVsSphere(real object1, real object2);`

Возвращает истину при обнаружении столкновения между ограничивающими сферами двух объектов.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectCheckSphereVsCube

`real ObjectCheckSphereVsCube(real object1, real object2);`

Возвращает истину при обнаружении столкновения между ограничивающей сферой объекта 1 и ориентированным ограничивающим параллелепипедом объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectCheckCubeVsFace

`real ObjectCheckCubeVsFace(real object1, real object2);`

Возвращает истину при обнаружении столкновения между ориентированным ограничивающим параллелепипедом (OBB) объекта 1 и геометрией объекта 2. Объект 2 должен быть типа freeform.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectCheckFaceVsFace

`real ObjectCheckFaceVsFace(real object1, real object2);`

Возвращает истину при обнаружении столкновения между геометрией двух объектов. Объекты должны быть типа freeform.

Эта проверка довольно медленная, поэтому рекомендуем оптимизировать ее использование - например, осуществлять точную проверку между геометриями объектов только в том случае, если обнаружено столкновение между их ограничивающими сферами.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectIsPointInObject

`real ObjectIsPointInObject(real object, real x, real y, real z);`

Возвращает истину, если заданная точка находится внутри объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - координаты точки.

---

## ObjectSetCulling

`real ObjectSetCulling(real object, real vc);`

Задает режим отбора видимости объекта.

- `object` - указатель на объект
- `vc` - режим отбора видимости. Доступны следующие значения `vc`:
    - `vcNone` = 0 - отсутствие отбора видимости;
    - `vcInherited` = 1 - отбор видимости наследуется от родителя объекта (значение по умолчанию);
    - `vcObjectBased` = 2 - объект может быть отобран в зависимости от своей позиции. Отбор объекта не затрагивает видимость его дочерних объектов;
    - `vcHierarchical` = 3 - отбор осуществляется по принципу иерархии: если материнский объект отобран, все его дочерние объекты также будут невидимы.

---

## ObjectSetName

`real ObjectSetName(real object, string name);`

Задает имя объекта.

- `object` - указатель на объект
- `name` - имя.

---

## ObjectGetName

`string ObjectGetName(real object);`

Возвращает имя объекта.

- `object` - указатель на объект.

---

## ObjectGetClassName

`string ObjectGetclassName(real object);`

Возвращает класс объекта.

- `object` - указатель на объект.

---

## ObjectSetTag

`real ObjectSetTag(real object, real tag);`

Задает метку объекта.

- `object` - указатель на объект
- `tag` - метка.

---

## ObjectGetTag

`real ObjectGetTag(real object);`

Возвращает метку объекта.

- `object` - указатель на объект.

---

## ObjectGetParent

`real ObjectGetParent(real object);`

Возвращает указатель на родителя объекта.

- `object` - указатель на объект.

---

## ObjectGetChildCount

`real ObjectGetChildCount(real object);`

Возвращает количество потомков объекта.

- `object` - указатель на объект.

---

## ObjectGetChild

`real ObjectGetChild(real object, real ind);`

Возвращает id потомка объекта заданного номера.

- `object` - указатель на объект
- `ind` - номер потомка.

---

## ObjectGetIndex

`real ObjectGetIndex(real object);`

Возвращает номер объекта в списке потомков его родителя (начиная с 0), или -1, если у объекта нет родителя.

- `object` - указатель на объект.

---

## ObjectFindChild

`real ObjectFindChild(real object, string name);`

Возвращает указатель на потомка объекта с заданным именем.

- `object` - указатель на объект
- `name` - имя потомка.

---

## ObjectFindByName

`real ObjectFindByName(string name);`

Возвращает id объекта с заданным именем.

- `name` - имя потомка.

---

## ObjectGetBoundingSphereRadius

`real ObjectGetBoundingSphereRadius(real object);`

Возвращает радиус ограничивающей сферы объекта.

- `object` - указатель на объект.

---

## ObjectGetAbsoluteUp

`real ObjectGetAbsoluteUp(real object, real ind);`

Возвращает абсолютный единичный вектор Up объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectSetAbsoluteUp

`real ObjectSetAbsoluteUp(real object, real x, real y, real z);`

Изменяет абсолютный единичный вектор Up объекта.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор.

---

## ObjectGetAbsoluteRight

`real ObjectGetAbsoluteRight(real object, real ind);`

Возвращает абсолютный единичный вектор Right объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetAbsoluteXVector

`real ObjectGetAbsoluteXVector(real object, real ind);`

Возвращает абсолютный вектор X, выраженный в локальных координатах объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetAbsoluteYVector

`real ObjectGetAbsoluteYVector(real object, real ind);`

Возвращает абсолютный вектор Y, выраженный в локальных координатах объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetAbsoluteZVector

`real ObjectGetAbsoluteZVector(real object, real ind);`

Возвращает абсолютный вектор Z, выраженный в локальных координатах объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectGetRight

`real ObjectGetRight(real object, real ind);`

Возвращает локальный единичный вектор Right объекта.

- `object` - указатель на объект
- `ind` - индекс оси (0 = X, 1 = Y, 2 = Z).

---

## ObjectMoveChildUp

`real ObjectMoveChildUp(real object, real ind);`

Сдвигает потомка объекта вверх в списке.

- `object` - указатель на объект
- `ind` - индекс потомка.

---

## ObjectMoveChildDown

`real ObjectMoveChildDown(real object, real ind);`

Сдвигает потомка объекта вниз в списке.

- `object` - указатель на объект
- `ind` - индекс потомка.

---

## ObjectSetParent

`real ObjectSetParent(real object, real parent);`

Делает объект потомком другого объекта.

- `object` - указатель на объект
- `parent` - указатель на родителя.

---

## ObjectRemoveChild

`real ObjectRemoveChild(real object, real child, real keepchildren);`

Удаляет потомка объекта.

- `object` - указатель на объект
- `child` - указатель на потомка
- `keepchildren` - определяет, нужно ли скопировать потомков удаляемого объекта его родителю (в противном случае потомки также будут удалены).

---

## ObjectMoveObjectAround

`real ObjectMoveObjectAround(real object1, real object2, real x, real y);`

Поворачивает объект 1 вокруг объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2
- `x`, `y` - углы поворота по осям X и Y.

---

## ObjectPitch

`real ObjectPitch(real object, real angle);`

Поворачивает объект локально по оси Х.

- `object` - указатель на объект
- `angle` - угол поворота.

---

## ObjectTurn

`real ObjectTurn(real object, real angle);`

Поворачивает объект локально по оси Y.

- `object` - указатель на объект
- `angle` - угол поворота.

---

## ObjectRoll

`real ObjectRoll(real object1, real angle);`

Поворачивает объект локально по оси Z.

- `object` - указатель на объект
- `angle` - угол поворота.

---

## ObjectGetUp

`real ObjectGetUp(real object, real ind);`

Возвращает локальный единичный вектор Up объекта.

- `object` - указатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## ObjectRotateAbsolute

`real ObjectRotateAbsolute(real object, real x, real y, real z);`

Поворачивает объект абсолютно на заданный угол по трем осям.

- `object` - указатель на объект
- `x`, `y`, `z` - углы по трем осям.

---

## ObjectRotateAbsoluteVector

`real ObjectRotateAbsoluteVector(real object, real x, real y, real z, real angle);`

Поворачивает объект абсолютно на заданный угол вокруг заданной оси.

- `object` - указатель на объект
- `x`, `y`, `z` - вектор оси поворота
- `angle` - угол поворота.

---

## ObjectInFrustrum

`real ObjectInFrustrum(real object, real viewer);`

Возвращает 1 (`true`), если объект находится в поле зрения (попадает в пирамиду видимости) заданного вида, и 0 (`false`) в противном случае.

- `object` - указатель на объект
- `viewer` - указатель вид.

---

## ObjectSetMatrixColumn

`real ObjectSetMatrixColumn(real object, real ind, real x, real y, real z, real w);`

Заполняет столбец локальной матрицы трансформации объекта. Вызвав эту функцию для каждого из 4 столбцов, можно задать произвольную матрицу трансформации.

- `object` - указатель на объект
- `ind` - индекс столбца (0, 1, 2 или 3)
- `x`, `y`, `z`, `w` - значения элементов столбца.

---

## ObjectExportMatrix

`real ObjectExportMatrix(real object1, real object2);`

Копирует локальную матрицу трансформации объекта 1 в локальную матрицу трансформации объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectExportAbsoluteMatrix

`real ObjectExportAbsoluteMatrix(real object1, real object2);`

Копирует абсолютную матрицу трансформации объекта 1 в абсолютную матрицу трансформации объекта 2.

- `object1` - указатель на объект 1
- `object2` - указатель на объект 2.

---

## ObjectIgnoreDepthBuffer

`real ObjectIgnoreDepthBuffer(real object, real mode);`

При включении этой опции объект не учитывает буфер глубины при рендеринге - то есть, всегда рендерится поверх предыдущих данных в буфере кадра.

- `object` - указатель на объект
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ObjectIsPicked

`real ObjectIsPicked(real object, real viewer, real x, real y);`

Возвращает 1, если точка по заданным координатам принадлежит объекту. Функция аналогична ViewerGetPickedObject, но, можно сказать, выполняет обратную задачу.

- `object` - указатель на объект
- `viewer` - указатель на вид
- `x`, `y` - координаты точки.

---

## ObjectNotifyChange

`real ObjectNotifyChange(real object);`

Уведомляет объект о том, что его свойства или структура изменились.

- `object` - указатель на объект.

---

## ObjectStructureChanged

`real ObjectStructureChanged(real object);`

Помечает объект как объект с измененной структурой потомков. Это необходимо для того, чтобы перестроить список отображения в некоторых особых сценариях рендеринга - например, при использовании `DummycubeAmalgamate`.

- `object` - указатель на объект.

---

## ObjectClearStructureChanged

`real ObjectClearStructureChanged(real object);`

Удаляет пометку об измененной структуре, ранее добавленную функцией `ObjectStructureChanged`.

- `object` - указатель на объект.

---
