# FPS

FPS - специальный, полностью автоматизированный метод обнаружения и обработки столкновений, оптимизированный для игр от первого лица. Хотя ничто не мешает использовать его и в играх от третьего лица. Среда FPS чрезвычайно проста в управлении, поэтому ее можно смело рекомендовать в качестве базовой системы столкновений как для начинающих, так и для опытных пользователей Xtreme3D. 

---

## FpsManagerCreate

`real FpsManagerCreate();`

Создает менеджер FPS и возвращает указатель на него.

---

## FpsManagerSetNavigator

`real FpsManagerSetNavigator(real manager, real nav);`

Задает навигатор, который необходимо использовать в среде FPS. Если этого не сделать, перемещение объектов будет невозможным.

- `manager` - указатель на менеджер FPS
- `nav` - указатель на навигатор.

---

## FpsManagerSetMovementScale

`real FpsManagerSetMovementScale(real manager, real scale);`

Задает коэффициент скорости движения. Фактически, этот параметр влияет только на силу гравитации.

- `manager` - указатель на менеджер FPS
- `scale` - коэффициент скорости движения.

---

## FpsManagerAddMap

`real FpsManagerAddMap(real manager, real object);`

Добавляет карту. Карта - это объект свободной формы (freeform), представляющий собой геометрию уровня, используемую для обнаружения столкновений. Эта геометрия может не совпадать с той, что непосредственно видна игроку в виде архитектурных построек и прочих объектов, так как с точки зрения оптимизации гораздо выгоднее представить сложные элементы уровня в качестве простых невидимых боксов.

- `manager` - указатель на менеджер FPS
- `object` - указатель на объект.

---

## FpsManagerRemoveMap

`real FpsManagerRemoveMap(real manager, real object);`

Удаляет заданную карту. На самом деле, карта остается нетронутой, просто она перестает учитываться обработчиком столкновений FPS.

- `manager` - указатель на менеджер FPS
- `object` - указатель на объект.

---

## FpsManagerMapSetCollisionGroup

`real FpsManagerMapSetCollisionGroup(real manager, real object, real group);`

Сведения отсутствуют.

- `object` - указатель на объект
- `group` - cведения отсутствуют.

---

## FpsSetManager

`real FpsSetManager(real object, real manager);`

Задает менеджер FPS для объекта. После этой процедуры объект будет учитываться при проверке столкновений, а его движение будет управляться функциями FPS.

- `object` - указатель на объект
- `manager` - указатель на менеджер FPS.

---

## FpsSetCollisionGroup

`real FpsSetCollisionGroup(real object, real group);`

Сведения отсутствуют.

- `object` - указатель на объект
- `group` - cведения отсутствуют.

---

## FpsSetSphereRadius

`real FpsSetSphereRadius(real object, real radius);`

Задает радиус ограничивающей сферы объекта в среде FPS.

- `object` - указатель на объект
- `radius` - cведения отсутствуют.

---

## FpsSetGravity

`real FpsSetGravity(real object, real mode);`

Включает или выключает гравитацию объекта.

- `object` - указатель на объект
- `mode` - `true` или `false`.

---

## FpsMove

`real FpsMove(real object, real speed);`

Двигает объект в его текущем направлении взгляда с заданной скоростью.

- `object` - указатель на объект
- `speed` - скорость движения.

---

## FpsStrafe

`real FpsStrafe(real object, real speed);`

Двигает объект вбок относительно направления взгляда.

- `object` - указатель на объект
- `speed` - скорость движения.

---

## FpsLift

`real FpsLift(real object, real speed);`

Двигает объект вверх относительно направления взгляда.

- `object` - указатель на объект
- `speed` - скорость движения.

---

## FpsGetVelocity

`real FpsGetVelocity(real object, real ind);`

Возвращает скорость объекта по заданной оси.

- `object` - указатель на объект
- `ind` - индекс оси (1 = X, 2 = Y, 3 = Z).
