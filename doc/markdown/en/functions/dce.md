# Dynamic Collision Engine (DCE)

DCE - очень мощный движок обнаружения и обработки столкновений, достаточно универсальный, чтобы его можно было применять в играх любого жанра. Вам для этого совершенно не потребуется разбираться в особенностях и технологиях обнаружения столкновений - движок очень прост в использовании. Нужно всего лишь назначить объекты, которые должны сталкиваться, а об остальном позаботится DCE. Вам же остается наслаждаться результатом!

Объекты в DCE делятся на два типа - динамические и статические. Динамические могут перемещаться в пространстве, и это учитывается при проверке столкновений. Это может быть, например, персонаж, которым управляет игрок. Статические объекты не перемещаются и используются, например, как стены, пол, потолок, колонны и прочие препятствия.

После привязки объектов в DCE, их движением нельзя управлять стандартными функциями трансформации. Для этого используются собственные функции DCE.

Объекты сталкиваются согласно форме и размеру их коллайдеров - невидимых геометрических тел, которыми они представлены в логической среде DCE. Иными словами, сталкиваются не сами объекты, а эти тела. Чаще всего коллайдеры представляют собой параллелепипеды и сферы.

В DCE также существует такое важное понятие, как слой (layer). Это свойство разграничивает свободу взаимодействий объектов. Сталкиваться друг с другом могут только объекты одного слоя. Например, если объект 1 находится на слое 4, а объект 2 - на слое 5, то столкновения между ними не обнаруживается.

По умолчанию все объекты создаются на нулевом слое. Объекты нулевого слоя всегда сталкиваются с объектами других слоев.

Вы можете использовать слои для того, чтобы разбить игровое пространство на отдельные области, в которых действуют объекты, и тем самым повысить производительность. А можно найти и другие применения.

Еще одна интересная особенность DCE в том, что динамические объекты имеют два кинематических состояния - локальное и абсолютное. Иными словами, такие параметры, как скорость, ускорение и импульс могут быть заданы как в локальных координатах объекта, так и в глобальных. Эти параметры независимы друг от друга, но оба учитываются одновременно при расчете движения. Локальные параметры удобны, когда вы управляете объектом, который может вращаться и двигается в ту сторону, куда "смотрит" - а такая ситуация наблюдается в абсолютном большинстве игр. При этом абсолютные параметры удобны для воздействия на объекты какой-либо внешней силой - например, если персонажа отбрасывает взрывом или сносит ветром (поскольку скорости суммируются, персонаж будет двигаться быстрее, если идет по ветру - примерно как и в реальной жизни).

---

## DceManagerCreate

`real DceManagerCreate();`

Создает менеджер DCE и возвращает указатель на него.

---

## DceManagerStep

`real DceManagerStep(real manager, real delta);`

Совершает ручное обновление менеджера. Используется только если ручное обновление включено функцией `DceManagerSetManualStep`. В ином случае обновление происходит автоматически.

- `manager` - указатель на менеджер DCE
- `delta` - шаг времени.

---

## DceManagerSetGravity

`real DceManagerSetGravity(real manager, real grav);`

Задает величину гравитации (силу, с которой динамические объекты будут притягиваться к земле).

- `manager` - указатель на менеджер DCE
- `grav` - гравитация.

---

## DceManagerSetWorldDirection

`real DceManagerSetWorldDirection(real manager, real x, real y, real z);`

Сведения отсутствуют.

- `manager` - указатель на менеджер DCE
- `x`, `y`, `z` - вектор.

---

## DceManagerSetWorldScale

`real DceManagerSetWorldScale(real manager, real scale);`

Сведения отсутствуют.

- `manager` - указатель на менеджер DCE
- `scale` - масштаб.

---

## DceManagerSetMovementScale

`real DceManagerSetMovementScale(real manager, real scale);`

Сведения отсутствуют.

- `manager` - указатель на менеджер DCE
- `scale` - масштаб.

---

## DceManagerSetLayers

`real DceManagerSetLayers(real manager, real ccs);`

Сведения отсутствуют.

- `manager` - указатель на менеджер DCE
- `ccs` - сведения отсутствуют.
    - `ccsDCEStandard` = 0 - сведения отсутствуют
    - `ccsCollisionStandard` = 1 - сведения отсутствуют
    - `ccsHybrid` = 2 - сведения отсутствуют.

---

## DceManagerSetManualStep

`real DceManagerSetManualStep(real manager, real mode);`

Включает или выключает ручной режим обновления. При включенном ручном обновлении необходимо использовать функцию `DceManagerStep`.

- `manager` - указатель на менеджер DCE
- `mode` - `true` или `false`.

---

## DceDynamicSetManager

`real DceDynamicSetManager(real object, real manager);`

Определяет объект в среде DCE как динамический, прикрепляя его к заданному менеджеру, и возвращает указатель на него.

- `object` - укaзатель на объект
- `manager` - указатель на менеджер DCE.

---

## DceDynamicSetActive

`real DceDynamicSetActive(real object, real mode);`

Включает или выключает динамический объект. Выключенные объекты не учитываются при проверке столкновений и, следовательно, на них не затрачиваются вычислительные ресурсы.

- `object` - укaзатель на объект
- `mode` - `true` или `false`.

---

## DceDynamicIsActive

`real DceDynamicIsActive(real object);`

Возвращает 1 (`true`), если объект включен, и 0 (`false`), если выключен.

- `object` - укaзатель на объект.

---

## DceDynamicSetUseGravity

`real DceDynamicSetUseGravity(real object, real mode);`

Включает или выключает гравитацию для динамического объекта.

- `object` - укaзатель на объект
- `mode` - `true` или `false`.

---

## DceDynamicSetLayer

`real DceDynamicSetLayer(real object, real layer);`

Задает слой динамического объекта.

- `object` - укaзатель на объект
- `layer` - номер слоя.

---

## DceDynamicGetLayer

`real DceDynamicGetLayer(real object);`

Возвращает номер слоя динамического объекта.

- `object` - укaзатель на объект.

---

## DceDynamicSetSolid

`real DceDynamicSetSolid(real object, real mode);`

Включает или выключает твердость динамического объекта. Если плотность включена, объект будет твердым по отношению к другим динамическим объектам.

- `object` - укaзатель на объект
- `mode` - `true` или `false`.

---

## DceDynamicSetFriction

`real DceDynamicSetFriction(real object, real friction);`

Задает коэффициент трения для динамического объекта.

- `object` - укaзатель на объект
- `friction` - коэффициент трения.

---

## DceDynamicSetBounce

`real DceDynamicSetBounce(real object, real bounce);`

Задает параметр упругости для динамического объекта.

- `object` - укaзатель на объект
- `bounce` - параметр упругости (значение в диапазоне от 0 до 1).

---

## DceDynamicSetSize

`real DceDynamicSetSize(real object, real x, real y, real z);`

Задает размер коллайдера динамического объекта. Коллайдер динамических объектов всегда имеет форму эллипсоида (csEllipsoid) - таким образом, эта функция задает радиусы эллипсоида по трем осям.

- `object` - укaзатель на объект
- `x`, `y`, `z` - размер коллайдера по трем осям.

---

## DceDynamicSetSlideOrBounce

`real DceDynamicSetSlideOrBounce(real object, real csb);`

Задает тип поведения динамического объекта при столкновениях - скольжение или отталкивание.

- `object` - укaзатель на объект
- `csb` - скольжение или отталкивание. Поддерживаются следующие значения `cbs`:
    - `csbSlide` = 0 - скольжение
    - `csbBounce` = 1 - отталкивание.

---

## DceDynamicApplyAcceleration

`real DceDynamicApplyAcceleration(real object, real x, real y, real z);`

Придает динамическому объекту локальное ускорение (выраженное в локальной координатной системе).

- `object` - укaзатель на объект
- `x`, `y`, `z` - вектор ускорения.

---

## DceDynamicApplyAbsAcceleration

`real DceDynamicApplyAbsAcceleration(real object, real x, real y, real z);`

Придает динамическому объекту абсолютное ускорение.

- `object` - укaзатель на объект
- `x`, `y`, `z` - вектор ускорения.

---

## DceDynamicStopAcceleration

`real DceDynamicStopAcceleration(real object);`

Останавливает локальное ускорение динамического объекта.

- `object` - укaзатель на объект.

---

## DceDynamicStopAbsAcceleration

`real DceDynamicStopAbsAcceleration(real object);`

Останавливает абсолютное ускорение динамического объекта.

- `object` - укaзатель на объект.

---

## DceDynamicJump

`real DceDynamicJump(real object, real height, real speed);`

Совершает прыжок динамического объекта.

- `object` - укaзатель на объект
- `height` - максимальная высота прыжка
- `speed` - скорость прыжка.

---

## DceDynamicMove

`real DceDynamicMove(real object, real x, real y, real z, real delta);`

Двигает динамический объект. Обратите внимание, что движение объекта этой функцией не подчиняется гравитации.

- `object` - укaзатель на объект
- `x`, `y`, `z` - вектор движения
- `delta` - сведения отсутствуют.

---

## DceDynamicMoveTo

`real DceDynamicMoveTo(real object, real x, real y, real z, real amount);`

Двигает динамический объект в направлении к указанной точке.

- `object` - укaзатель на объект
- `x`, `y`, `z` - абсолютные координаты точки
- `amount - скорость движения.

---

## DceDynamicSetVelocity

`real DceDynamicSetVelocity(real object, real x, real y, real z);`

Задает локальную скорость динамического объекта (выраженную в локальной координатной системе).

- `object` - укaзатель на объект
- `x`, `y`, `z` - вектор скорости.

---

## DceDynamicGetVelocity

`real DceDynamicGetVelocity(real object, real ind);`

Возвращает локальную скорость динамического объекта.

- `object` - укaзатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## DceDynamicSetAbsVelocity

`real DceDynamicSetAbsVelocity(real object, real x, real y, real z);`

Задает абсолютную скорость динамического объекта (выраженную в глобальной координатной системе).

- `object` - укaзатель на объект
- `x`, `y`, `z` - вектор скорости.

---

## DceDynamicGetAbsVelocity

`real DceDynamicGetAbsVelocity(real object, real ind);`

Возвращает абсолютную скорость динамического объекта.

- `object` - укaзатель на объект
- `ind` - индекс координаты (0 = X, 1 = Y, 2 = Z).

---

## DceDynamicApplyImpulse

`real DceDynamicApplyImpulse(real object, real x, real y, real z);`

Придает динамическому объекту локальный импульс (моментальное изменение локальной скорости).

- `object` - укaзатель на объект
- `x`, `y`, `z` - импульс. Суммируется с текущей локальной скоростью.

---

## DceDynamicApplyAbsImpulse

`real DceDynamicApplyAbsImpulse(real object, real x, real y, real z);`

Придает динамическому объекту абсолютный импульс (моментальное изменение абсолютной скорости).

- `object` - укaзатель на объект
- `x`, `y`, `z` - импульс. Суммируется с текущей абсолютной скоростью.

---

## DceDynamicInGround

`real DceDynamicInGround(real object);`

Возвращает истину, если динамический объект "стоит на земле" - то есть, находится в контакте с поверхностью под ним.

- `object` - укaзатель на объект.

---

## DceDynamicSetMaxRecursionDepth

`real DceDynamicSetMaxRecursionDepth(real object, real depth);`

Сведения отсутствуют.

- `object` - укaзатель на объект
- `depth` - сведения отсутствуют.

---

## DceStaticSetManager

`real DceStaticSetManager(real object, real manager);`

Определяет объект в среде DCE как статический, прикрепляя его к заданному менеджеру, и возвращает указатель на него.

- `object` - укaзатель на объект
- `manager` - указатель на менеджер DCE.

---

## DceStaticSetActive

`real DceStaticSetActive(real object, real mode);`

Включает или выключает статический объект. Выключенные объекты не учитываются при проверке столкновений и, следовательно, на них не затрачиваются вычислительные ресурсы.

- `object` - укaзатель на объект
- `mode` - `true` или `false`.

---

## DceStaticSetShape

`real DceStaticSetShape(real object, real cs);`

Задает форму коллайдера статического объекта.

- `object` - укaзатель на объект
- `cs` - форма коллайдера. Поддерживаются следующие значения `cs`:
    - `csEllipsoid` = 0 - эллипсоид
    - `csBox` = 1 - прямоугольный параллелепипед
    - `csFreeform` = 2 - объект свободной формы
    - `csTerrain` = 3 - ландшафт.

---

## DceStaticSetLayer

`real DceSetLayer(real object, real layer);`

Задает слой статического объекта.

- `object` - укaзатель на объект
- `layer` - номер слоя.

---

## DceStaticSetSize

`real DceStaticSetSize(real object, real x, real y, real z);`

Задает размер коллайдера статического объекта.

- `object` - укaзатель на объект
- `x`, `y`, `z` - размер коллайдера по трем осям.

---

## DceStaticSetSolid

`real DceStaticSetSolid(real object, real mode);`

Включает или выключает твердость статического объекта. Если плотность включена, объект будет твердым по отношению к динамическим объектам.

- `object` - укaзатель на объект
- `mode` - `true` или `false`.

---

## DceStaticSetFriction

`real DceStaticSetFriction(real object, real friction);`

Задает коэффициент трения для статического объекта.

- `object` - укaзатель на объект
- `friction` - коэффициент трения.

---

## DceStaticSetBounceFactor

`real DceStaticSetBounceFactor(real object, real bounce);`

Задает параметр упругости для статического объекта.

- `object` - укaзатель на объект
- `bounce` - параметр упругости (значение в диапазоне от 0 до 1).
