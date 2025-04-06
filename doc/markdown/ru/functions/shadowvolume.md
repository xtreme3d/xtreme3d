# Shadowvolume

Объект shadowvolume позволяет строить настоящие объемные тени в реальном времени! В отличие от shadowplane, тени здесь не ограничены плоскостью, они могут падать на любые объекты. Этот метод построения теней заключается в создании объекта, определяющего объем, внутри которого точки находятся в тени. К сожалению, он чрезвычайно ресурсоемкий и может привести к колоссальному снижению производительности. Поэтому рекомендуется применять shadowvolume на небольшом количестве объектов.

---

## ShadowvolumeCreate

`real ShadowvolumeCreate(real parent);`

Создает новый объект shadowvolume и возвращает указатель на него. Объекты, на которые должна падать тень, необходимо сделать потомками shadowvolume.

- `parent` - указатель на родителя.

---

## ShadowvolumeSetActive

`real ShadowvolumeSetActive(real sv, real mode);`

Включает или выключает тени. В выключенном состоянии shadowvolume не потребляет вычислительных ресурсов, поэтому стоит выключать его, когда тени не попадают в поле обзора.

- `sv` - указатель на объект shadowvolume
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ShadowvolumeAddLight

`real ShadowvolumeAddLight(real sv, real light);`

Добавляет источник света, от которого падают тени. Вы можете добавить несколько источников, но это повлияет на производительность.

- `sv` - указатель на объект shadowvolume
- `light` - указатель на источник света.

---

## ShadowvolumeRemoveLight

`real ShadowvolumeRemoveLight(real sv, real light);`

Удаляет источник света из shadowvolume.

- `sv` - указатель на объект shadowvolume
- `light` - указатель на источник света.

---

## ShadowvolumeAddOccluder

`real ShadowvolumeAddOccluder(real sv, real object);`

Добавляет объект, который должен отбрасывать тень. Объект может одновременно и отбрасывать, и принимать тень.

- `sv` - указатель на объект shadowvolume
- `object` - указаль на объект.

---

## ShadowvolumeRemoveOccluder

`real ShadowvolumeRemoveOccluder(real sv, real object);`

Удаляет объект из shadowvolume.

- `sv` - указатель на объект shadowvolume
- `object` - указаль на объект.

---

##ShadowvolumeSetDarkeningColor

`real ShadowvolumeSetDarkeningColor(real sv, real color, real alpha);`

Задает цвет и прозрачность теней.

- `sv` - указатель на объект shadowvolume
- `color`, `alpha` - цвет и прозрачность.

---

## ShadowvolumeSetMode

`real ShadowvolumeSetMode(real sv, real svm);`

Задает режим теней.

- `sv` - указатель на объект shadowvolume
- `svm` - режим теней.
    - `svmAccurate` = 0 - режим по умолчанию
    - `svmDarkening` = 1 - в этом режиме работает цвет и прозрачность теней
    - `svmOff` = 2 - тени отключены.

---

## ShadowvolumeSetOptions

`real ShadowvolumeSetOptions(real sv, real showvolumes, real cachesilhouettes, real scissorclips, real worldscissorclip);`

Задает опции shadowvolume.

- `sv` - указатель на объект shadowvolume
- `showvolumes` - отображение теневых объемов: `true` или `false` (1 и 0 соответственно). Значение по умолчанию - 0
- `cachesilhouettes` - включает или выключает кэширование силуэтов: `true` или `false` (1 и 0 соответственно). Кэширование может повысить производительность, когда объекты неподвижны относительно теней. Значение по умолчанию - 1.
- `scissorclips` - включает или выключает обрезку теней внутри экранной области, соответствующей области влияния источника света: `true` или `false` (1 и 0 соответственно). Эта обрезка повышает производительность, если источник света имеет ограниченную область влияния, и слишком далекие от него тени не должны рендериться. Значение по умолчанию - 1.
- `worldscissorclip` - включает или выключает обрезку теней внутри экранной области, соответствующей объекту, на который падает тень: `true` или `false` (1 и 0 соответственно). Значение по умолчанию - 0.
