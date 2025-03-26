# Primitives

Примитивы (Primitive) - простейшие геометрические тела, которые генерируются движком. К примитивам относятся плоскость (plane), куб (cube), сфера (sphere), цилиндр (cylinder), конус (cone), полый цилиндр (annulus), тор (torus), диск (disk), усеченная пирамида (frustrum), додекаэдр (dodecahedron), икосаэдр (icosahedron) и чайник Юта (teapot).

---

## PlaneCreate

`real PlaneCreate(real singlequad, real width, real height, real xtiles, real ytiles, real parent);`

Создает плоскость и возвращает указатель на нее.

- `singlequad` - определяет, разбивать ли плоскость на сетку, или представить ее одним квадратом - `false` или `true` (0 и 1 соответственно). При использовании освещения, как правило, лучше разбить плоскость
- `width`, `height` - ширина и высота плоскости
- `xtiles`, `ytiles` - при `singlequad` = 0 задают количество ячеек сетки по горизонтали и вертикали
- `parent` - указатель на родителя для плоскости (0 - отсутствие родителя).

---

## PlaneSetOptions

`real PlaneSetOptions(real plane, real singlequad, real xtiles, real ytiles);`

Задает параметры плоскости.

- `plane` - указатель на плоскость
- `singlequad` - определяет, разбивать ли плоскость на сетку, или представить ее одним квадратом - false или true (0 и 1 соответственно). При использовании освещения, как правило, лучше разбить плоскость
- `xtiles`, `ytiles` - при singlequad = 0 задают количество ячеек сетки по горизонтали и вертикали.

---

## PlaneGetOptions

`real PlaneGetOptions(real plane, real ind);`

Возвращает параметры плоскости.

- `plane` - указатель на плоскость
- `ind` - индекс параметра (0 = `xtiles`, 1 = `ytiles`).

---

## CubeCreate

`real CubeCreate(real width, real height, real depth, real parent);`

Создает куб (строго говоря, прямоугольный параллелепипед) и возвращает указатель на него.

- `width`, `height`, `depth` - ширина, высота и долгота куба
- `parent` - указатель на родителя для куба (0 - отсутствие родителя).

---

## CubeSetNormalDirection

`real CubeSetNormalDirection(real cube, real nd);`

Задает направление нормалей куба.

- `cube` - указатель на куб
- `nd` - направление нормалей. Доступны следующие значения `nd`:
    - `ndOutside` = 0 - нормали направлены внутрь (значение по умолчанию)
    - `ndInside` = 1 - нормали направлены наружу.

---

## CubeGetNormalDirection

`real CubeGetNormalDirection(real cube);`

Возвращает направление нормалей куба. 

- `cube` - указатель на куб.

---

## SphereCreate

`real SphereCreate(real radius, real slices, real stacks, real parent);`

Создает сферу и возвращает указатель на нее.

- `radius` - радиус сферы
- `slices`, `stacks` - задают количество меридиан и параллелей
- `parent` - указатель на родителя для сферы (0 - отсутствие родителя).

---

## SphereSetOptions

`real SphereSetOptions(real sphere, real radius, real slices, real stacks);`

Задает параметры сферы.

- `sphere` - указатель на сферу
- `radius` - радиус сферы
- `slices`, `stacks` - задают количество меридиан и параллелей.

---

## SphereGetOptions

`real SphereGetOptions(real sphere, real ind);`

Возвращает параметры сферы.

- `sphere` - указатель на сферу
- `ind` - индекс параметра (0 = `radius`, 1 = `slices`, 2 = `stacks`).

---

## SphereSetAngleLimits

`real SphereSetAngleLimits(real sphere, real start, real stop, real top, real bottom);`

Сведения отсутствуют.

- `sphere` - указатель на сферу
- `start`, `stop` - cведения отсутствуют
- `top`, `bottom` - cведения отсутствуют.

---

## SphereGetAngleLimits

`real SphereGetAngleLimits(real sphere, real ind);`

Сведения отсутствуют.

- `sphere` - указатель на сферу
- `ind` - индекс параметра (0 = `start`, 1 = `stop`, 2 = `top`, 3 = `bottom`).

---

## CylinderCreate

`real CylinderCreate(real topradius, real bottomradius, real height, real slices, real stacks, real loops, real parent);`

Создает цилиндр и возвращает указатель на него.

- `topradius` - радиус верхнего основания 
- `bottomradius` - радиус нижнего основания 
- `height` - высота (значение по умолчанию: 1) 
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей 
- `parent` - указатель на родителя для цилиндра (0 - отсутствие родителя).

---

## CylinderSetOptions

`real CylinderSetOptions(real cylinder, real topradius, real bottomradius, real height, real slices, real stacks, real loops);`

Задает параметры цилиндра.

- `cylinder` - указатель на цилиндр
- `topradius` - радиус верхнего основания 
- `bottomradius` - радиус нижнего основания 
- `height` - высота (значение по умолчанию: 1) 
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей.

---

## CylinderGetOptions

`real CylinderGetOptions(real cylinder, real ind);`

Возвращает параметры цилиндра.

- `cylinder` - указатель на цилиндр
- `ind` - индекс параметра (0 = `topradius`, 1 = `bottomradius`, 2 = `height`, 3 = `slices`, 4 = `stacks`, 5 = `loops`).

---

## ConeCreate

`real ConeCreate(real bottomradius, real height, real slices, real stacks, real loops, real parent);`

Создает конус и возвращает указатель на него.

- `bottomradius` - радиус основания
- `height` - высота (значение по умолчанию: 1)
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей
- `parent` - id родителя для конуса (0 - отсутствие родителя).

---

## ConeSetOptions

`real ConeSetOptions(real cone, real bottomradius, real height, real slices, real stacks, real loops);`

Задает параметры конуса.

- `cone` - указатель на конус
- `bottomradius` - радиус основания
- `height` - высота (значение по умолчанию: 1)
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей.

---

## ConeGetOptions

`real ConeGetOptions(real cone, real ind);`

Возвращает параметры конуса.

- `cone` - указатель на конус
- `ind` - индекс параметра (0 = `bottomradius`, 1 = `height`, 2 = `slices`, 3 = `stacks`, 4 = `loops`).

---

## AnnulusCreate

`real AnnulusCreate(real innerradius, real outerradius, real height, real slices, real stacks, real loops, real parent);`

Создает кольцо (полый цилиндр) и возвращает указатель на него.

- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `height` - высота (значение по умолчанию: 1)
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей
- `parent` - указатель на родителя для полого цилиндра (0 - отсутствие родителя).

---

## AnnulusSetOptions

`real AnnulusSetOptions(real annulus, real innerradius, real outerradius, real height, real slices, real stacks, real loops);`

Задает параметры кольца.

- `annulus` - указатель на кольцо
- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `height` - высота (значение по умолчанию: 1)
- `slices`, `stacks`, `loops` - задают количество ребер, сегментов и концентрических окружностей.

---

## AnnulusGetOptions

`real AnnulusGetOptions(real annulus, real ind);`

Возвращает параметры кольца.

- `annulus` - указатель на кольцо
- `ind` - индекс параметра (0 = `innerradius`, 1 = `outerradius`, 2 = `height`, 3 = `slices`, 4 = `stacks`, 5 = `loops`).

---

## TorusCreate

`real TorusCreate(real innerradius, real outerradiusrings, real rings, real sides, real parent);`

Создает тор и возвращает указатель на него. Тор - это тело, напоминающее бублик.

- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `rings`, `sides` - задают количество колец и сегментов
- `parent` - указатель на родителя для тора (0 - отсутствие родителя).

---

## TorusSetOptions

`real TorusSetOptions(real torus, real inerradius, real outerradiusrings, real rings, real sides);`

Задает параметры тора.

- `torus` - указатель на тор
- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `rings`, `sides` - задают количество колец и сегментов.

---

## TorusGetOptions

`real TorusGetOptions(real torus, real ind);`

Возвращает параметры тора.

- `torus` - указатель на тор
- `ind` - индекс параметра (0 = `innerradius`, 1 = `outerradius`, 2 = `rings`, 3 = `sides`).

---

## DiskCreate

`real DiskCreate(real innerradius, real outerradius, real startangle, real sweepangle, real loops, real slices, real parent);`

Создает диск и возвращает указатель на него.

- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `startangle`, `sweepangle` - начальный и конечный угол сектора. Для замкнутого диска эти значения соответствуют 0, 360
- `loops`, `slices` - задают количество концентрических окружностей и ребер
- `parent` - указатель на родителя для диска (0 - отсутствие родителя).

---

## DiskSetOptions

`real DiskSetOptions(real disk, real innerradius, real outerradius, real startangle, real sweepangle, real loops, real slices);`

Задает параметры диска.

- `disk` - указатель на диск
- `innerradius` - радиус внутренней окружности
- `outerradius` - радиус внешней окружности
- `startangle`, `sweepangle` - начальный и конечный угол сектора. Для замкнутого диска эти значения соответствуют 0, 360
- `loops`, `slices` - задают количество концентрических окружностей и ребер.

---

## DiskGetOptions

`real DiskGetOptions(real disk, real ind);`

Возвращает параметры диска.

- `disk` - указатель на диск
- `ind` - индекс параметра (0 = `innerradius`, 1 = `outerradius`, 2 = `startangle`, 3 = `sweepangle`, 4 = `loops`, 5 = `slices`).

---

## FrustrumCreate

`real FrustrumCreate(real basewidth, real basedepth, real apexheight, real cutheight, real parent);`

Создает усеченную пирамиду и возвращает указатель на нее.

- `basewidth`, `basedepth` - ширина и глубина основания
- `apexheight` - высота апекса (вершины, противолежащей основанию)
- `cutheight` - высота отсечения
- `parent` - указатель на родителя для усеченной пирамиды (0 - отсутствие родителя).

---

## FrustrumSetOptions

`real FrustrumSetOptions(real frustrum, real basewidth, real basedepth, real apexheight, real cutheight);`

Задает параметры усеченной пирамиды.

- `frustrum` - указатель на пирамиду
- `basewidth`, `basedepth` - ширина и глубина основания
- `apexheight` - высота апекса (вершины, противолежащей основанию)
- `cutheight` - высота отсечения.

---

## FrustrumGetOptions

`real FrustrumGetOptions(real frustrum, real ind);`

Возвращает параметры усеченной пирамиды.

- `frustrum` - указатель на пирамиду
- `ind` - индекс параметра (0 = `basewidth`, 1 = `basedepth`, 2 = `apexheight`, 3 = `cutheight`).

---

## DodecahedronCreate

`real DodecahedronCreate(real parent);`

Создает додекаэдр (многогранник, составленный из 12 правильных пятиугольников) и возвращает указатель на него.

- `parent` - указатель на родителя для додекаэдра (0 - отсутствие родителя).

---

## IcosahedronCreate

`real IcosahedronCreate(real parent);`

Создает икосаэдр (многогранник, составленный из 20 равносторонних треугольников) и возвращает указатель на него.

- `parent` - указатель на родителя для икосаэдра (0 - отсутствие родителя).

---

## TeapotCreate

`real TeapotCreate(parent);`

Создает чайник Юта и возвращает указатель на него.

- `parent` - указатель на родителя для чайника (0 - отсутствие родителя).

---

## PipeCreate

`real PipeCreate(real division, real slices, real parent);`

Создает трубу и возвращает указатель на нее.

- `division` - сведения отсутствуют
- `slices` - задает количество ребер
- `parent` - указатель на родителя для трубы (0 - отсутствие родителя).

---

## PipeAddNode

`real PipeAddNode(real pipe, real x, real y, real z);`

Добавляет узел к трубе и присваивает ему порядковый номер. Отсчет начинается с нуля.

- `pipe` - указатель на трубу
- `x`, `y`, `z` - локальные координаты узла.

---

## PipeDeleteNode

`real PipeDeleteNode(real pipe, real index);`

Удаляет узел из трубы.

- `pipe` - указатель на трубу
- `index` - порядковый номер узла.

---

## PipeSetNode

`real PipeSetNode(real pipe, real index, real x, real y, real z);`

Задает кординаты узла трубы.

- `pipe` - указатель на трубу
- `index` - порядковый номер узла
- `x`, `y`, `z` - локальные кординаты узла.

---

## PipeSetDivision

`real PipeSetDivision(real pipe, real division);`

Сведения отсутствуют.

- `pipe` - указатель на трубу
- `division` - сведения отсутствуют.

---

## PipeSetSlices

`real PipeSetSlices(real pipe, real slices);`

Задает количество ребер трубы.

- `pipe` - указатель на трубу
- `slices` - количество ребер.

---

## PipeSetRadius

`real PipeSetRadius(real pipe, real radius);`

Задает радиус трубы.

- `pipe` - указатель на трубу
- `radius` - радиус.

---

## PipeSetSplineMode

`real PipeSetSplineMode(real pipe, real lsm);`

Задает режим сплайна трубы.

- `pipe` - указатель на трубу
- `lsm` - режим сплайна:
    - `lsmLines` = 0 - прямая
    - `lsmCubicSpline` = 1 - кубический сплайн
    - `lsmBezierSpline` = 2 - сплайн Безье
    - `lsmNURBSCurve` = 3 - сплайн NURBS
    - `lsmSegments` = 4 - пунктир.

---

## TilePlaneCreate

`real TilePlaneCreate(real parent);`

Создает тайловую плоскость и возвращает указатель на нее.

parent - указатель на родителя для тайловой плоскости (0 - отсутствие родителя).

---

## TilePlaneSetTile

`real TilePlaneSetTile(real tileplane, real x, real y, string mat);`

Задает материал тайла в позиции x и y.

tileplane - указатель на тайловую плоскость
x, y - позиция тайла
mat - материал.
