# Урок 6. Примитивы

Примитивами обычно называют либо простейшие объекты, которые может нарисовать GPU (точка, отрезок, треугольник), либо геометрические тела, встроенные в графический движок. В Xtreme3D используется второе значение этого термина. К телам-примитивам относятся плоскость (plane), куб (cube), сфера (sphere), цилиндр (cylinder), конус (cone), полый цилиндр (annulus), тор (torus), диск (disk), усеченная пирамида (frustrum), додекаэдр (dodecahedron), икосаэдр (icosahedron) и чайник Юта (teapot). На предыдущих уроках мы уже создавали некоторые из них. Давайте познакомимся с примитивами поближе.

Plane. Прямоугольная плоскость. В Xtreme3D плоскость может быть представлена одним прямоугольником (квадом), либо разбита на сетку из квадов. Второй вариант более предпочтителен, если вы создаете большую плоскость-землю, так как в этом случае получается более качественное вершинное освещение (впрочем, при использовании попиксельного освещения детализация плоскости особого значения, как правило, не имеет). Обратите внимание, что количество квадов влияет на повторение текстуры на плоскости (один квад - один тайл текстуры). Плоскость создается функцией `PlaneCreate`.

**Cube**. Куб. Строго говоря, это не обязательно куб, а любой прямоугольный параллелепипед. Создается функцией `CubeCreate`.

**Sphere**. Сфера. Создается функцией `SphereCreate`.

**Cylinder**. Цилиндр. Создается функцией `CylinderCreate`.

**Cone**. Конус. Создается функцией `ConeCreate`.

**Annulus**. Полый цилиндр (кольцо). Создается функцией `AnnulusCreate`.

**Torus**. Тор (тело, напоминающее бублик). Создается функцией `TorusCreate`.

**Disk**. Диск. Создается функцией `DiskCreate`.

**Frustum**. Усеченная пирамида. Создается функцией `FrustrumCreate`.

**Dodecahedron**. Додекаэдр - многогранник, составленный из 12 правильных пятиугольников. Создается функцией `DodecahedronCreate`.

**Icosahedron**. Икосаэдр - многогранник, составленный из 20 равносторонних треугольников. Создается функцией `IcosahedronCreate`.

**Teapot**. Чайник Юта. Вы можете почитать подробнее об этой модели в глоссарии. Создается функцией `TeapotCreate`.
