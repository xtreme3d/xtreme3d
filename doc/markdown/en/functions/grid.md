# Grid

Сетка (grid) - это набор пересекающихся отрезков, образующих равномерную прямоугольную сетку, двумерную или трехмерную. Этот объект удобен для использования в отладочных и вспомогательных целях, когда нужно визуализировать координатную плоскость.

---

## GridCreate

`real GridCreate(real x, real y, real z, real step, real parent);`

Создает новую сетку и возвращает указатель на нее.

- `x`, `y`, `z` - полуразмер сетки по трем осям
- `step` - размер ячейки сетки
- `parent` - указатель на родителя.

---

## GridSetLineStyle

`real GridSetLineStyle(real grid, real gls);`

Задает стиль линий сетки.

- `grid` - указатель на сетку
- `gls` - стиль линий сетки. Поддерживаются следующие значения `gls`:
    - `glsSegments` = 0 - сведения отсутствуют
    - `glsLine` = 1 - сведения отсутствуют.

---

## GridSetLineSmoothing

`real GridSetLineSmoothing(real grid, real mode);`

Переключает сглаживание линий сетки.

- `grid` - указатель на сетку
- `mode` - сглаживание: `true` или `false` (1 и 0 соответственно).

---

## GridSetParts

`real GridSetParts(real grid, real gp);`

Задает координатную плоскость сетки.

- `grid` - указатель на сетку
- `gp` - координатная плоскость. Поддерживаются следующие значения `gp`:
    - `gpXY` = 0 - плоскость XY
    - `gpYZ` = 1 - плоскость YZ
    - `gpXZ` = 2 - плоскость XZ
    - `gpXYZ` = 3 - в этом режиме будет отрисована трехмерная сетка (XYZ)

---

## GridSetColor

`real GridSetColor(real grid, real color, real alpha);`

Задает цвет сетки.

- `grid` - указатель на сетку
- `color` - цвет
- `alpha` - прозрачность.

---

## GridSetSize

`real GridSetSize(real grid, real size);`

Задает толщину линии сетки.

- `grid` - указатель на сетку
- `size` - толщина линии.

---

## GridSetPattern

`real GridSetPattern(real grid, real pattern);`

Сведения отсутствуют.

- `grid` - указатель на сетку
- `pattern` - сведения отсутствуют.

---

## GridSetTile

`real GridSetTile(real grid, real x, real y, real z);`

Сведения отсутствуют.

- `grid` - указатель на сетку
- `x`, `y`, `z` - сведения отсутствуют.

---

## GridSetStep

`real = GridSetStep(real grid, real step);`

Сведения отсутствуют.

- `grid` - указатель на сетку
- `step` - сведения отсутствуют.
