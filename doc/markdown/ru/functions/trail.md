# Trail

Объект trail создает позади другого объекта красивый шлейф, возникающий при быстром движении. Такой эффект можно использовать, например, в авиасимуляторах.

---

## TrailCreate

`real TrailCreate(real object, real parent);`

Создает новый объект trail и возвращает указатель.

- `object` - указатель на целевой объект, к которому должен быть применен эффект шлейфа
- `parent` - указатель на родителя.

---

## TrailSetObject

`real TrailSetObject(real trail, real object);`

Меняет целевой объект, к которому должен быть применен эффект шлейфа.

- `trail` - указатель на объект trail
- `object` - указатель на новый целевой объект.

---

## TrailSetAlpha

`real TrailSetAlpha(real trail, real alpha, real fade);`

Задает прозрачность шлейфа.

- `trail` - указатель на объект trail
- `alpha` - прозрачность
- `fade` - определяет, нужно ли сделать шлейф плавно исчезающим: `true` или `false` (1 и 0 соответственно).

---

## TrailSetLimits

`real TrailSetLimits(real trail, real vl, real tl);`

Задает ограничения шлейфа.

- `trail` - указатель на объект trail
- `vl` - максимальная длина шлейфа
- `tl` - время до исчезновения шлейфа (в секундах).

---

## TrailSetMinDistance

`real TrailSetMinDistance(real trail, real distance);`

Задает расстояние, при прохождении которого целевым объектом следует создать очередной сегмент шлейфа. Чем выше это значение, тем качественнее эффект.

- `trail` - указатель на объект trail
- `distance` - расстояние.

---

## TrailSetUVScale

`real TrailSetUVScale(real trail, real uv);`

Сведения отсутствуют.

- `trail` - указатель на объект trail
- `uv` - сведения отсутствуют.

---

## TrailSetMarkStyle

`real TrailSetMarkStyle(real trail, real ms);`

Задает стиль шлейфа (ms).

- `trail` - указатель на объект trail
- `ms` - стиль шлейфа. Доступны следующие значения `ms`:
    - `msUp` = 0 - шлейф всегда выровнен по вертикали
    - `msDirection` = 1 - шлейф похож на экран радара: движется по кругу относительно локального центра объекта trail, следуя за целевым объектом
    - `msFaceCamera` = 2 - шлейф всегда выровнен по камере и поэтому кажется объемным (значение по умолчанию).

---

## TrailSetMarkWidth

`real TrailSetMarkWidth(real trail, real width);`

Задает размер шлейфа.

- `trail` - указатель на объект trail
- `width` - размер (по умолчанию: 1).

---

## TrailSetEnabled

`real TrailSetEnabled(real trail, real mode);`

Включает или выключает шлейф.

- `trail` - указатель на объект trail
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## TrailClearMarks

`real TrailClearMarks(real trail);`

Сбрасывает состояние шлейфа. Полезно, когда нужно резко остановить объект, не дожидаясь, когда шлейф сам исчезнет.

- `trail` - указатель на объект trail.
