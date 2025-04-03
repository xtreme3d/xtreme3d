# TextureEx

Функции для расширенных текстур материала, которые создаются функцией `MaterialAddTextureEx`.

---

## TextureExLoad

`real TextureExLoad(real textureExItem, string filename);`

Загружает текстуру из файла.

- `textureExItem` - указатель на текстуру
- `filename` - путь к файлу изображения.

---

## TextureExSetFromMaterial

`real TextureExSetFromMaterial(real textureExItem, string material);`

Копирует текстуру из материала.

- `textureExItem` - указатель на текстуру
- `material` - имя материала.

---

## TextureExGenerate

`real TextureExGenerate(real textureExItem, real width, real height);`

Генерирует текстуру.

- `textureExItem` - указатель на текстуру
- `width`, `height` - ширина и высота текстуры в пикселях.

---

## TextureExDelete

`real TextureExDelete(real textureExItem);`

Удаляет текстуру.

- `textureExItem` - указатель на текстуру.

---

## TextureExSetTextureScale

`real TextureExSetTextureScale(real textureExItem, real xscale, real yscale);`

Задает масштаб текстуры.

- `textureExItem` - указатель на текстуру
- `xscale`, `yscale` - масштаб по горизонтали и вертикали.

---

## TextureExSetTextureOffset

`real TextureExSetTextureOffset(real textureExItem, real x, real y);`

Задает смещение текстуры.

- `textureExItem` - указатель на текстуру
- `x`, `y` - смещение по горизонтали и вертикали.

---

## TextureExEnable

`real TextureExEnable(real textureItem, real mode);`

Включает или выключает текстуру.

- `textureExItem` - указатель на текстуру
- `mode` - `true` или `false` (1 и 0 соответственно).
