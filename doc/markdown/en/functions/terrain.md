# Terrain

Xtreme3D поддерживает отрисовку ландшафта (terrain), состоящего из сетки прямоугольников - тайлов. При рендеринге ландшафта поддерживается тесселяция, то есть, повышение детализации по мере приближения к камере.

Для создания ландшафта необходима карта высот (HDS - Height Data Source) - черно-белое изображение, светлые участки которого означают увеличение высоты поверхности, темные - снижение. HDS может быть загружена из файла изображения. Глубина цвета этой текстуры должна соответствовать 8 битам на пиксель (256 цв., Grayscale). В противном случае ландшафт будет выглядеть ступенчатым.

---

## BmpHDSCreate

`real BmpHDSCreate(string filename);`

Загружает HDS из изображения и возвращает указатель на него.

- `filename` - имя файла.

---

## BmpHDSCreateEmpty

`real BmpHDSCreateEmpty(real width, real height, real fillvalue);`

Создает в памяти чистый HDS и возвращает указатель на него.

- `width`, `height` - ширина и высота HDS. Должны быть степенями двойки: 64, 128, 256 и т.д.
- `fillvalue` - значение высоты, которым заполняется HDS (от 0.0 до 1.0). Ландшафт генерируется так, что высота 0.5 соответствует в пространстве плоскости XZ, проходящей через начало координат.

---

## BmpHDSSetInfiniteWarp

`real BmpHDSSetInfiniteWarp(real hds, real mode);`

Включает или выключает повторение HDS. При включенном повторении ландшафт будет бесконечным.

- `hds` - указатель на HDS
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## BmpHDSInvert

`real BmpHDSInvert(real hds);`

Инвертирует HDS (обращает высоты).

- `hds` - указатель на HDS
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## BmpHDSSetHeight

`real BmpHDSSetHeight(real hds, real x, real y, real height);`

Задает высоту в указанном пикселе HDS.

- `hds` - указатель на HDS
- `x`, `y` - координаты пикселя
- `height` - значение высоты (от 0.0 до 1.0).

---

## BmpHDSGetHeight

`real BmpHDSGetHeight(real hds, real x, real y);`

Возвращает высоту в указанном пикселе HDS.

- `hds` - указатель на HDS
- `x`, `y` - координаты пикселя.

---

## BmpHDSSave

`real BmpHDSSave(real hds, string filename);`

Сохраняет HDS в файл BMP.

- `hds` - указатель на HDS
- `filename` - имя файла.

---

## TerrainCreate

`real TerrainCreate(real parent);`

Создает новый ландшафт и возвращает указатель на него.

- `parent` - указатель на родителя для ландшафта (0 - отсутствие родителя).

---

## TerrainSetHeightData

`real TerrainSetHeightData(real terrain, real hds);`

Применяет HDS к ландшафту.

- `terrain` - указатель на ландшафт
- `hds` - указатель на HDS.

---

## TerrainSetTileSize

`real TerrainSetTileSize(real terrain, real size);`

Задает размер тайла.

- `terrain` - указатель на ландшафт
- `size` - размер тайла (значение по умолчанию: 16).

---

## TerrainSetTilesPerTexture

`real TerrainSetTilesPerTexture(real terrain, real tiles);`

Задает количество тайлов на текстуру.

- `terrain` - указатель на ландшафт
- `tiles` - количество тайлов (значение по умолчанию: 1).

---

## TerrainSetQualityDistance

`real TerrainSetQualityDistance(real terrain, real distance);`

Задает расстояние от камеры, на котором детализация ландшафта начинает снижаться для повышения скорости отрисовки.

- `terrain` - указатель на ландшафт
- `distance` - расстояние (значение по умолчанию: 0).

---

## TerrainSetQualityStyle

`real TerrainSetQualityStyle(real terrain, real hrs);`

Определяет метод обработки тайлов, находящихся ближе расстояния `TerrainSetQualityDistance`.

- `terrain` - указатель на ландшафт
- `hrs` - доступны следующие значения:
    - `hrsFullGeometry` = 0 - осуществляется полный рендеринг (без LOD)
    - `hrsTesselated` = 1 - осуществляется одна адаптивная тесселяция (в соответствии со значением `TerrainSetCLodPrecision`), результат которой и используется в дальнейшем, не меняясь.

---

## TerrainSetMaxCLodTriangles

`real TerrainSetMaxCLodTriangles(real terrain, real triangles);`

Задает максимально возможное количество полигонов для отдаленных участков ландшафта (CLOD). Этот лимит не затрагивает тайлы, находящиеся ближе расстояния `TerrainSetQualityDistance`.

- `terrain` - указатель на ландшафт
- `triangles` - количество полигонов (значение по умолчанию: 65536).

---

## TerrainSetCLodPrecision

`real TerrainSetCLodPrecision(real terrain, real precision);`

Задает точность обработки для отдаленных участков ландшафта (CLOD). Чем ниже это значение, тем выше детализация. Этот лимит не затрагивает тайлы, находящиеся ближе расстояния `TerrainSetQualityDistance`.

- `terrain` - указатель на ландшафт
- `precision` - точность (>=1; значение по умолчанию: 100).

---

## TerrainSetOcclusionFrameSkip

`real TerrainSetOcclusionFrameSkip(real terrain, real frames);`

Задает количество кадров отрисовки, в течение которых следует остановить отрисовку "невидимых" тайлов. Тайлы считаются "невидимыми", когда полностью перекрываются другими объектами или частями самого ландшафта (например, большие дома или горы). Это способствует снижению нагрузки на процессор, но может также привести к артефактам и снижению FPS в сценах с минимальным перекрытием (вид сверху). Данная оптимизация требует аппаратной поддержки расширения `GL_NV_occlusion_query`.

- `terrain` - указатель на ландшафт
- `frames` - количество кадров (значение по умолчанию: 0).

---

## TerrainSetOcclusionTesselate

`real TerrainSetOcclusionTesselate(real terrain, real tot);`

Определяет метод влияния перекрытия на тесселяцию. Выключение тесселяции для "невидимых" тайлов повышает производительность, но может негативно повлиять на тесселяцию соседних "видимых" тайлов. Этот эффект можно частично устранить повышением расстояния `TerrainSetQualityDistance` - таким образом, артефакты будут проявляться на отдаленных областях ландшафта и будут менее заметны.

- `terrain` - указатель на ландшафт
- `tot` - `totTesselateAlways` или `totTesselateIfVisible` (0 и 1 соответственно).

---

## TerrainGetHeightAtObjectPosition

`real TerrainGetHeightAtObjectPosition(real terrain, real object);`

Возвращает высоту точки на поверхности ландшафта, соответствующей абсолютной позиции заданного объекта.

- `terrain` - указатель на ландшафт
- `object` - указатель на объект.

---

## TerrainGetHDSPosition

`real TerrainGetHDSPosition(real terrain, real x, real y, real z, real index);`

Возвращает координаты пикселя HDS, соответствующего точке на поверхности ландшафта.

- `terrain` - указатель на ландшафт
- `x`, `y`, `z` - координаты точки
- `index` - индекс координаты (0 = X, 1 = Y).

---

## TerrainGetLastTriCount

`real TerrainGetLastTriCount(real terrain);`

Возвращает количество полигонов в ландшафте на момент последней отрисовки сцены.

- `terrain` - указатель на ландшафт.
