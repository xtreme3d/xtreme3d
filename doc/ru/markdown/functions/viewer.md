# Viewer

Видом (Viewer) в Xtreme3D называется область окна, в которой происходит отрисовка 3D-сцены. Вы можете создать несколько независимых видов, и все они могут отображать сцену с различных ракурсов.

---
## ViewerCreate

`real ViewerCreate(real x, real y, real width, real height, real windowHandle);`

Создает новый вид и возвращает ссылку на него.
- `x`, `y` - координаты вида (левый верхний угол) относительно родительского окна;
- `width`, `height` - ширина и высота вида;
- `windowHandle` - дескриптор окна (HWND), к которому следует привязать движок. Дескриптор окна игры в GML возвращается функцией `window_handle()`. В GameMaker: Studio 2 эта функция возвращает не `real`, а `ptr` (указатель). GML, к сожалению, не поддерживает передачу указателей в DLL-функции напрямую, поэтому нужно перевести это значение в `real`, прежде чем передавать во `ViewerCreate`. Для этого в Xtreme3D предусмотрена функция [PointerToReal](engine.html#pointertoreal):

```gml
viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), PointerToReal(windowHandle));
```

---
## ViewerSetCamera

`real ViewerSetCamera(real viewer, real camera);`

Определяет камеру, которую должен использовать вид для отрисовки проекции на сцену. Можно использовать одну и ту же камеру для нескольких видов, но обычно каждому виду назначается своя отдельная камера.

- `viewer` - ссылка на вид;
- `camera` - ссылка на камеру.

---
## ViewerEnableVSync

`real ViewerEnableVSync(real viewer, real vsm);`

Включает или выключает вертикальную синхронизацию для вида. Существует распространенная проблема, когда обновление экрана расходится во времени с рендерингом кадров. Эта функция решает проблему, синхронизируя видеокарту с монитором так, что FPS не превысит частоту обновления экрана. Это и называется вертикальной синхронизацией.

- `viewer` - ссылка на вид;
- `vsm` - `vsmSync` или `vsmNoSync` (0 и 1 соответственно).

---
## ViewerRender

`real ViewerRender(real viewer);`

Совершает рендеринг вида - отрисовку проекции и вывод на экран. Эта функция рендерит все существующие объекты сцены.

- `viewer` - ссылка на вид.

---
## ViewerRenderObject

`real ViewerRenderObject(real viewer, real obj);`

Совершает рендеринг указанного объекта в виде.

- `viewer` - ссылка на вид;
- `obj` - ссылка на объект.

---
## ViewerRenderToFile

`real ViewerRenderToFile(real viewer, string filename);`

Совершает рендеринг вида в файл BMP или PNG (иными словами, делает скриншот).

- `viewer` - ссылка на вид;
- `filename` - имя файла.

---
## ViewerResize

`real ViewerResize(real viewer, real x, real y, real width, real height);`

Изменяет позицию и размер вида. Обычно в играх это не требуется, но может пригодиться, например, если вы даете игроку возможность растягивать окно в оконном режиме. Обратите внимание, что эта функция довольно медленная и не должна вызываться постоянно.

- `viewer` - ссылка на вид;
- `x`, `y` - новые координаты вида (левый верхний угол) относительно левого верхнего угла окна Game Maker 
- `width`, `height` - новые ширина и высота вида.

---
## ViewerGetSize

`real ViewerGetSize(real viewer, real index);`

Возвращает ширину или высоту вида. 

- `viewer` - ссылка на вид;
- `index` - если этот параметр равен 0, функция возвращает ширину, если 1 - высоту.

---
## ViewerGetPosition

`real ViewerGetPosition(real viewer, real index);`

Возвращает позицию вида (координату левого верхнего угла) относительно левого верхнего угла окна Game Maker.

- `viewer` - ссылка на вид;
- `index` - если этот параметр равен 0, функция возвращает координату X, если 1 - Y.

---
## ViewerSetVisible

`real ViewerSetVisible(real viewer, real mode);`

Прячет или показывает вид. Помните, что рендеринг для спрятанного вида не прекращается автоматически - для остановки рендеринга вам необходимо прекратить вызов ViewerRender для данного вида.

- `viewer` - ссылка на вид;
- `mode` - true или false (1 и 0 соответственно).

---
## ViewerGetPixelColor

`real ViewerGetPixelColor(real viewer, real x, real y);`

Возвращает цвет пикселя на позиции x,y относительно верхнего левого угла вида.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты пикселя.

---
## ViewerGetPixelDepth

`real ViewerGetPixelDepth(real viewer, real x, real y);`

Возвращает глубину пикселя в Z-буфере на позиции x,y относительно верхнего левого угла вида.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты пикселя.

---
## ViewerSetLighting

`real ViewerSetLighting(real viewer, real mode);`

Включает или выключает освещение для вида. Если освещение включено, при отрисовке объектов будут учтены источники света, в ином случае объекты будут отрисованы плоско, без затенения.

- `viewer` - ссылка на вид;
- `mode` - true или false (1 и 0 соответственно).

---
## ViewerSetBackgroundColor

`real ViewerSetBackgroundColor(real viewer, real color);`

Определяет фоновой цвет вида. Это цвет, которым по умолчанию заполняется все пустое пространство позади объектов. Это не очень эффектно, но в некоторых случаях достаточно.

- `viewer` - ссылка на вид;
- `color` - цвет.

---
## ViewerSetAmbientColor

`real ViewerSetAmbientColor(real viewer, real color);`

Определяет цвет окружения вида. Цвет окружения используется для задания общего оттенка объектов отдельно от источников света и вне зависимости от их собственных цветов. Можно привести такой пример: ночью все предметы, особенно белые, в неярком свете кажутся голубоватыми, независимо от их собственного цвета. Похожий эффект наблюдается и при использовании инфракрасной камеры.

- `viewer` - ссылка на вид;
- `color` - цвет.

---
## ViewerEnableFog

`real ViewerEnableFog(real viewer, real mode);`

Включает или выключает туман для вида. Туман - это очень распространенный эффект. Все объекты дальше определенного расстояния постепенно окрашиваются в заданный цвет по мере отдаления. Самые дальние объекты уже не имеют собственного цвета и, как предполагается, должны полностью слиться с фоном.

- `viewer` - ссылка на вид;
- `mode` - true или false (1 и 0 соответственно).

---
## ViewerSetFogColor

`real ViewerSetFogColor(real viewer, real color);`

Определяет цвет тумана.

- `viewer` - ссылка на вид;
- `color` - цвет.

---
## ViewerSetFogDistance

`real ViewerSetFogDistance(real viewer, real start, real end);`

Определяет расстояние "затуманивания".

- `viewer` - ссылка на вид;
- `start` - минимальное расстояние, объекты ближе которого не подвергаются действию тумана
- `end` - максимальное расстояние, на котором объекты полностью окрашены в цвет тумана.

---
## ViewerScreenToWorld

`real ViewerScreenToWorld(real viewer, real x, real y, real ind);`

Конвертирует двумерные координаты на экране в абсолютные трехмерные координаты сцены. За (0, 0) берется левый нижний угол экрана.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты на экране
- `ind` - определяет, какую координату должна возвращать функция: x если ind=0, y если ind=1, z если ind=2.

---
## ViewerWorldToScreen

`real ViewerWorldToScreen(real viewer, real x, real y, real z, real ind);`

Конвертирует абсолютные трехмерные координаты сцены в двумерные координаты на экране. За (0, 0) берется левый нижний угол экрана.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты на экране
- `ind` - определяет, какую координату должна возвращать функция: x если ind=0, y если ind=1, z если ind=2.

---
## ViewerPixelRayToWorld

`real ViewerPixelRayToWorld(real viewer, real x, real y, real ind);`

Конвертирует двумерные координаты вида x и y в трехмерные координаты сцены и возвращает индекс кординаты.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты на экране
- `ind` - определяет, какую координату должна возвращать функция: x если ind=0, y если ind=1.

---
## ViewerCopyToTexture

`real ViewerCopyToTexture(real viewer, string material);`

Копирует содержимое вида (отрисованное изображение) в текстуру материала. Обратите внимание, что эта функция работает только в том случае, когда ширина и высота вида соответствуют степеням двойки (512, 256, 128 и т.д.).

- `viewer` - ссылка на вид;
- `material` - имя материала.

---
## ViewerGetFramesPerSecond

`real ViewerGetFramesPerSecond(real viewer);`

Возвращает количество кадров в секунду, отрисовываемое видом (frames per second, FPS).

- `viewer` - ссылка на вид.

---
## ViewerGetPickedObject

`real ViewerGetPickedObject(real viewer, real x, real y);`

Возвращает ближайший объект, которому соответствует заданная точка на экране.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты точки.

---
## ViewerGetPickedObjectsList

`real ViewerGetPickedObjectsList(real viewer, real list, real x, real y, real w, real h, real num);`

Сохраняет в список объекты, которые попадают в заданную прямоугольную область на экране.

- `viewer` - ссылка на вид;
- `list` - id списка PickList
- `x`, `y` - координаты верхнего левого угла прямоугольника
- `w`, `h` - ширина и высота прямоугольника
- `num` - приблизительное ожидаемое количество объектов.

---
## ViewerScreenToVector

`real ViewerScreenToVector(real viewer, real x, real y, real ind);`

Вычисляет единичный вектор от позиции камеры вида к заданной точке на экране.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты на экране
- `ind` - индекс координаты вектора (0 = X, 1 = Y, 2 = Z).

---
## ViewerVectorToScreen

`real ViewerVectorToScreen(real viewer, real x, real y, real z, real ind);`

Вычисляет координаты точки на экране, соответствующей заданному единичному вектору от позиции камеры вида.

- `viewer` - ссылка на вид;
- `x`, `y`, `z` - вектор
- `ind` - индекс координаты точки (0 = X, 1 = Y).

---
## ViewerPixelToDistance

`real ViewerPixelToDistance(real viewer, real x, real y);`

Вычисляет расстояние от позиции камеры вида до ближайшего объекта, которому соответствует заданная точка на экране.

- `viewer` - ссылка на вид;
- `x`, `y` - координаты на экране.

---
## ViewerSetAntiAliasing

`real ViewerSetAntiAliasing(real viewer, real aa);`

Задает режим антиалиасинга (сглаживания) для вида, если это поддерживается видеодрайвером.

- `viewer` - ссылка на вид;
- `aa` - режим антиалиасинга. Доступны следующие значения aa:
- `aaDefault` = 0 - значение по умолчанию (использовать настройку драйвера);
- `aaNone` = 1 - антиалиасинг отключен;
- `aa2x` = 2 - двойной MSAA;
- `aa2xHQ` = 3 - двойной MSAA с технологией Quincunx (на видеокартах от NVIDIA);
- `aa4x` = 4 - четвертной MSAA;
- `aa4xHQ` = 5 - четвертной MSAA с технологией Quincunx (на видеокартах от NVIDIA);
- `aa6x` = 6 - 6-кратный MSAA;
- `aa8x` = 7 - 8-кратный MSAA;
- `aa16x` = 8 - 16-кратный MSAA;
- `csa8x` = 9 - 8-кратный CSAA;
- `csa8xHQ` = 10 - 8-кратный CSAA с технологией Quincunx (на видеокартах от NVIDIA);
- `csa16x` = 11 - 16-кратный CSAA;
- `csa16xHQ` = 12 - 16-кратный CSAA с технологией Quincunx (на видеокартах от NVIDIA).

---
## ViewerShadeModel

`real ViewerShadeModel(real viewer, real smooth);`

Задает тип освещения на полигонах - плоское или сглаженное.

- `viewer` - ссылка на вид;
- `smooth` - `true` (сглаженное освещение) или `false` (плоское освещение) (1 и 0 соответственно).

---
## ViewerGetFBOSupported

`real ViewerGetFBOSupported(real viewer);`

Возвращает истину, если видеодрайвер поддерживает кадровые буферы (FBO).

- `viewer` - ссылка на вид.

---
## ViewerGetVBOSupported

`real ViewerGetVBOSupported(real viewer);`

Возвращает истину, если видеодрайвер поддерживает вершинные буферы (VBO).

- `viewer` - ссылка на вид.

---
## ViewerGetGLSLSupported

`real ViewerGetGLSLSupported(real viewer);`

Возвращает истину, если видеодрайвер поддерживает шейдеры на языке GLSL.

- `viewer` - ссылка на вид.

---
## ViewerIsOpenGLExtensionSupported

`real ViewerIsOpenGLExtensionSupported(real viewer, string ext);`

Возвращает истину, если видеодрайвер поддерживает заданное расширение OpenGL. С помощью этой функции можно заранее проверить, будут ли работать на целевой машине те или иные функции Xtreme3D, и предпринять соответствующие меры - например, вывести информативное сообщение об ошибке. Если какая-то функция зависит от расширения, в данной справке это оговорено в ее описании.

- `viewer` - ссылка на вид;
- `ext` - полное название расширения (например, `"GL_ARB_texture_float"` или `"GL_NV_occlusion_query"`).

---
## ViewerResetPerformanceMonitor

`real ViewerResetPerformanceMonitor(real viewer);`

Сбрасывает счетчик частоты кадров вида.

- `viewer` - ссылка на вид.
