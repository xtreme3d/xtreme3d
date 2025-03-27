# Freeform

Объект свободной формы (Freeform) - неанимированная модель. Объекты такого типа используются обычно в качестве игровых уровней, декораций и интерактивных предметов. Freeform состоит из одного или более мешей (Mesh) - наборов вершин, каждый из которых, в свою очередь, содержит одну или более фейсгрупп (FaceGroup) - наборов граней, составленных из этих вершин. Фейсгруппам и мешам можно назначать отдельные материалы. Однако не все форматы моделей поддерживают материалы, как и деление на меши и фейсгруппы.

Freeform обычно загружается из файла 3D-модели. Xtreme3D поддерживает следующие форматы моделей:
- Игровые и околоигровые - BSP (Quake 3), B3D (Blitz3D), LOD (LODka 3D), X (Microsoft DirectX), CSM (Cartography Shop 4), LMTS (Pulsar LMTools), DXS (DeleD);
- Пакеты 3D-моделирования и CAD - 3DS (3D Studio), OBJ (Maya), LWO (Lightwave), MS3D (Milkshape), ASE (3ds Max), OCT (FSRad), NMF (AMD NormalMapper), WRL (VRML 1.0);
- Научные - PLY (Stanford Triangle Format), GTS (GNU Triangulated Surface), TIN (Triangular Irregular Network), STL (Stereolithography);
- Собственный формат - GLSM (GLScene Mesh).

Поддерживается также формат FBX, но для загрузки этого формата требуется дополнительная библиотека OpenFBX.dll (ищите ее в SDK). Ее нужно поместить в рабочую папку игры, рядом с xtreme3d.dll. Если вам не нужна поддержка FBX, то библиотека необязательна.

Вы также можете создать Freeform в оперативной памяти "с нуля", добавляя в пустую модель вершины и грани. Созданную в памяти модель можно сохранить в файл функцией FreeformSave, но для сохранения доступны не все форматы.

---

## FreeformCreate

`real FreeformCreate(string filename, real matlib, real lmapmatlib, real parent);`

Создает новый объект свободной формы и возвращает указатель на него.

- `filename` - путь к файлу модели
- `matlib` - библиотека материалов, в которой следует хранить материалы модели
- `lmapmatlib` - библиотека материалов, в которой следует хранить материалы карт освещения модели
- `parent` - указатель на родителя для объекта свободной формы (0 - отсутствие родителя).

---

## FreeformCreateEmpty

`real FreeformCreateEmpty(real matlib, real lmapmatlib, real parent);`

Создает новый пустой объект свободной формы и возвращает указатель на него. Геометрия в нем задается при помощи функций ниже - таким образом, вы можете создать собственный формат моделей.

- `matlib` - библиотека материалов, в которой хранятся материалы модели
- `lmapmatlib` - библиотека материалов, в которой хранятся материалы карт освещения модели
- `parent` - указатель на родителя для объекта свободной формы (0 - отсутствие родителя).

---

## FreeformSetMaterialLibraries

`real FreeformSetMaterialLibraries(real freeform, real matlib, real lmapmatlib);`

Задает библиотеки материалов объекта свободной формы.

- `freeform` - указатель на объект свободной формы
- `matlib` - библиотека материалов, в которой хранятся материалы модели
- `lmapmatlib` - библиотека материалов, в которой хранятся материалы карт освещения модели.

---

## FreeformAddMesh

`real FreeformAddMesh(real freeform);`

Создает новый пустой меш в объекте свободной формы и возвращает его индекс.

- `freeform` - указатель на объект свободной формы.

---

## FreeformMeshAddVertex

`real FreeformMeshAddVertex(real freeform, real mesh, real x, real y, real z);`

Добавляет новую вершину в меш и возвращает ее индекс. Вершины задаются в локальном пространстве объекта (то есть, безотносительно позиции, поворота и масштаба).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - координаты вершины.

---

## FreeformMeshAddNormal

`real FreeformMeshAddNormal(real freeform, real mesh, real x, real y, real z);`

Добавляет новую нормаль в меш и возвращает ее индекс. Нормали задаются в локальном пространстве объекта (то есть, безотносительно позиции, поворота и масштаба).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - вектор нормали.

---

## FreeformMeshAddTexCoord

`real FreeformMeshAddTexCoord(real freeform, real mesh, real u, real v);`

Добавляет новую пару текстурных координат в меш и возвращает ее индекс.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `u`, `v` - текстурные координаты.

---

## FreeformMeshAddSecondTexCoord

`real FreeformMeshAddSecondTexCoord(real freeform, real mesh, real u, real v);`

Добавляет новую пару вторых текстурных координат в меш и возвращает ее индекс. Второй набор текстурных координат обычно используется для наложения карты освещения (lightmap).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `u`, `v` - вторые текстурные координаты.

---

## FreeformMeshAddTangent

`real FreeformMeshAddTangent(real freeform, real mesh, real x, real y, real z);`

Добавляет новый касательный вектор (тангент) в меш и возвращает его индекс. Касательные векторы задаются в локальном пространстве объекта (то есть, безотносительно позиции, поворота и масштаба).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - касательный вектор.

---

## FreeformMeshAddBinormal

`real FreeformMeshAddBinormal(real freeform, real mesh, real x, real y, real z);`

Добавляет новую бинормаль в меш и возвращает ее индекс. Бинормали задаются в локальном пространстве объекта (то есть, безотносительно позиции, поворота и масштаба).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - вектор бинормали.

---

## FreeformMeshSetVertex

`real FreeformMeshSetVertex(real freeform, real mesh, real vertex, real x, real y, real z);`

Задает координаты вершины меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `vertex` - индекс вершины
- `x`, `y`, `z` - координаты.

---

## FreeformMeshSetNormal

`real FreeformMeshSetNormal(real freeform, real mesh, real normal, real x, real y, real z);`

Задает вектор нормали меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `normal` - индекс нормали
- `x`, `y`, `z` - вектор нормали.

---

## FreeformMeshSetTexCoord

`real FreeformMeshSetTexCoord(real freeform, real mesh, real texcoord, real u, real v);`

Задает пару текстурных координат меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `texcoord` - индекс текстурных координат
- `u`, `v` - текстурные координаты.

---

## FreeformMeshSetSecondTexCoord

`real FreeformMeshSetSecondTexCoord(real freeform, real mesh, real texcoord, real u, real v);`

Задает пару вторых текстурных координат меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `texcoord` - индекс вторых текстурных координат
- `u`, `v` - текстурные координаты.

---

## FreeformMeshSetTangent

`real FreeformMeshSetTangent(real freeform, real mesh, real tangent, real x, real y, real z);`

Задает вектор касательной меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `tangent` - индекс вектора касательной
- `x`, `y`, `z` - вектор касательной.

---

## FreeformMeshSetBinormal

`real FreeformMeshSetBinormal(real freeform, real mesh, real binormal, real x, real y, real z);`

Задает вектор бинормали меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `binormal` - индекс вектора бинормали
- `x`, `y`, `z` - вектор бинормали.

---

## FreeformMeshFaceGroupSetIndex

`real FreeformMeshFaceGroupSetIndex(real freeform, real mesh, real facegroup, real index, real i);`

Задает индекс вершины в фейсгруппе. Индексы идут друг за другом, образуя "плоский" список треугольников - то есть, индексы под номером 0, 1, 2 образуют первый треугольник, 3, 4, 5 - второй и т.д. Для нормалей, текстурных координат и других атрибутов используются те же индексы, что и для вершин.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `facegroup` - индекс фейсгруппы
- `index` - номер индекса вершины
- `i` - индекс вершины.

---

## FreeformMeshGetVertex

`real FreeformMeshGetVertex(real freeform, real mesh, real vertex, real index);`

Возвращает координату вершины меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `vertex` - индекс вершины
- `index` - индекс координаты вершины (0 = X, 1 = Y, 2 = Z).

---

## FreeformMeshGetNormal

`real FreeformMeshGetNormal(real freeform, real mesh, real normal, real index);`

Возвращает координату вектора нормали меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `normal` - индекс нормали
- `index` - индекс координаты вектора нормали (0 = X, 1 = Y, 2 = Z).

---

## FreeformMeshGetTexCoord

`real FreeformMeshGetTexCoord(real freeform, real mesh, real texcoord, real index);`

Возвращает текстурную координату меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `texcoord` - индекс пары текстурных координат
- `index` - индекс координаты (0 = U, 1 = V).

---

## FreeformMeshGetSecondTexCoord

`real FreeformMeshGetSecondTexCoord(real freeform, real mesh, real texcoord, real index);`

Возвращает текстурную координату меша из второго набора текстурных координат.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `texcoord` - индекс пары текстурных координат
- `index` - индекс координаты (0 = U, 1 = V).

---

## FreeformMeshGetTangent

`real FreeformMeshGetTangent(real freeform, real mesh, real tangent, real index);`

Возвращает координату вектора касательной меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `tangent` - индекс вектора касательной
- `index` - индекс координаты вектора касательной (0 = X, 1 = Y, 2 = Z).

---

## FreeformMeshGetBinormal

`real FreeformMeshGetBinormal(real freeform, real mesh, real binormal, real index);`

Возвращает координату вектора бинормали меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `binormal` - индекс вектора бинормали
- `index` - индекс координаты вектора бинормали (0 = X, 1 = Y, 2 = Z).

---

## FreeformMeshFaceGroupGetIndex

`real FreeformMeshFaceGroupGetIndex(real freeform, real mesh, real facegroup, real index);`

Возвращает индекс вершины в фейсгруппе. Индексы идут друг за другом, образуя "плоский" список треугольников - то есть, индексы под номером 0, 1, 2 образуют первый треугольник, 3, 4, 5 - второй и т.д. Для нормалей, текстурных координат и других атрибутов используются те же индексы, что и для вершин.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `facegroup` - индекс фейсгруппы
- `index` - номер индекса вершины.

---

## FreeformMeshVerticesCount

`real FreeformMeshVerticesCount(real freeform, real mesh);`

Возвращает количество вершин в меше.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshTriangleCount

`real FreeformMeshTriangleCount(real freeform, real mesh);`

Возвращает количество треугольников в меше.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshObjectsCount

`real FreeformMeshObjectsCount(real freeform);`

Возвращает количество мешей в модели.

- `freeform` - указатель на объект свободной формы.

---

## FreeformMeshObjectGetName

`string FreeformMeshObjectGetName(real freeform, real mesh);`

Возвращает имя меша в модели (работает не для всех форматов моделей).

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshObjectSetName

`string FreeformMeshObjectSetName(real freeform, real mesh, string name);`

Задает имя меша свободной формы.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `name` - имя меша.

---

## FreeformMeshObjectDestroy

`real FreeformMeshObjectDestroy(real freeform, real mesh);`

Удаляет меш свободной формы под индексом mesh.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshAddFaceGroup

`real FreeformMeshAddFaceGroup(real freeform, real mesh);`

Создает новую пустую фейсгруппу в меше и возвращает ее индекс.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshFaceGroupAddTriangle

`real FreeformMeshFaceGroupAddTriangle(real freeform, real mesh, real fgroup, real v1, real v2, real v3);`

Добавляет новый треугольник в фейсгруппу и возвращает его индекс. Треугольник образуется вершинами индексов v1, v2, v3. Эти же индексы используются в отношении нормалей, текстурных координат и других вершинных атрибутов. Нормали, текстурные координаты и другие дополнительные атрибуты опциональны - если у меша их нет, то ничего страшного не произойдет, движок продолжит работу и будет рендерить меш без этих данных.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы
- `v1`, `v2`, `v3` - индексы вершин в меше.

---

## FreeformMeshFaceGroupTriangleCount

`real FreeformMeshFaceGroupTriangleCount(real freeform, real mesh, real fgroup);`

Возвращает количество треугольников в фейсгруппе.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы.

---

## FreeformMeshFaceGroupSetMaterial

`real FreeformMeshFaceGroupSetMaterial(real freeform, real mesh, real fgroup, string material);`

Присваивает фейсгруппе материал.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы
- `material` - имя материала. Материал должен присутствовать в библиотеке материалов, указанной при создании объекта.

---

## FreeformMeshFaceGroupGetMaterial

`string FreeformMeshFaceGroupGetMaterial(real freeform, real mesh, real fgroup);`

Возвращает имя материала фейсгруппы.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы.

---

## FreeformMeshFaceGroupSetLightmapIndex

`real FreeformMeshFaceGroupSetLightmapIndex(real freeform, real mesh, real fgroup, real index);`

Задает индекс материала, который должен использоваться в качестве карты освещения фейсгруппы. Индекс является порядковым номером материала в библиотеке материалов для карт освещения, задаваемой при создании объекта свободной формы или функцией `FreeformSetMaterialLibraries`.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы
- `index` - индекс материала.

---

## FreeformMeshFaceGroupGetLightmapIndex

`real FreeformMeshFaceGroupGetLightmapIndex(real freeform, real mesh, real fgroup);`

Возвращает индекс материала, используемого в качестве карты освещения фейсгруппы. Индекс является порядковым номером материала в библиотеке материалов для карт освещения, задаваемой при создании объекта свободной формы или функцией FreeformSetMaterialLibraries.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы.

---

## FreeformMeshFaceGroupsCount

`real FreeformMeshFaceGroupsCount(real freeform, real mesh);`

Возвращает количество фейсгрупп меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshGenNormals

`real FreeformMeshGenNormals(real freeform, real mesh);`

Генерирует сглаженные нормали для меша. Эту функцию можно применить, если данные о нормалях отсутствуют. Ее можно вызывать только после создания фейсгрупп, так как нормали невозможно сгенерировать, не имея информации о том, как вершины соединяются между собой.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshGenTangents

`real FreeformMeshGenTangents(real freeform, real mesh);`

Генерирует векторы касательных и бинормалей для меша. Эту функцию можно применить, если данные о касательных и бинормалях отсутствуют. Ее можно вызывать только после создания фейсгрупп, нормалей и текстурных координат.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша.

---

## FreeformMeshSetVisible

`real FreeformMeshSetVisible(real freeform, real mesh, real mode);`

Переключает видимость меша.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## FreeformMeshSetSecondCoords

`real FreeformMeshSetSecondCoords(real freeform1, real mesh1, real freeform2, real mesh2);`

Применяет текстурные координаты заданного меша объекта freeform2 в качестве вторых текстурных координат заданного меша объекта freeform1. Второй набор текстурных координат обычно используется для наложения карты освещения (lightmap).

- `freeform1` - указатель на объект свободной формы
- `mesh1` - индекс меша
- `freeform2` - указатель на объект свободной формы
- `mesh2` - индекс меша

---

## FreeformMeshTranslate

`real FreeformMeshTranslate(real freeform, real mesh, real x, real y, real z);`

Перемещает меш на заданный вектор. Обратите внимание, что это перемещение применяется непосредственно к вершинам модели и никак не влияет на обычную позицию объекта - если вам нужно переместить Freeform, не изменяя его геометрию, воспользуйтесь функцией `ObjectTranslate`.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - вектор переноса.

---

## FreeformMeshRotate

`real FreeformMeshRotate(real freeform, real mesh, real x, real y, real z);`

Поворачивает меш на заданные углы вокруг осей X, Y и Z. Обратите внимание, что этот поворот применяется непосредственно к вершинам модели и никак не влияет на обычный поворот объекта - если вам нужно повернуть объект Freeform, не изменяя его геометрию, воспользуйтесь функцией `ObjectRotate`.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - углы поворота в градусах.

---

## FreeformMeshScale

`real FreeformMeshScale(real freeform, real mesh, real x, real y, real z);`

Масштабирует меш. Обратите внимание, что это масштабирование применяется непосредственно к вершинам модели в локальном пространстве и никак не влияет на обычный масштаб объекта - если вам нужно масштабировать объект Freeform, не изменяя его геометрию, воспользуйтесь функцией ObjectScale.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `x`, `y`, `z` - вектор масштабирования.

---

## FreeformMeshSetMaterial

`real FreeformMeshSetMaterial(real freeform, real mesh, string material);`

Присваивает мешу материал.

- `freeform` - указатель на объект свободной формы
- `mesh` - индекс меша
- `material` - имя материала. Материал должен присутствовать в библиотеке материалов, указанной при создании объекта.

---

## FreeformUseMeshMaterials

`real FreeformUseMeshMaterials(real freeform, real mode);`

Переключает использование материалов мешей. Если отключить эту опцию, объект Freeform будет отрисован с использованием его собственного материала, задаваемого функцией `ObjectSetMaterial`.

- `freeform` - указатель на объект свободной формы
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## FreeformPointInMesh

`real FreeformPointInMesh(real freeform, real x, real y, real z);`

Возвращает true (1), если заданная точка находится внутри меша объекта свободной формы. Функция хорошо подходит, например, для проверки столкновений объекта с пулей.

- `freeform` - указатель на объект свободной формы
- `x`, `y`, `z` - координаты точки.

---

## FreeformSphereSweepIntersect

`real FreeformSphereSweepIntersect(real freeform, real object, real radius, real velocity);`

Возвращает true (1), если сфера заданного радиуса вокруг заданного объекта, движущегося с заданной скоростью, при следующем шаге столкнется с поверхностью объекта свободной формы.

- `freeform` - указатель на объект свободной формы
- `object` - указатель на объект
- `radius` - радиус сферы
- `velocity` - скорость движения объекта.

---

## FreeformToFreeforms

`real FreeformToFreeforms(real freeform, real parent);`

Разбивает объект свободной формы на его составляющие - меши, каждый из которых становится отдельным объектом свободной формы.

- `freeform` - указатель на объект свободной формы
- `parent` - указатель на родителя для полученных объектов.

---

## FreeformSave

`real FreeformSave(real freeform, string filename);`

Сохраняет объект свободной формы в файл. Поддерживается сохранение в форматы OBJ, GLSM, STL и NMF.

- `freeform` - указатель на объект свободной формы
- `filename` - имя файла для сохранения.

---

## FreeformGenTangents

`real FreeformGenTangents(real freeform);`

Генерирует векторы касательных и бинормалей для всех мешей объекта свободной формы. Эту функцию можно применить, если данные о касательных и бинормалях отсутствуют. Ее можно вызывать только если у модели есть нормали и текстурные координаты.

- `freeform` - указатель на объект свободной формы.

---

## FreeformBuildOctree

`real FreeformBuildOctree(real freeform);`

Строит октарное дерево для объекта свободной формы. Это необходимо для работы функций Partition и проверки столкновений.

- `freeform` - указатель на объект свободной формы.

---

## FreeformCreateExplosionFX

`real FreeformCreateExplosionFX(real freeform, real enable);`

Создает эффект взрыва модели: полигоны, составляющие ее, разлетаются в стороны.

- `freeform` - указатель на объект свободной формы
- `enable` - определяет, активировать ли взрыв или нет: `true` или `false` (1 и 0 соответственно).

---

## FreeformExplosionFXReset

`real FreeformExplosionFXReset(real freeform);`

Сбрасывает эффект взрыва (модель принимает свою исходную форму, эффект взрыва деактивируется).

- `freeform` -  указатель на объект свободной формы.

---

## FreeformExplosionFXEnable

`real FreeformExplosionFXEnable(real freeform, real mode);`

Переключает активность эффекта взрыва.

- `freeform` - указатель на объект свободной формы
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## FreeformExplosionFXSetSpeed

`real FreeformExplosionFXSetSpeed(real freeform, real speed);`

Задает скорость разлета полигонов при взрыве.

- `freeform` - указатель на объект свободной формы
- `speed` - скорость.
