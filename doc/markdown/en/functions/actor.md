# Actor

Актер (Actor) - анимированный объект, загружаемый из внешнего файла. Поддерживаются форматы MD2 (Quake 2), MD3 (Quake 3), MD5 (Doom 3), SMD (Half-Life), MDC (Return To Castle Wolfenstein).

Есть два типа анимации - вертексная и скелетная. В случае вертексной анимации, в памяти хранятся отдельные модели для каждого ключевого кадра. Вершины полигональной сетки просто интерполируются от одного положения к другому.

Скелетная анимация подразумевает наличие только одной модели в нейтральной позе и набора матриц, которые трансформируют отдельные части этой модели. Эти матрицы условно называют костями. Иными словами, в модель встроен "скелет" из этих костей - при повороте отдельной кости, поворачивается и соответствующая часть тела.

---

## ActorCreate

`real ActorCreate(string filename, real matlib, real parent);`

Создает нового актера и возвращает указатель на него.

- `filename` - путь к файлу модели
- `matlib` - библиотека материалов, которую следует использовать для хранения материалов модели
- `parent` - указатель на родителя для актера (0 - отсутствие родителя).

---

## ActorCopy

`real ActorCopy(real actor, real parent);`

Создает копию уже существующего актера и возвращает указатель на него.

- `actor` - указатель на актера
- `parent` - указатель на родителя для копии (0 - отсутствие родителя).

---

## ActorSetAnimationRange

`real ActorSetAnimationRange(real actor, real start, real end);`

Задает начальный и конечный кадр анимации.

- `actor` - указатель на актера
- `start`, `end` - начальный и конечный кадр. Отсчет кадров начинается с нуля.

---

## ActorSetFrame

`real ActorSetFrame(real actor, real frame);`

Задает текущий кадр анимации.

- `actor` - указатель на актера
- `frame` - кадр.

---

## ActorGetCurrentFrame

`real ActorGetCurrentFrame(real actor);`

Возвращает текущий кадр анимации.

- `actor` - указатель на актера.

---

## ActorSwitchToAnimation

`real ActorSwitchToAnimation(real actor, real anim, real smooth);`

Совершает переход к заданной анимационной последовательности.

- `actor` - указатель на актера
- `anim` - порядковый номер анимации
- `smooth` - плавный или резкий переход: `true` или `false` (1 и 0 соответственно).

---

## ActorSwitchToAnimationName

`real ActorSwitchToAnimationName(real actor, string anim, real smooth);`

Совершает переход к заданной анимационной последовательности, определяемой именем.

- `actor` - указатель на актера
- `anim` - имя анимации
- `smooth` - плавный или резкий переход: `true` или `false` (1 и 0 соответственно).

---

## ActorAnimationNextFrame

`real ActorAnimationNextFrame(real actor);`

Переключает кадр анимации на шаг вперед.

- `actor` - указатель на актера.

---

## ActorAnimationPrevFrame

`real ActorAnimationPrevFrame(real actor);`

Переключает кадр анимации на шаг назад.

- `actor` - указатель на актера.

---

## ActorSynchronize

`real ActorSynchronize(real actor1, real actor2);`

Синхронизирует анимацию актера 1 с актером 2.

- `actor1` - указатель на актера 1
- `actor2` - указатель на актера 2.

---

## ActorSetInterval

`real ActorSetInterval(real actor, real interval);`

Задает временной интервал интерполяции между двумя кадрами. Чем выше это значение - тем медленнее анимация.

- `actor` - указатель на актера
- `interval` - интервал (в миллисекундах).

---

## ActorSetAnimationMode

`real ActorSetAnimationMode(real actor, real aam);`

Задает режим воспроизведения анимации.

- `actor` - указатель на актера
- `aam` - режим анимации. Доступны следующие значения `aam`:
    - `aamNone` = 0 - анимация не воспроизводится;
    - `aamPlayOnce` = 1 - анимация воспроизводится один раз и останавливается при достижении конечного кадра;
    - `aamLoop` = 2 - повторяется циклически (по умолчанию);
    - `aamBounceForward` = 3 - повторяется циклически вперед до конечного кадра, затем в обратную сторону до начального кадра, затем опять вперед и так далее;
    - `aamBounceBackward` = 4 - то же самое, но в обратную сторону;
    - `aamLoopBackward` = 5 - повторяется циклически в обратную сторону.

---

## ActorSetFrameInterpolation

`real ActorSetFrameInterpolation(real actor, real mode);`

Включает или выключает линейную интерполяцию между кадрами. При выключенной интерполяции будет резкая смена кадров.

- `actor` - указатель на актера
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ActorAddObject

`real ActorAddObject(real actor, string filename);`

Особенность некоторых форматов (SMD, MD5) - раздельное хранение модели и анимации. Данная функция добавляет актеру новую анимационную последовательность из файла и присваивает ей порядковый номер. Отсчет ведется с 1.

- `actor` - указатель на актера
- `filename` - путь к файлу с анимацией.

---

## ActorAnimationDestroy

`real ActorAnimationDestroy(real actor, real index);`

Удаляет анимацию актера под индексом index.

- `actor` - указатель на актера
- `index` - индекс анимации.

---

## ActorGetCurrentAnimation

`string ActorGetCurrentAnimation(real actor);`

Возвращает текущую анимационную последовательность - путь к файлу анимации без расширения.

- `actor` - указатель на актера.

---

## ActorGetFrameCount

`real ActorGetFrameCount(real actor);`

Возвращает общее количество кадров анимации.

- `actor` - указатель на актера.

---

## ActorGetAnimationCount

`real ActorGetAnimationCount(real actor);`

Возвращает количество анимаций актера.

- `actor` - указатель на актера.

---

## ActorGetAnimationName

`string ActorGetAnimationName(real actor, real index);`

Возвращает имя анимации под заданным индексом.

- `actor` - указатель на актера
- `index` - индекс анимации.

---

## ActorGetBoneCount

`real ActorGetBoneCount(real actor);`

Возвращает общее количество костей в скелете актера.

- `actor` - указатель на актера.

---

## ActorGetBoneByName

`real ActorGetBoneByName(real actor, string bonename);`

Возвращает индекс кости с заданным именем.

- `actor` - указатель на актера
- `bonename` - имя кости. 

---

## ActorRotateBone

`real ActorRotateBone(real actor, real boneindex, real x, real y, real z);`

Поворачивает кость.

- `actor` - указатель на актера
- `boneindex` - индекс кости
- `x`, `y`, `z` - углы поворота по трем осям.

---

## ActorGetBoneRotation

`real ActorGetBoneRotation(real actor, real boneindex, real index);`

Возвращает локальный угол поворота кости по заданной оси.

- `actor` - указатель на актера
- `boneindex` - индекс кости
- `index` - индекс оси (0 = X, 1 = Y, 2 = Z).

---

## ActorMoveBone

`real ActorMoveBone(real actor, real boneindex, real x, real y, real z);`

Перемещает кость.

- `actor` - указатель на актера
- `boneindex` - индекс кости
- `x`, `y`, `z` - координаты смещения по трем осям.

---

## ActorGetBonePosition

`real ActorGetBonePosition(real actor, real boneindex, real index);`

Возвращает координату локального смещения кости по заданной оси.

- `actor` - указатель на актера
- `boneindex` - индекс кости
- `index` - индекс оси (0 = X, 1 = Y, 2 = Z).

---

## ActorShowSkeleton

`real ActorShowSkeleton(real actor, real mode);`

Включает или выключает схематичное отображение скелета.

- `actor` - указатель на актера
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ActorBoneExportMatrix

`real ActorBoneExportMatrix(real actor, real boneindex, real object);`

Копирует матрицу трансформации кости в локальную матрицу трансформации заданного объекта - иными словами, прикрепляет объект к кости. Матрица локальна относительно актера, поэтому для полноценного прикрепления объект нужно сделать дочерним по отношению к актеру. Эта функция незаменима, когда нужно, например дать в руки герою оружие или надеть на него броню.

- `actor` - указатель на актера
- `boneindex` - индекс кости
- `object` - id объекта.

---

## ActorMakeSkeletalTranslationStatic

`real ActorMakeSkeletalTranslationStatic(real actor, real anim);`

Некоторые модели SMD содержат смещение скелета в анимации ходьбы - данная функция устраняет это смещение.

- `actor` - указатель на актера
- `anim` - порядковый номер анимации.

---

## ActorMakeSkeletalRotationDelta

`real ActorMakeSkeletalRotationDelta(real actor, real anim);`

Сведения отсутствуют.

- `actor` - указатель на актера
- `anim` - порядковый номер анимации.

---

## AnimationBlenderCreate

`real AnimationBlenderCreate();`

Создает смешиватель анимации - объект, при помощи которого вы можете применить дополнительную анимацию к актеру - и возвращает указатель на него.

---

## AnimationBlenderSetActor

`real AnimationBlenderSetActor(real animblender, real actor);`

Применяет смешиватель анимации к актеру.

- `animblender` - указатель на смешивателя анимации
- `actor` - указатель на актера.

---

## AnimationBlenderSetAnimation

`real AnimationBlenderSetAnimation(real animblender, string animname);`

Задает анимацию, которую нужно смешать с основной анимацей актера.

- `animblender` - указатель на смешивателя анимации
- `animname` - имя анимации.

---

## AnimationBlenderSetRatio

`real AnimationBlenderSetRatio(real animblender, real ratio);`

Устанавливает кадровую позицию смешиваемой анимации в виде значения от 0 до 1 (0 - первый кадр, 1 - последний кадр). Таким образом, основная и дополнительная анимации могут быть рассинхронизированы во времени, и вы можете контролировать их скорость по отдельности.

- `animblender` - указатель на смешивателя анимации
- `ratio` - кадровая позиция.

---

## ActorLoadQ3TagList

`real ActorLoadQ3TagList(string filename);`

Загружает список анимационных меток из файла MD3 и возвращает указатель на него. Метки - это особенность формата MD3: они используются для того, чтобы прикреплять объекты к разным частям модели и синхронизировать их с вершинной анимацией.

- `filename` - имя файла MD3.

---

## ActorQ3TagExportMatrix

`real ActorQ3TagExportMatrix(real actor, real taglist, string tagname, real object);`

Копирует матрицу трансформации анимационной метки MD3 в матрицу трансформации объекта - то есть, прикрепляет объект к этой метке.

- `actor` - указатель на актера
- `taglist` - указатель на список меток
- `tagname` - имя метки
- `object` - id объекта.

---

## ActorLoadQ3Animations

`real ActorLoadQ3Animations(real actor, string filename, string classname);`

Загружает анимационную последовательность MD3 из файла CFG.

- `actor` - указатель на актера
- `filename` - имя файла CFG
- `classname` - имя класса анимационной последовательности.

---

## ActorMeshObjectsCount

`real ActorMeshObjectsCount(real actor);`

Возвращает количество мешей в модели актера.

- `actor` - указатель на актера

---

## ActorTriangleCount

`real ActorTriangleCount(real actor);`

Возвращает количество треугольников в модели актера.

- `actor` - указатель на актера

---

## ActorMeshSetVisible

`real ActorMeshSetVisible(real actor, real mesh, real mode);`

Включает или выключает отображение меша актера.

- `actor` - указатель на актера
- `mesh` - индекс меша
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## ActorFaceGroupsCount

`real ActorFaceGroupsCount(real actor, real mesh);`

Возвращает количество фейсгрупп в заданном меше актера.

- `actor` - указатель на актера
- `mesh` - индекс меша.

---

## ActorFaceGroupGetMaterialName

`real ActorFaceGroupGetMaterialName(real actor, real mesh, real fgroup);`

Возвращает имя материала заданной фейсгруппы меша.

- `actor` - указатель на актера
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы.

---

## ActorFaceGroupSetMaterial

`real ActorFaceGroupSetMaterial(real actor, real mesh, real fgroup, string material);`

Задает материал фейсгруппе меша. Материал должен присутствовать в библиотеке материалов, указанной при создании актера.

- `actor` - указатель на актера
- `mesh` - индекс меша
- `fgroup` - индекс фейсгруппы
- `material` - имя материала.

---

## ActorSetReference

`real ActorSetReference(real actor, real aar);`

Задает тип анимации актера.

- `actor` - указатель на актера
- `aar`
    - `aarMorph` = 0 - вершинная анимация;
    - `aarSkeleton` = 1 - скелетная анимация;
    - `aarNone` = 2 - анимация отсутствует.
