# Shaders

Шейдер в Xtreme3D - это спецэффект для материала. Есть несколько видов шейдеров: Bump, Cel, HiddenLine, MultiMaterial, Outline, TexCombine, Phong и GLSL. Для применения шейдера к материалу используется функция `MaterialSetShader`. При помощи шейдеров на языке GLSL вы можете программировать свои собственные спецэффекты.

---

## ShaderEnable

`real ShaderEnable(real shader, real mode);`

Включает или выключает шейдер.

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## BumpShaderCreate

`real BumpShaderCreate();`

Создает шейдер рельефа (bump mapping) и возвращает указатель на него. С его помощью можно добиться очень эффектной имитации неровностей на поверхности модели.

Данный шейдер требует поддержки расширений `GL_ARB_shading_language` (или `GL_ARB_shading_language_100`), `GL_ARB_shader_objects`, `GL_ARB_vertex_shader`, `GL_ARB_fragment_shader`.

---

## BumpShaderSetDiffuseTexture

`real BumpShaderSetDiffuseTexture(real shader, string material);`

Задает диффузную текстуру шейдера рельефа. Диффузная текстура - это обычная текстура, определяющая цвет поверхности объекта (а также прозрачность, если у нее есть альфа-канал).

- `shader` - указатель на шейдер
- `material` - имя материала в текущей библиотеке, из которого следует читать текстуру. Если вместо имени указана пустая строка, то будет использована текстура материала, к которому подключен шейдер (текстура в слоте 0). Таким образом, один и тот же шейдер может работать с разными текстурами в зависимости от материала, что в некоторых ситуациях очень удобно.

---

## BumpShaderSetNormalTexture

`real BumpShaderSetNormalTexture(real shader, string material);`

Задает карту нормалей шейдера рельефа. Карта нормалей - это текстура, определяющая нормаль в каждой точке поверхности объекта (XYZ = RGB). BumpShader поддерживает карты нормалей в пространстве касательных (tangent space) - то есть, заданные в пространстве, где ось Z соответствует интерполированной нормали треугольника, а X и Y - взаимно перпендикулярным касательным к нормали (их также называют тангенс и бинормаль).

- `shader` - указатель на шейдер
- `material` - имя материала в текущей библиотеке, из которого следует читать текстуру. Если вместо имени указана пустая строка, то будет использована вторая текстура материала, к которому подключен шейдер (текстура в слоте 1). Таким образом, один и тот же шейдер может работать с разными текстурами в зависимости от материала, что в некоторых ситуациях очень удобно.

---

## BumpShaderSetHeightTexture

`real BumpShaderSetHeightTexture(real shader, string material);`

Задает карту высот шейдера рельефа. Карта высот - это текстура, темные элементы которой означают снижение высоты, а светлые - повышение. Карта высот необходима при использовании эффекта parallax mapping (см. ниже). Если этот эффект отключен, карта высот не требуется.

- `shader` - указатель на шейдер
- `material` - имя материала в текущей библиотеке, из которого следует читать текстуру. Если вместо имени указана пустая строка, то будет использована третья текстура материала, к которому подключен шейдер (текстура в слоте 2). Таким образом, один и тот же шейдер может работать с разными текстурами в зависимости от материала, что в некоторых ситуациях очень удобно.

---

## BumpShaderUseParallax

`real BumpShaderUseParallax(real shader, real mode);`

Включает или выключает parallax mapping - эффект усиления иллюзии рельефа, достигаемый за счет смещения текстурных координат в зависимости от угла обзора. Этот эффект требует указания карты высот функцией BumpShaderSetHeightTexture.

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно). По умолчанию parallax mapping выключен.

---

## BumpShaderSetParallaxOffset

`real BumpShaderSetParallaxOffset(real shader, real offset);`

Задает коэффициент смещения для эффекта parallax mapping. Чем больше это значение, тем выше рельеф.

- `shader` - указатель на шейдер
- `offset` - смещение. Значение по умолчанию: 0.03.

---

## BumpShaderSetMaxLights

`real BumpShaderSetMaxLights(real shader, real maxlights);`

Задает количество источников света, которые должны учитываться шейдером рельефа. Это количество, как правило, меньше или равно числу источников света, созданных функцией `LightCreate`. Соответственно, оно ограничено восемью - по максимальному количеству встроенных источников света OpenGL. Шейдер поддерживает все типы источников света: `lsOmni`, `lsSpot`, `lsParallel`.

Обратите также внимание, что состояние включенности/выключенности источников света, задаваемое функцией `LightSetShining`, игнорируется шейдером. Чтобы выключить источник света, можно функцией `LightSetAttenuation` выставить в ноль его постоянную составляющую затухания (constant attenuation).

- `shader` - указатель на шейдер
- `maxlights` - количество источников света. Значение по умолчанию: 8.

---

## BumpShaderSetShadowMap

`real BumpShaderSetShadowMap(real shader, real shadowmap);`

Задает теневую карту, которую шейдер рельефа должен использовать для отрисовки теней. По умолчанию теневая карта не присвоена, и шейдер не рендерит тени.

- `shader` - указатель на шейдер
- `shadowmap` - указатель на теневую карту. Если в этот параметр передать 0, то тени будут выключены.

---

## BumpShaderSetShadowBlurRadius

`real BumpShaderSetShadowBlurRadius(real shader, real radius);`

Если шейдеру рельефа присвоена теневая карта, то эта функция задает радиус PCF-фильтрации теневой карты - в итоге, получаются мягкие тени.

- `shader` - указатель на шейдер
- `radius` - радиус размытия. Значение 0 означает отсутствие фильтрации (тени будут резкими). Рекомендуется выставлять значения радиуса от 1 до 3 - более высокие значения могут снизить производительность. Значение по умолчанию - 0.

---

## BumpShaderUseAutoTangentSpace

`real BumpShaderUseAutoTangentSpace(real shader, real mode);`

Некоторые виды объектов Xtreme3D не предоставляют шейдерам информацию о пространстве касательных, необходимую для рендеринга рельефа - это актеры и встроенные примитивы. Данная функция решает эту проблему: она включает или выключает автоматическое генерирование касательного вектора и бинормали во фрагментном шейдере. Результат получается несколько менее качественный, чем при использовании полноценно предрассчитанных векторов - при сильном приближении иногда видны ребра полигонов, но это зависит от того, есть ли разрывы (швы) в текстурных координатах модели. Если UV-развертка непрерывна, как у примитивов, то ребер заметно не будет.

Для включения этой опции необходимо, чтобы у модели были текстурные координаты.

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно). По умолчанию опция включена.

---

## CelShaderCreate

`real CelShaderCreate();`

Создает так называемый cel-шейдер и возвращает указатель на него. Этот тип шейдера используется для создания эффекта мультипликационного изображения. Он включает отрисовку контуров и упрощение модели освещения (свет и тень представлены четко разграниченными пятнами).

Данный шейдер требует поддержки расширений GL_ARB_vertex_program` и `GL_ARB_fragment_program`.

---

## CelShaderSetLineColor

`real CelShaderSetLineColor(real shader, real color);`

Задает цвет контура в cel-шейдере.

- `shader` - указатель на шейдер
- `color` - цвет.

---

## CelShaderSetLineWidth

`real CelShaderSetLineWidth(real shader, real width);`

Задает толщину контура в cel-шейдере.

- `shader` - указатель на шейдер
- `width` - ширина.

---

## CelShaderSetOptions

`real CelShaderSetOptions(real shader, real outlines, real textured);`

Задает опции cel-шейдера.

- `shader` - указатель на шейдер
- `outlines` - указывает, рисовать ли контуры - `true` или `false` (1 и 0 соответственно)
- `textured` - указывает, использовать ли текстуру материала - `true` или `false` (1 и 0 соответственно).

---

## HiddenLineShaderCreate

`real HiddenLineShaderCreate();`

Создает шейдер скрытых линий и возвращает указатель на него. Скрытые линии в данном случае - это ребра полигонов, из которых состоят объекты. Обычно они не отображаются, но этот шейдер позволяет их увидеть.

---

## HiddenLineShaderSetLineSmooth

`real HiddenLineShaderSetLineSmooth(real shader, real mode);`

Включает или выключает антиалиасинг линий (по умолчанию отключен).

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## HiddenLineShaderSetSolid

`real HiddenLineShaderSetSolid(real shader, real mode);`

Включает или выключает отображение самого материала (по умолчанию отключено).

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## HiddenLineShaderSetSurfaceLit

`real HiddenLineShaderSetSurfaceLit(real shader, real mode);`

Включает или выключает влияние освещения на материал с шейдером.

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## HiddenLineShaderSetFrontLine

`real HiddenLineShaderSetFrontLine(real shader, real width, real color, real p, real f);`

Задает параметры передних линий. Передние линии принадлежат полигонам, нормали которых повернуты в сторону наблюдателя.

- `shader` - указатель на шейдер
- `width` - толщина
- `color` - цвет
- `p` - cведения отсутствуют (по умолчанию 65535)
- `f` - cведения отсутствуют.

---

## HiddenLineShaderSetBackLine

`real HiddenLineShaderSetBackLine(real shader, real width, real color, real p, real f);`

Задает параметры задних линий. Задние линии принадлежат полигонам, нормали которых повернуты в противоположную сторону от наблюдателя.

- `shader` - указатель на шейдер
- `width` - толщина
- `color` - цвет
- `p` - cведения отсутствуют (по умолчанию 65535)
- `f` - cведения отсутствуют.

---

## MultiMaterialShaderCreate

`real MultiMaterialShaderCreate(real matlib);`

Создает мультиматериальный шейдер и возвращает указатель на него. Этот шейдер позволяет создать материал, состоящий из нескольких слоев других материалов, с собственными текстурами и параметрами. Эти материалы загружаются из отдельной библиотеки.

Этот чрезвычайно мощный шейдер будет главным образом полезен для имитации поверхностей, состоящих из нескольких веществ с различными свойствами: например, деревянный ящик, окованный металлом, или камень с вкраплениями лавы или кристаллов. Но ему можно найти и другие применения.

Не рекомендуется создавать слишком много слоев, иначе движок будет тормозить. Если вы хотите создать сложный многокомпонентный материал, то эффективнее будет использовать шейдер GLSL.

- `matlib` - указатель на библиотеку материалов. Материалы будут накладываться в том порядке, в каком они были добавлены в библиотеку (то есть, первый материал будет внизу).

---

## OutlineShaderCreate

`real OutlineShaderCreate(real smooth);`

Создает контурный шейдер и возвращает указатель на него. Используется для простейшего мультипликационного эффекта - отрисовки контуров вокруг объекта.

- `smooth` - cведения отсутствуют.

---

## OutlineShaderSetLineColor

`real OutlineShaderSetLineColor(real shader, real color);`

Задает цвет контура в контурном шейдере.

- `shader` - указатель на шейдер
- `color` - цвет.

---

## OutlineShaderSetLineWidth

`real OutlineShaderSetLineWidth(real shader, real width);`

Задает толщину контура в контурном шейдере.

- `shader` - указатель на шейдер
- `width` - ширина.

---

## TexCombineShaderCreate

`real TexCombineShaderCreate(real matlib);`

Создает шейдер комбинирования текстур и возвращает указатель на него. Этот шейдер позволяет запрограммировать простейшие операции с текстурами: суммирование, вычитание, умножение и т.д. - это что-то наподобие языка описания шейдеров, который работает только с текстурами. TexCombineShader поддерживает до 4 текстур - первые две задаются автоматически (это первая и вторая текстуры первого материала в библиотеке), третья и четвертая задаются вручную соответствующими функциями.

- `matlib` - указатель на библиотеку материалов, из которой будут читаться текстуры.

---

## TexCombineShaderMaterial3

`real TexCombineShaderMaterial3(real shader, real material);`

Задает материал, из которого нужно прочитать третью текстуру для шейдера комбинирования текстур.

- `shader` - указатель на шейдер
- `material` - имя материала.

---

## TexCombineShaderMaterial4

`real TexCombineShaderMaterial4(real shader, real material);`

Задает материал, из которого нужно прочитать четвертую текстуру для шейдера комбинирования текстур.

- `shader` - указатель на шейдер
- `material` - имя материала.

---

## TexCombineShaderAddCombiner

`real TexCombineShaderAddCombiner(real shader, string instruction);

Добавляет новый комбинатор - то есть, инструкцию для комбинирования текстур. Синтаксис инструкции напоминает операцию присваивания в Pascal. Доступны четыре обозначения текстурных слоев (`"Tex0"`, `"Tex1"`, `"Tex2"`, `"Tex3"`), к которым можно добавлять суффиксы `".а"` и `".rgb"` для получения по отдельности альфа-канала и значений RGB, соответственно. Также поддерживаются идентификатор `PrimaryColor` (или просто `Col`) - основной цвет (освещенность вершины), функции `Interpolate` (интерполяция между двумя текстурами) и `Dot3` (скалярное произведение текстур).

Результатом работы шейдера будет комбинатор с наибольшим индексом.

- `shader` - указатель на шейдер
- `instruction` - строка, содержащая инструкцию.

Вот несколько примеров инструкций:

`"Tex1:=Tex0;"` - закрашивание первой текстурой<br>
`"Tex1:=Tex0+Tex1;"` - суммирование первых двух текстур<br>
`"Tex1:=Tex0-Tex1;"` - вычитание второй текстуры из первой<br>
`"Tex1:=Tex0*Tex1;"` - умножение первых двух текстур<br>
`"Tex1:=Interpolate(Tex0,Tex1,PrimaryColor);"` - интерполяция между первыми двумя текстурами с использованием освещенности в качестве параметра (то есть, текстура на освещенной стороне будет плавно переходить в текстуру на теневой стороне)<br>
`"Tex1:=Dot3(Tex0,Tex1);"` - скалярное произведение первых двух текстур.

---

## PhongShaderCreate

`real PhongShaderCreate();`

Создает шейдер Фонга, реализующий попиксельное освещение с бликами по Блинну-Фонгу (Blinn-Phong), и возвращает указатель на него.

Данный шейдер требует поддержки расширений `GL_ARB_shading_language` (или `GL_ARB_shading_language_100`), `GL_ARB_shader_objects`, `GL_ARB_vertex_shader`, `GL_ARB_fragment_shader`.

---

## PhongShaderUseTexture

`real PhongShaderUseTexture(real shader, real mode);`

Определяет, учитывать ли текстуру в шейдере Фонга. Используется текстура материала, к которому подключен шейдер (текстура в слоте 0). Помимо цвета, текстура определяет также прозрачность объекта, если у нее есть альфа-канал. Если эта опция отключена, для задания прозрачности используется значение альфа диффузной компоненты материала, к которому подключен шейдер. 

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно). По умолчанию текстура выключена.

---

## PhongShaderSetMaxLights

`real PhongShaderSetMaxLights(real shader, real maxlights);`

Задает количество источников света, которые должны учитываться шейдером Фонга. Это количество, как правило, меньше или равно числу источников света, созданных функцией `LightCreate`. Соответственно, оно ограничено восемью - по максимальному количеству встроенных источников света OpenGL. Шейдер поддерживает все типы источников света: `lsOmni`, `lsSpot`, `lsParallel`.

Обратите также внимание, что состояние включенности/выключенности источников света, задаваемое функцией `LightSetShining`, игнорируется шейдером. Чтобы выключить источник света, можно функцией `LightSetAttenuation` выставить в ноль его постоянную составляющую затухания (constant attenuation)

- `shader` - указатель на шейдер
- `maxlights` - количество источников света. Значение по умолчанию: 1.

---

## GLSLShaderCreate

`real GLSLShaderCreate(string vertexshader, string fragmentshader);`

Создает шейдер GLSL и возвращает указатель на него. GLSL (OpenGL Shading Language) - это язык описания шейдеров, при помощи которого вы можете запрограммировать графический конвейер (иными словами, управлять рендерингом объектов на вершинном и пиксельном уровне). За обработку вершин отвечает вершинная программа, за обработку пикселей - фрагментная.

Данный шейдер требует поддержки расширений `GL_ARB_shading_language` (или `GL_ARB_shading_language_100`), `GL_ARB_shader_objects`, `GL_ARB_vertex_shader`, `GL_ARB_fragment_shader`.

- `vertexshader` - строка, содержащая вершинную программу GLSL (не имя файла!)
- `fragmentshader` - строка, содержащая фрагментную программу GLSL (не имя файла!)

Если в функцию будет передана некорректная программа, будет выведено сообщение об ошибке, и движок продолжит работу. Ошибки выводятся в логгер, который задается функцией `GLSLShaderSetLogger`.

---

## GLSLShaderSetLogger

`real GLSLShaderSetLogger(real shader, real logger);`

Задает логгер, в который будут выводиться ошибки компиляции шейдера GLSL.

- `shader` - указатель на шейдер
- `logger` - указатель на логгер.

---

## GLSLShaderForceDisableStencilTest

`real GLSLShaderForceDisableStencilTest(real shader, real mode);`

Отключает стенсильный тест для шейдера GLSL.

- `shader` - указатель на шейдер
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## GLSLShaderSetOptions

`real GLSLShaderSetOptions(real shader, real lightingEnabled, real fogEnabled);`

Переключает использование освещения и тумана для шейдера GLSL.

- `shader` - указатель на шейдер
- `lightingEnabled` - `true` или `false` (1 и 0 соответственно)
- `fogEnabled` - `true` или `false` (1 и 0 соответственно).

---

## GLSLShaderCreateParameter

`real GLSLShaderCreateParameter(real shader, real name);`

Создает новый параметр шейдера и возвращает указатель на него. Эти параметры соответствуют uniform-переменным GLSL. Их можно модифицировать динамически в вашей игре, но в шейдере они доступны только для чтения.

- `shader` - указатель на шейдер
- `name` - имя параметра.

---

## GLSLShaderSetParameter1i

`real GLSLShaderSetParameter1i(real param, real value);`

Передает в параметр шейдера значение типа `int` (целое число).

- `param` - указатель на параметр
- `value` - значение параметра. Если указать дробное число, то оно будет округлено вниз до ближайшего целого.

---

## GLSLShaderSetParameter1f

`real GLSLShaderSetParameter1f(real param, real value);`

Передает в параметр шейдера значение типа `float` (вещественное число).

- `param` - указатель на параметр
- `value` - значение параметра.

---

## GLSLShaderSetParameter2f

`real GLSLShaderSetParameter2f(real param, real x, real y);`

Передает в параметр шейдера значение типа `vec2` (вектор из двух вещественных чисел).

- `param` - указатель на параметр
- `x`, `y` - координаты вектора.

---

## GLSLShaderSetParameter3f

`real GLSLShaderSetParameter3f(real param, real x, real y, real z);`

Передает в параметр шейдера значение типа `vec3` (вектор из трех вещественных чисел).

- `param` - указатель на параметр
- `x`, `y`, `z` - координаты вектора.

---

## GLSLShaderSetParameter4f

`real GLSLShaderSetParameter4f(real param, real x, real y, real z, real w);`

Передает в параметр шейдера значение типа `vec4` (вектор из четырех вещественных чисел).

- `param` - указатель на параметр
- `x`, `y`, `z`, `w` - координаты вектора.

---

## GLSLShaderSetParameterTexture

`real GLSLShaderSetParameterTexture(real param, string material, real texunit);`

Передает в параметр шейдера текстуру заданного материала. Тип текстуры (`sampler2D`, `samplerCube` и пр.) устанавливается автоматически.

- `param` - указатель на параметр
- `material` - имя материала (в активной библиотеке материалов). Если вместо имени указана пустая строка, то будет использована текстура материала, к которому подключен шейдер (текстура в слоте с индексом texunit). Таким образом, один и тот же шейдер может работать с разными текстурами в зависимости от материала, что в некоторых ситуациях очень удобно.
- `texunit` - определяет, через какой текстурный блок передавать текстуру. Стандарт OpenGL гаранирует 8 доступных текстурных блоков (0-7) - у современных видеокарт их может быть и больше (до 16 и даже 32), но для лучшей совместимости лучше не использовать больше 8. При автоматическом использовании текстуры материала (см. пояснение к предыдущему аргументу), номер текстурного блока соответствует текстурному слоту материала, а их количество также равно 8. В одном шейдере нельзя передавать две разные текстуры через один и тот же текстурный блок.

---

## GLSLShaderSetParameterSecondTexture

`real GLSLShaderSetParameterSecondTexture(real param, string material, real texunit);`

Передает в параметр шейдера вторую текстуру заданного материала. Тип текстуры (`sampler2D`, `samplerCube` и пр.) устанавливается автоматически.

- `param` - указатель на параметр
- `material` - имя материала (в активной библиотеке материалов). Если вместо имени указана пустая строка, то будет использована вторая текстура материала, к которому подключен шейдер. 
- `texunit` - определяет, через какой текстурный блок передавать текстуру.

---

## GLSLShaderSetParameterHasTextureEx

`real GLSLShaderSetParameterHasTextureEx(real param, real index);`

Передает в параметр шейдера (`int` или `bool`) единицу, если у материала, к которому подключен шейдер, есть текстура в заданном текстурном блоке, и ноль в противном случае.

- `param` - указатель на параметр
- `index - индекс текстурного слота (от 0 до 7 включительно).

---

## GLSLShaderSetParameterMatrix

`real GLSLShaderSetParameterMatrix(real param, real object);`

Передает в параметр шейдера абсолютную матрицу трансформации заданного объекта (тип `mat4`).

- `param` - указатель на параметр
- `object` - указатель на объект.

---

## GLSLShaderSetParameterInvMatrix

`real GLSLShaderSetParameterInvMatrix(real param, real object);`

Передает в параметр шейдера обратную абсолютную матрицу трансформации заданного объекта (тип `mat4`).

- `param` - указатель на параметр
- `object` - указатель на объект.

---

## GLSLShaderSetParameterViewMatrix

`real GLSLShaderSetParameterViewMatrix(real param);`

Передает в параметр шейдера текущую видовую матрицу (тип `mat4`), которая переносит точки и векторы из мирового пространства в видовое, где началом координат является позиция камеры, через которую совершается рендеринг. Параметр автоматически обновляется при трансформации камеры.

- `param` - указатель на параметр.

---

## GLSLShaderSetParameterInvViewMatrix

`real GLSLShaderSetParameterInvViewMatrix(real param);`

Передает в параметр шейдера текущую обратную видовую матрицу (тип `mat4`), которая переносит точки и векторы из видового пространства в мировое. Параметр автоматически обновляется при трансформации камеры.

- `param` - указатель на параметр.

---

## GLSLShaderSetParameterShadowTexture

`real GLSLShaderSetParameterShadowTexture(real param, real shadowmap, real texunit);`

Передает в параметр шейдера текстуру буфера теневой карты. В GLSL она имеет тип `sampler2DShadow`. С помощью этой текстуры можно реализовать в шейдере рендеринг теней.

- `param` - указатель на параметр
- `shadowmap` - указатель на теневую карту
- `texunit` - определяет, через какой текстурный блок передавать текстуру.

---

## GLSLShaderSetParameterShadowMatrix

`real GLSLShaderSetParameterShadowMatrix(real param, real shadowmap);`

Передает в параметр шейдера теневую матрицу из теневой карты (тип `mat4`). Эта матрица отображает векторы модельно-видового пространства в пространство отсечения теневого буфера.

- `param` - указатель на параметр
- `shadowmap` - указатель на теневую карту.

---

## GLSLShaderSetParameterFBOColorTexture

`real GLSLShaderSetParameterFBOColorTexture(real param, real fbo, real texunit);`

Передает в параметр шейдера текстуру цветового буфера FBO.

- `param` - указатель на параметр
- `fbo` - указатель на FBO
- `texunit` - определяет, через какой текстурный блок передавать текстуру.

---

## GLSLShaderSetParameterFBODepthTexture

`real GLSLShaderSetParameterFBODepthTexture(real param, real fbo, real texunit);`

Передает в параметр шейдера текстуру Z-буфера FBO.

- `param` - указатель на параметр
- `fbo` - указатель на FBO
- `texunit` - определяет, через какой текстурный блок передавать текстуру.
