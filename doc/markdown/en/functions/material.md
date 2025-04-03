# Material

Материал - это совокупность параметров поверхности объекта. Параметры включают в себя текстуру, цвет, смешивание, фильтрацию и т.д. Можно создать один материал и применить его к нескольким объектам - изменения, внесенные в параметры материала, автоматически коснутся всех этих объектов.

Библиотекой материалов (Material Library) называется специальная конструкция, содержащая список материалов. Библиотека может быть активной или неактивной. В первом случае в нее можно добавлять материалы, настраивать их и применять к объектам. Во втором случае это невозможно. Поскольку разные библиотеки могут содержать материалы с одинаковыми именами, нельзя сделать активными две или более библиотеки одновременно.

В связи с тем, что в Xtreme3D 3.x появилась поддержка шейдеров GLSL, был переработан механизм мультитекстурирования (TextureEx). Теперь каждый материал может иметь вплоть до 8 текстур - а, точнее, текстурных слотов, которым могут быть присвоены текстуры. Слоты нумеруются от 0 до 7 - их индексы соответствуют текстурным блокам, которые передаются в параметры шейдерам GLSL. При этом поддержка двух основных текстур материала также сохранилась в целях обеспечения обратной совместимости - они используются в том случае, если не присвоены текстуры слотов 0 и/или 1. В противном случае основные текстуры игнорируются, и материал использует текстуры из слотов 0 и/или 1.

Таким образом, шейдер может автоматически использовать текстуры материала, к которому присвоен - и это будут не только привычная первая (диффузная карта) и вторая (карта нормалей или освещения), но и еще 6 произвольных. Правда, большинство стандартных форматов 3D-моделей не поддерживают такой вид мультитекстурирования - если вы хотите загружать в Xtreme3D модели со сложными многотекстурными материалами, вам придется для этого использовать специализированный формат и написать свой загрузчик для него. 

---

## MaterialLibraryCreate

`real MaterialLibraryCreate();`

Создает библиотеку материалов и возвращает указатель на нее.

---

## MaterialLibraryActivate

`real MaterialLibraryActivate(real matlib);`

Активирует библиотеку материалов. Все остальные библиотеки автоматически становятся неактивными.

- `matlib` - указатель на библиотеку материалов.

---

## MaterialLibrarySetTexturePaths

`real MaterialLibrarySetTexturePaths(real matlib, string path);`

Указывает библиотеке, откуда по умолчанию загружать текстуры при загрузке модели, содержащей данные о текстурах.

- `matlib` - указатель на библиотеку материалов
- `path` - путь к текстурам.

---

## MaterialLibraryClear

`real MaterialLibraryClear(real matlib);`

Очищает библиотеку, удаляя из нее все материалы.

- `matlib` - указатель на библиотеку материалов.

---

## MaterialLibraryDeleteUnused

`real MaterialLibraryDeleteUnused(real matlib);`

Удаляет из библиотеки неиспользуемые материалы.

- `matlib` - указатель на библиотеку материалов.

---

## MaterialLibraryHasMaterial

`real MaterialLibraryHasMaterial(real matlib, string material);`

Возвращает истину, если в библиотеке есть материал с заданным именем.

- `matlib` - указатель на библиотеку материалов
- `material` - имя материала.

---

## MaterialLibraryGetTextureByName

`real MaterialLibraryGetTextureByName(real matlib, string material);`

Возвращает указатель на текстуру материала по имени материала в заданной библиотеке материалов, либо 0, если указанного материала не существует.

- `matlib` - указатель на библиотеку материалов
- `material` - имя материала.

---

## MaterialLibraryLoadScript

`real MaterialLibraryLoadScript(real matlib, string filename);`

Загружает скрипт материала - то есть, создает в библиотеке новый материал согласно текстовому описанию из файла.

- `matlib` - указатель на библиотеку материалов
- `filename` - имя файла скрипта.

---

## MaterialCreate

`real MaterialCreate(string material, string filename);`

Создает новый материал в активной библиотеке. Можно указать путь к файлу текстуры. Xtreme3D поддерживает форматы BMP, JPEG, PNG, TGA, DDS (DXT1, DXT3, DXT5). Если оставить это значение пустым, будет создан материал без текстуры.

- `material` - имя материала
- `filename` - путь к файлу текстуры.

---

## MaterialDestroy

`real MaterialDestroy(string material);`

Удаляет материал из активной библиотеки.

- `material` - имя материала.

---

## MaterialAddCubeMap

`real MaterialAddCubeMap(string material);`

Создает материал для кубического проецирования (cubemapping). В таком материале используется шесть текстур, формирующих шесть сторон воображаемого куба, окружающего объект. В результате этого создается эффект полного отражения объектом окружающего пространства. Cubemap-материалы обычно используются для отражающих материалов, таких, как металл.

- `material` - имя cubemap-материала.

---

## MaterialCubeMapLoadImage

`real MaterialCubeMapLoadImage(string material, string texture, real index);`

Загружает текстуру для заданной стороны куба в cubemap-материале.

- `material` - имя cubemap-материала
- `texture` - путь к файлу текстуры.
- `index` - индекс стороны куба:
    - 0 - левая
    - 1 - правая
    - 2 - верхняя
    - 3 - нижняя
    - 4 - задняя
    - 5 - передняя.

---

## MaterialCubeMapGenerate

`real MaterialCubeMapGenerate(string material, real res);`

Генерирует текстуры для cubemap-материала.

- `material` - имя cubemap-материала
- `res` - сторона текстуры. Рекомендуется выставить ее равной степени двойки (64, 128, 256 и т.д.).

---

## MaterialCubeMapFromScene

`real MaterialCubeMapFromScene(string material, real viewer, real camera, real res);`

Совершает рендеринг текстур для куба в cubemap-материале через заданную камеру и с настройками заданного вида. При этом следует указать разрешение рендеринга по стороне (512, 256, 128 и т.д.).

Эта функция позволяет получить точное отражение сцены. Но пользоваться ей надо осторожно, так как при высоких разрешениях cubemap-материала может значительно упасть производительность.

- `material` - имя cubemap-материала
- `viewer` - указатель на вид
- `camera` - указатель на камеру
- `res` - разрешение.

---

## MaterialSetName

`real MaterialSetName(string material, string name);`

Задает имя материалу.

- `material` - имя материала
- `name` - новое имя материала.

---

## MaterialSetAmbientColor

`real MaterialSetAmbientColor(string material, real color, real alpha);`

Задает цветовой компонент Аmbient. Объект полностью закрашивается этим цветом (если включено освещение, цвет Ambient останется только в теневой части объекта - иными словами, этот компонент соответствует цвету тени).

- `material` - имя материала
- `color` - цвет
- `alpha` - прозрачность.

---

## MaterialSetDiffuseColor

`real MaterialSetDiffuseColor(string material, real color, real alpha);`

Задает цветовой компонент Diffuse. Этим цветом закрашивается только освещенная часть объекта.

- `material` - имя материала
- `color` - цвет
- `alpha` - прозрачность.

---

## MaterialSetEmissionColor

`real MaterialSetEmissionColor(string material, real color, real alpha);`

Задает цветовой компонент Emission. Используется для имитации самосвечения объекта (суммируется с остальными компонентами, делая объект более ярким).

- `material` - имя материала
- `color` - цвет
- `alpha` - прозрачность.

---

## MaterialSetSpecularColor

`real MaterialSetSpecularColor(string material, real color, real alpha);`

Задает цветовой компонент Specular, соответствующий цвету блика (блик возникает при прямом отражении световых лучей поверхностью объекта).

- `material` - имя материала
- `color` - цвет
- `alpha` - прозрачность.

---

## MaterialGetColor

`real MaterialGetColor(string material, real index);`

Возвращает цветовой компонент материала.

- `material` - имя материала
- `index` - индекс компонента (0 = Ambient, 1 = Diffuse, 2 = Specular, 3 = Emission).

---

## MaterialGetAlpha

`real MaterialGetAlpha(string material, real index);`

Возвращает значение прозрачности (alpha) цветового компонента материала.

- `material` - имя материала
- `index` - индекс компонента (0 = Ambient, 1 = Diffuse, 2 = Specular, 3 = Emission).

---

## MaterialSetShininess

`real MaterialSetShininess(string material, real shininess);`

Задает уровень зеркального блеска. Параметр управляет бликом (Specular): чем выше это значение, тем меньше будет размытость блика.

- `material` - имя материала
- `shininess` - уровень блеска (значение от 0 до 128).

---

## MaterialSetPolygonMode

`real MaterialSetPolygonMode(string material, real pm);`

Задает режим отображения полигонов материала.

- `material` - имя материала
- `pm` - режим отображения полигонов. Доступны следующие значения `pm`:
    - `pmFill` = 0 - полигоны в виде плоскостей (значение по умолчанию)
    - `pmLines` = 1 - полигоны в виде образующих линий (каркасное отображение)
    - `pmPoints` = 2 - полигоны в виде образующих точек.

---

## MaterialSetTexture

`real MaterialSetTexture(string material1, string material2);`

Копирует текстуру из материала 2 в материал 1.

- `material1` - имя материала 1
- `material2` - имя материала 2.

---

## MaterialSetTextureMappingMode

`real MaterialSetTextureMappingMode(string material, real tmm);`

Задает режим проецирования текстуры материала.

- `material` - имя материала
- `tmm` - режим отображения полигонов. Доступны следующие значения `tmm`:
    - `tmmUser` = 0 - проецирование по умолчанию (определяется разверткой объекта)
    - `tmmObjectLinear` = 1 - линейное проецирование, фиксированное по отношению к объекту
    - `tmmEyeLinear` = 2 - линейное проецирование, фиксированное по отношению к сцене
    - `tmmSphere` = 3 - сферическое проецирование
    - `tmmCubeMapReflection` = 4 - кубическое проецирование
    - `tmmCubeMapNormal` = 5 - кубическое проецирование, фиксированное по отношению к нормали поворота
    - `tmmCubeMapLight0` = 6 - сведения отсутствуют
    - `tmmCubeMapCamera` = 7 - сведения отсутствуют.

---

## MaterialSetTextureImageAlpha

`real MaterialSetTextureImageAlpha(string material, real tia);`

Задает режим прозрачности текстуры материала.

- `material` - имя материала
- `tia` - режим прозрачности текстуры. Доступны следующие значения `tia`:
    - `tiaDefault` = 0 - прозрачность по умолчанию (определяется альфа-каналом)
    - `tiaAlphaFromIntensity` = 1 - прозрачность выделяется из интенсивности цвета (чем темнее, тем прозрачнее)
    - `tiaSuperBlackTransparent` = 2 - текстура прозрачна в абсолютно черных пикселях (0, 0, 0)
    - `tiaLuminance` = 3 - сведения отсутствуют
    - `tiaLuminanceSqrt` = 4 - сведения отсутствуют
    - `tiaOpaque` = 5 - полная непрозрачность
    - `tiaTopLeftPointColorTransparent` = 6 - текстура прозрачна в местах одного цвета с левым нижним углом изображения
    - `tiaInverseLuminance` = 7 - сведения отсутствуют
    - `tiaInverseLuminanceSqrt` = 8 - сведения отсутствуют
    - `tiaBottomRightPointColorTransparent` = 9 - текстура прозрачна в местах одного цвета с правым нижним углом изображения.

---

## MaterialSetTextureScale

`real MaterialSetTextureScale(string material, real u, real v);`

Задает масштаб текстуры материала.

- `material` - имя материала
- `xscale`, `yscale` - масштаб по горизонтали и вертикали.

---

## MaterialSetTextureOffset

`real MaterialSetTextureOffset(string material, real x, real y);`

Задает смещение текстуры материала.

- `material` - имя материала
- `x`, `y` - смещение по горизонтали и вертикали.

---

## MaterialSetTextureMode

`real MaterialSetTextureMode(string material, real tm);`

Задает режим текстуры материала.

- `material` - имя материала
- `tm` - режим текстуры. Доступны следующие значения tm:
    - `tmDecal` = 0 - замещение с прозрачностью
    - `tmModulate` = 1 - замещение с прозрачностью и цветовым смешиванием (используется компонент Diffuse)
    - `tmBlend` = 2 - сведения отсутствуют
    - `tmReplace` = 3 - полное замещение (без прозрачности и смешивания)

---

## MaterialSetBlendingMode

`real MaterialSetBlendingMode(string material, real bm);`

Задает режим смешивания материала.

- `material` - имя материала
- `bm` - режим смешивания. Доступны следующие значения bm:
    - `bmOpaque` = 0 - отсутствие прозрачности и смешивания (полное замещение)
    - `bmTransparency` = 1 - используется прозрачность или альфа-канал
    - `bmAdditive` = 2 - используется суммирование цветов
    - `bmAlphaTest50` = 3 - используется альфа-тест на 50% (прозрачность ниже 0.5 трактуется как полная прозрачность, выше - полная непрозрачность)
    - `bmAlphaTest100` = 4 - используется альфа-тест на 100% (прозрачность ниже 1.0 трактуется как полная прозрачность, выше - полная непрозрачность)
    - `bmModulate` = 5 - используется модуляция (умножение) цветов.

---

## MaterialSetTextureFilter

`real MaterialSetTextureFilter(string material, real mi, real ma);`

Задает режим фильтрации сэмплинга текстуры при минификации и максимизации. Минификация происходит, когда пиксель текстуры становится меньше, чем пиксель на экране. Максимизация, соответственно, наоборот - когда пиксель текстуры больше, чем пиксель на экране.

- `material` - имя материала
- `mi` - фильтр минификации. Доступны следующие значения `mi`:
    - `miNearest` = 0 - фильтрация методом ближайшего соседа
    - `miLinear` = 1 - линейная фильтрация
    - `miNearestMipmapNearest` = 2 - сведения отсутствуют
    - `miLinearMipmapNearest` = 3 - сведения отсутствуют
    - `miNearestMipmapLinear` = 4 - сведения отсутствуют
    - `miLinearMipmapLinear` = 5 - сведения отсутствуют
- `ma` - фильтр максимизации. Доступны следующие значения `ma`:
    - `maNearest` = 0 - фильтрация методом ближайшего соседа
    - `maLinear` = 1 - линейная фильтрация.

---

## MaterialEnableTexture

`real MaterialEnableTexture(string material, real mode);`

Включает или выключает отображение текстуры материала.

- `material` - имя материала
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## MaterialLoadTexture

`real MaterialLoadTexture(string material, string filename);`

Загружает в материал текстуру.

- `material` - имя материала
- `filename` - путь к файлу текстуры.

---

## MaterialGetCount

`real MaterialGetCount();`

Возвращает общее количество материалов в активной библиотеке.

---

## MaterialGetName

`string MaterialGetName(real index);`

Возвращает имя материала заданного номера.

- `index` - порядковый номер материала в библиотеке.

---

## MaterialGetNameFromLibrary

`string MaterialGetNameFromLibrary(real matlib, real index);`

Возвращает имя материала заданного номера в заданной библиотеке материалов.

- `matlib` - указатель на библиотеку материалов
- `index` - порядковый номер материала в библиотеке.

---

## MaterialSetFaceCulling

`real MaterialSetFaceCulling(string material, real fc);`

Задает режим отбора видимости полигонов.

- `material` - имя материала
- `fc` - режим отбора видимости. Доступны следующие значения `fc`:
    - `fcBufferDefault` = 0 - значение по умолчанию для буфера кадра
    - `fcCull` = 1 - отбор видимости включен
    - `fcNoCull` = 2 - отбор видимости выключен.

---

## MaterialSetSecondTexture

`real MaterialSetSecondTexture(string material1, string material2);`

Загружает материалу 1 вторую текстуру из материала 2. Вторая текстура ложится поверх первой и использует второй набор текстурных координат (если его нет, используется первый).

- `material1` - имя материала 1
- `material2` - имя материала 2.

---

## MaterialSetTextureFormat

`real MaterialSetTextureFormat(string material, real tf);`

Задает формат хранения текстуры в видеопамяти.

- `material` - имя материала
- `tf` - формат текстуры. Доступны следующие значения `tf`:
    - `tfDefault` = 0 - формат по умолчанию (`tfRGBA`)
    - `tfRGB` = 1 - 24-битный RGB, 8 бит на компонент, без альфа-канала
    - `tfRGBA` = 2 - 32-битный RGBA, 8 бит на компонент, включает альфа-канал
    - `tfRGB16` = 3 - 16-битный RGB, без альфа-канала
    - `tfRGBA16` = 4 - 16-битный RGBA, включает альфа-канал
    - `tfAlpha` = 5 - только 8-битный альфа-канал без цвета
    - `tfLuminance` = 6 - только 8-битный компонент яркости, без альфа-канала
    - `tfLuminanceAlpha` = 7 - только 8-битный компонент яркости, включает альфа-канал
    - `tfIntensity` = 8 - только 8-битный компонент насыщенности, без альфа-канала
    - `tfNormalMap` = 9 - 24-битная RGB карта нормалей, генерируемая из исходной текстуры
    - `tfRGBAFloat16` = 10 - 16-битный RGBA с плавающей запятой, включает альфа-канал
    - `tfRGBAFloat32` = 11 - 32-битный RGBA с плавающей запятой, включает альфа-канал
    - `tfExtended` = 12 - формат задается из расширенного списка функцией `MaterialSetTextureFormatEx`.

---

## MaterialSetTextureFormatEx

`real MaterialSetTextureFormatEx(string material, real tfex)`

Задает расширенный формат хранения текстуры в видеопамяти.

- `material` - имя материала
- `tfex` - формат текстуры. Доступны следующие значения `tfex`:
    - `tfALPHA4` = 0
    - `tfALPHA8` = 1
    - `tfALPHA12` = 2
    - `tfALPHA16` = 3
    - `tfDEPTHCOMPONENT16` = 4
    - `tfDEPTHCOMPONENT24` = 5
    - `tfDEPTHCOMPONENT32` = 6
    - `tfLUMINANCE4` = 7
    - `tfLUMINANCE8` = 8
    - `tfLUMINANCE12` = 9
    - `tfLUMINANCE16` = 10
    - `tfLUMINANCE4ALPHA4` = 11
    - `tfLUMINANCE6ALPHA2` = 12
    - `tfLUMINANCE8ALPHA8` = 13
    - `tfLUMINANCE12ALPHA4` = 14
    - `tfLUMINANCE12ALPHA12` = 15
    - `tfLUMINANCE16ALPHA16` = 16
    - `tfINTENSITY4` = 17
    - `tfINTENSITY8` = 18
    - `tfINTENSITY12` = 19
    - `tfINTENSITY16I` = 20
    - `tfR3G3B2` = 21
    - `tfRGB4` = 22
    - `tfRGB5` = 23
    - `tfRGB8` = 24
    - `tfRGB10` = 25
    - `tfRGB12` = 26
    - `tfR16G16B16` = 27
    - `tfRGBA2` = 28
    - `tfRGBA4` = 29
    - `tfRGB5A1` = 30
    - `tfRGBA8` = 31
    - `tfRGB10A2` = 32
    - `tfRGBA12` = 33
    - `tfR16G16B16A16` = 34
    - `tfCOMPRESSEDRGBS3TCDXT1` = 35
    - `tfCOMPRESSEDRGBAS3TCDXT1` = 36
    - `tfCOMPRESSEDRGBAS3TCDXT3` = 37
    - `tfCOMPRESSEDRGBAS3TCDXT5` = 38
    - `tfSIGNEDLUMINANCE8` = 39
    - `tfSIGNEDLUMINANCE8ALPHA8` = 40
    - `tfSIGNEDRGB8` = 41
    - `tfSIGNEDRGBA8` = 42
    - `tfSIGNEDRGB8UNSIGNEDALPHA8` = 43
    - `tfSIGNEDALPHA8` = 44
    - `tfSIGNEDINTENSITY8` = 45
    - `tfHILO16` = 46
    - `tfSIGNEDHILO16` = 47
    - `tfDSDT8` = 48
    - `tfDSDT8MAG8` = 49
    - `tfDSDT8MAG8INTENSITY8` = 50
    - `tfHILO8` = 51
    - `tfSIGNEDHILO8` = 52
    - `tfFLOATR16` = 53
    - `tfFLOATR32` = 54
    - `tfFLOATRG16` = 55
    - `tfFLOATRGB16` = 56
    - `tfFLOATRGBA16` = 57
    - `tfFLOATRG32` = 58
    - `tfFLOATRGB32` = 59
    - `tfFLOATRGBA32` = 60
    - `tfRGBAFLOAT32` = 61
    - `tfRGBFLOAT32` = 62
    - `tfALPHAFLOAT32` = 63
    - `tfINTENSITYFLOAT32` = 64
    - `tfLUMINANCEFLOAT32` = 65
    - `tfLUMINANCEALPHAFLOAT32` = 66
    - `tfRGBAFLOAT16` = 67
    - `tfRGBFLOAT16` = 68
    - `tfALPHAFLOAT16` = 69
    - `tfINTENSITYFLOAT16` = 70
    - `tfLUMINANCEFLOAT16` = 71
    - `tfLUMINANCEALPHAFLOAT16` = 72
    - `tfDEPTH24STENCIL8` = 73
    - `tfDEPTHCOMPONENT32F` = 74
    - `tfDEPTH32FSTENCIL8` = 75
    - `tfSRGB8` = 76
    - `tfSRGB8ALPHA8` = 77
    - `tfSLUMINANCE8` = 78
    - `tfSLUMINANCE8ALPHA8` = 79
    - `tfCOMPRESSEDSRGBS3TCDXT1` = 80
    - `tfCOMPRESSEDSRGBALPHAS3TCDXT1` = 81
    - `tfCOMPRESSEDSRGBALPHAS3TCDXT3` = 82
    - `tfCOMPRESSEDSRGBALPHAS3TCDXT5` = 83
    - `tfRGB9E5` = 84
    - `tfR11FG11FB10F` = 85
    - `tfCOMPRESSEDLUMINANCELATC1` = 86
    - `tfCOMPRESSEDSIGNEDLUMINANCELATC1` = 87
    - `tfCOMPRESSEDLUMINANCEALPHALATC2` = 88
    - `tfCOMPRESSEDSIGNEDLUMINANCEALPHALATC2` = 89
    - `tfCOMPRESSEDLUMINANCEALPHA3DC` = 90
    - `tfRGBA32UI` = 91
    - `tfRGB32UI` = 92
    - `tfALPHA32UI` = 93
    - `tfINTENSITY32UI` = 94
    - `tfLUMINANCE32UI` = 95
    - `tfLUMINANCEALPHA32UI` = 96
    - `tfRGBA16UI` = 97
    - `tfRGB16UI` = 98
    - `tfALPHA16UI` = 99
    - `tfINTENSITY16UI` = 100
    - `tfLUMINANCE16UI` = 101
    - `tfLUMINANCEALPHA16UI` = 102
    - `tfRGBA8UI` = 103
    - `tfRGB8UI` = 104
    - `tfALPHA8UI` = 105
    - `tfINTENSITY8UI` = 106
    - `tfLUMINANCE8UI` = 107
    - `tfLUMINANCEALPHA8UI` = 108
    - `tfRGBA32I` = 109
    - `tfRGB32I` = 110
    - `tfALPHA32I` = 111
    - `tfINTENSITY32I` = 112
    - `tfLUMINANCE32I` = 113
    - `tfLUMINANCEALPHA32I` = 114
    - `tfRGBA16I` = 115
    - `tfRGB16I` = 116
    - `tfALPHA16I` = 117
    - `tfLUMINANCE16I` = 119
    - `tfLUMINANCEALPHA16I` = 120
    - `tfRGBA8I` = 121
    - `tfRGB8I` = 122
    - `tfALPHA8I` = 123
    - `tfINTENSITY8I` = 124
    - `tfLUMINANCE8I` = 125
    - `tfLUMINANCEALPHA8I` = 126
    - `tfRG32UI` = 127
    - `tfR32UI` = 128
    - `tfRG16UI` = 129
    - `tfR16UI` = 130
    - `tfRG8UI` = 131
    - `tfR8UI` = 132
    - `tfRG32I` = 133
    - `tfR32I` = 134
    - `tfRG16I` = 135
    - `tfR16I` = 136
    - `tfRG8I` = 137
    - `tfR8I` = 138
    - `tfRG8` = 139
    - `tfR8` = 140
    - `tfRG16` = 141
    - `tfR16` = 142
    - `tfRG16F` = 143
    - `tfR16F` = 144
    - `tfRG32F` = 145
    - `tfR32F` = 146
    - `tfCOMPRESSEDREDRGTC1` = 147
    - `tfCOMPRESSEDSIGNEDREDRGTC1` = 148
    - `tfCOMPRESSEDRGRGTC2` = 149
    - `tfCOMPRESSEDSIGNEDRGRGTC2` = 150
    - `tfR8SNORM` = 151
    - `tfRG8SNORM` = 152
    - `tfRGB8SNORM` = 153
    - `tfRGBA8SNORM` = 154
    - `tfR16SNORM` = 155
    - `tfRG16SNORM` = 156
    - `tfRGB16SNORM` = 157
    - `tfRGBA16SNOR` = 158

---

## MaterialSetTextureCompareMode

`real MaterialSetTextureCompareMode(string material, real tcm);`

Задает режим сравнения текстуры. Эта опция нужна только в одном случае - когда создается буфер глубины в FBO под теневую карту.

- `material` - имя материала
- `tcm` - режим сравнения текстуры. Доступны следующие значения `tcm`:
    - `tcmNone` - сравнение отключено;
    - `tcmCompareRtoTexture` - значение, сэмплируемое из текстуры, вычисляется при помощи теста глубины для значения, задаваемого в шейдерной функции `shadow2D`

---

## MaterialSetTextureDepthCompareFunc

`real MaterialSetTextureDepthCompareFunc(string material, real cf);`

Задает функцию сравнения теста глубины текстуры. Эта функция срабатывает, если текстура используется в качестве буфера глубины в FBO.

- `material` - имя материала
- `cf` - функция сравнения текстуры. Доступны следующие значения `cf`:
    - `cfNever` = 0 - тест всегда неуспешен;
    - `cfAlways` = 1 - тест всегда успешен;
    - `cfLess` = 2 - тест успешен, если глубина меньше, чем сэмпл из текстуры;
    - `cfLEqual` = 3 - тест успешен, если глубина меньше или равна сэмплу из текстуры;
    - `cfEqual` = 4 - тест успешен, если глубина равна сэмплу из текстуры;
    - `cfGreater` = 5 - тест успешен, если глубина больше, чем сэмпл из текстуры;
    - `cfNotEqual` = 6 - тест успешен, если глубина не равна сэмплу из текстуры;
    - `cfGEqual` = 7 - тест успешен, если глубина больше или равна сэмплу из текстуры.

---

## MaterialSetTextureCompression

`real MaterialSetTextureCompression(string material, real tc);`

Задает режим сжатия текстуры.

- `material` - имя материала
- `tc` - режим сжатия. Доступны следующие значения `tc`:
    - `tcDefault` = 0 - сжатие по умолчанию
    - `tcNone` = 1 - не использовать сжатие
    - `tcStandard` = 2 - стандартное сжатие (средние качество и скорость декомпрессии)
    - `tcHighQuality` = 3 - высокое качество, низкая скорость декомпрессии
    - `tcHighSpeed` = 4 - высокая скорость декомпрессии, низкое качество.

---

## MaterialTextureRequiredMemory

`real MaterialTextureRequiredMemory(string material);`

Возвращает объем памяти, занимаемый текстурой (в байтах).

- `material` - имя материала.

---

## MaterialSetFilteringQuality

`real MaterialSetFilteringQuality(string material, real tf);`

Задает режим фильтрации текстуры (не путать с `MaterialSetTextureFilter`). Фильтрация происходит, когда разрешение mip-уровня текстуры становится больше или меньше изображения-оригинала.

- `material` - имя материала
- `tf` - режим фильтрации. Доступны следующие значения `tf`:
    - `tfIsotropic` = 0 - изотропная фильтрация
    - `tfAnisotropic` = 1 - анизотропная фильтрация.

---

## MaterialSetOptions

`real MaterialSetOptions(string material, real ignorefog, real nolighting);`

Включает или выключает влияние тумана (ignorefog) и света (nolighting) на материал.

- `material` - имя материала
- `ignorefog` - игнорировать ли туман: `true` или `false` (1 и 0 соответственно)
- `nolighting` - отключить ли освещение: `true` или `false` (1 и 0 соответственно).

---

## MaterialSetShader

`real MaterialSetShader(string material, real shader);`

Применяет к материалу шейдер.

- `material` - имя материала
- `shader` - указатель на шейдер.

---

## MaterialSetTextureWrap

`real MaterialSetTextureWrap(string material, real mode);`

Переключает повторение текстуры, когда UV-координаты выходят за пределы диапазона [0..1].

- `material` - имя материала
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## MaterialGetTextureWidth

`real MaterialGetTextureWidth(string material);`

Возвращает ширину текстуры материала. Если у материала нет текстуры, функция возвращает 0.

- `material` - имя материала.

---

## MaterialGetTextureHeight

`real MaterialGetTextureHeight(string material);`

Возвращает высоту текстуры материала. Если у материала нет текстуры, функция возвращает 0.

- `material` - имя материала.

---

## MaterialGenTexture

`real MaterialGenTexture(string material, real width, real height);`

Создает пустую текстуру у материала.

- `material` - имя материала
- `width`, `height` - ширина и высота текстуры.

---

## MaterialLoadTexture

`real MaterialLoadTexture(string material, string filename);`

Загружает материалу текстуру из файла.

- `material` - имя материала
- `filename` - путь к файлу изображения.

---

## MaterialSaveTexture

`real MaterialSaveTexture(string material, string filename);`

Сохраняет текстуру материала в файл BMP.

- `material` - имя материала
- `filename` - имя файла.

---

## MaterialAddTextureEx

`real MaterialAddTextureEx(string material, real index);`

Добавляет материалу текстуру в заданный текстурный слот и возвращает указатель на нее. Всего доступно 8 слотов. Первые два слота (0 и 1) заменяют первую и вторую собственные текстуры материала.

- `material` - имя материала
- `index` - индекс текстурного слота (от 0 до 7 включительно).

---

## MaterialHasTextureEx

`real MaterialHasTextureEx(string material, real index);`

Возвращает 1, если у материала есть текстура в заданном текстурном слоте, и 0 в противном случае.

- `material` - имя материала
- `index` - индекс текстурного слота (от 0 до 7 включительно).

---

## MaterialGetTextureEx

`real MaterialGetTextureEx(string material, real index);`

Возвращает текстуру материала в заданном текстурном слоте, либо 0, если ее не существует.

- `material` - имя материала
- `index` - индекс текстурного слота (от 0 до 7 включительно).

---

## MaterialTextureExClear

`real MaterialTextureExClear(string material);`

Очищает текстуры материала во всех слотах.

- `material` - имя материала.

---

## MaterialSetDepthWrite

`real MaterialSetDepthWrite(string material, real mode);`

Включает или выключает запись в буфер глубины.

- `material` - имя материала
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## MaterialSetDepthTest

`real MaterialSetDepthTest(string material, real mode);`

Включает или выключает тест глубины - то есть, проверку перекрытия объекта другими объектами, уже отрисованными.

- `material` - имя материала
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## MaterialNoiseCreate

`real MaterialNoiseCreate(string material);`

Создает материал с процедурно сгенерированной текстурой шума Перлина.

- `material` - имя материала.

---

## MaterialNoiseSetDimensions

`real MaterialNoiseSetDimensions(string material, real width, real height);`

Задает разрешение текстуры шума.

- `material` - имя материала
- `width`, `height` - ширина и высота текстуры.

---

## MaterialNoiseAnimate

`real MaterialNoiseAnimate(string material, real speed);`

Анимирует текстуру шума (генерирует новое изображение, близкое к изначальному).

- `material` - имя материала
- `speed` - скорость.

---

## MaterialNoiseSetMinCut

`real MaterialNoiseSetMinCut(string material, real mincut);`

Сведения отсутствуют.

- `material` - имя материала
- `mincut` - сведения отсутствуют.

----

MaterialNoiseSetSharpness

`real MaterialNoiseSetSharpness(string material, real sharpness);`

Сведения отсутствуют.

- `material` - имя материала
- `sharpness` - сведения отсутствуют.

---

## MaterialNoiseSetSeamless

`real MaterialNoiseSetSeamless(string material, real mode);`

Переключает бесшовность текстуры шума.

- `material` - имя материала
- `mode` - `true` или `false` (1 и 0 соответственно).

---

## MaterialNoiseRandomSeed

`real MaterialNoiseRandomSeed(string material, real seed);`

Сведения отсутствуют.

- `material` - имя материала
- `seed` - сведения отсутствуют.
