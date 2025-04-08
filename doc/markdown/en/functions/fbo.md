# FBO

FBO (Frame Buffer Object) - это современный механизм внеэкранного рендеринга, пришедший на смену p-буферам (см. [MemoryViewer](memviewer.html)). Как и MemoryViewer, FBO позволяет рендерить сцену в отдельный буфер в видеопамяти, содержимое которого затем становится доступным в качестве текстуры. Это позволяет производить над полученным изображением различные шейдерные операции постобработки, применять фильтры и спецэффекты.

В отличие от MemoryViewer, FBO не требует относительно дорогостоящей операции переключения контекстов OpenGL, поэтому на современных системах предпочтительнее использовать именно его. Также FBO работает намного быстрее, чем `ViewerCopyToTexture`, которая, фактически, совершает копирование данных из одного буфера в другой. Еще одно преимущество - FBO предоставляет полноценный доступ к буферу глубины, что необходимо для реализации некоторых фильтров. При этом пользоваться FBO ничуть не сложнее, чем MemoryViewer. Единственное ограничение состоит в том, что доступ к текстурам FBO возможен только через шейдеры GLSL - то есть, нельзя создать материал, который бы использовал FBO-текстуру без шейдера.

Чтобы передать текстуры FBO в GLSL-шейдер, используйте функции `GLSLShaderSetParameterFBOColorTexture` и `GLSLShaderSetParameterFBODepthTexture` - соответственно, для цветового буфера и буфера глубины.

Функции FBO требуют поддержки OpenGL 1.4 и расширения `GL_ARB_framebuffer_object`.

---

## FBOCreate

`real FBOCreate(real width, real height, real parent);`

Создает новый FBO и возвращает указатель на него.

- `width`, `height` - ширина и высота буфера
- `parent` - указатель на родителя.

---

## FBOSetActive

`real FBOSetActive(real fbo, real mode);`

Переключает активность FBO.

- `fbo` - указатель на FBO
- `mode` - `true` или `false`.

---

## FBOSetAspect

`real FBOSetAspect(real fbo, real aspect);`

Задает соотношение сторон FBO (ширина, деленная на высоту).

- `fbo` - указатель на FBO
- `aspect` - соотношение сторон.

---

## FBOSetPickableTarget

`real FBOSetPickableTarget(real fbo, real mode);`

Сведения отсутствуют.

- `fbo` - указатель на FBO
- `mode` - `true` или `false`.

---

## FBOSetSize

`real FBOSetSize(real fbo, real width, real height);`

Задает размер FBO.

- `fbo` - указатель на FBO
- `width`, `height` - ширина и высота в пикселях.

---

## FBOSetCamera

`real FBOSetCamera(real fbo, real camera);`

Задает камеру, через которую FBO должен рендерить сцену.

- `fbo` - указатель на FBO
- `camera` - указатель на камеру.

---

## FBOSetRootObject

`real FBOSetRootObject(real fbo, real obj);`

Задает корневой объект для рендеринга. FBO будет рисовать этот объект и, рекурсивно, всех его потомков.

- `fbo` - указатель на FBO
- `obj` - указатель на объект.

---

## FBOSetBackgroundColor

`real FBOSetBackgroundColor(real fbo, real color);`

Задает фоновой цвет FBO (цвет очистки кадрового буфера).

- `fbo` - указатель на FBO
- `color` - цвет.

---

## FBOSetEnabledRenderBuffers

`real FBOSetEnabledRenderBuffers(real fbo, real depth, real stencil);`

Включает или выключает дополнительные буферы для FBO - буфер глубины и стенсильный буфер.

- `fbo` - указатель на FBO
- `depth` - `true` или `false`
- `stencil` - `true` или `false`.

---

## FBOSetSceneScaleFactor

`real FBOSetSceneScaleFactor(real fbo, real scale);`

Задает масштаб сцены при рендеринге в FBO.

- `fbo` - указатель на FBO
- `scale` - масштаб.

---

## FBOSetTargetVisibility

`real FBOSetTargetVisibility(real fbo, real tv);`

Задает режим видимости объектов, которые рендерятся в FBO.

- `fbo` - указатель на FBO
- `tv` - режим видимости. Поддерживаются следующие значения `tv`:
    - `tvDefault` = 0 - режим по умолчанию: видимость объектов не меняется;
    - `tvFBOOnly` = 1 - объекты рендерятся только в FBO и не рендерятся обычным образом.

---

## FBOSetMaterialLibrary

`real FBOSetMaterialLibrary(real fbo, real matlib);`

Задает библиотеку материалов, в которой хранятся текстуры для FBO.

- `fbo` - указатель на FBO
- `matlib` - указатель на библиотеку материалов.

---

## FBOSetColorTextureName

`real FBOSetColorTextureName(real fbo, string name);`

Задает имя материала, текстуру которого FBO должен использовать в качестве буфера кадра. Материал должен существовать в заданной библиотеке материалов, текстура должна быть создана функцией `MaterialGenTexture`.

- `fbo` - указатель на FBO
- `name` - имя материала.

---

## FBOSetDepthTextureName

`real FBOSetDepthTextureName(real fbo, string name);`

Задает имя материала, текстуру которого FBO должен использовать в качестве буфера глубины. Материал должен существовать в заданной библиотеке материалов, текстура должна быть создана функцией `MaterialGenTexture`.

- `fbo` - указатель на FBO
- `name` - имя материала.

---

## FBOSetClearOptions

`real FBOSetClearOptions(real fbo, real clearColor, real clearDepth, real clearStencil, real useBufferBackground);`

Задает опции очистки FBO.


- `clearColor` - очищать ли буфер кадра, `true` или `false`
- `clearDepth` - очищать ли буфер глубины, `true` или `false`
- `clearStencil` - очищать ли стенсильный буфер, `true` или `false`
- `useBufferBackground` - использовать ли для очистки фоновой цвет FBO, `true` или `false`.

---

## FBOSetStencilPrecision

`real FBOSetStencilPrecision(real fbo, real sp);`

Задает точность стенсильного буфера.

- `fbo` - указатель на FBO
- `sp` - точность стенсильного буфера. Поддерживаются следующие значения `sp`:
    - `spDefault` = 0
    - `sp1bit` = 1
    - `sp4bits` = 2
    - `sp8bits` = 3
    - `sp16bits` = 4

---

## FBOSetShadowMapMode

`real FBOSetShadowMapMode(real fbo, real mode);`

Переключает режим рендеринга теней. В режиме рендеринга теней используется смещение полигонов, отключается отбор видимости и игнорируются материалы объектов.

- `fbo` - указатель на FBO
- `mode` - `true` или `false`
