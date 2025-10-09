# Window

Функции для создания дополнительных окон и управления ими. Вы можете рендерить в созданном окне графику Xtreme3D, если передадите при создании вида соответствующий идентификатор (HWND) при помощи функции `WindowGetHandle`.

Имейте в виду, что использование дополнительных окон - это нестандартное поведение, которое не предусмотрено движком GameMaker (эта возможность существует, в основном, для языков, где нет встроенного способа создавать окна). Для дополнительных окон не будут работать события ввода GameMaker - нужно обрабатывать ввод встроенными функциями Xtreme3D (`MouseGetPosition`, `MouseGetPositionY`, `KeyIsPressed`). Перед завершением игры все созданные окна нужно удалить функцией `WindowDestroy`, иначе возникнет ошибка.

---

## WindowCreate

`real WindowCreate(real x, real y, real width, real height, real resizeable);`

Создает окно и возвращает указатель на него.

- `x`, `y` - позиция окна на экране
- `width`, `height` - ширина и высота окна
- `resizeable` - может ли пользователь изменять размер окна (`true` или `false`). Обратите внимание, что изменение размера окна не приводит к изменению размеров подключенного к нему вида, это нужно делать вручную.

---

## WindowSetBackgroundColor

`real WindowSetBackgroundColor(real window, real color);`

Задает фоновый цвет окна.

- `window` - указатель на окно;
- `color` - цвет.

---

## WindowCenter

`real WindowCenter(real window);`

Помещает окно в центр экрана.

- `window` - указатель на окно.

---

## WindowResize

`real WindowResize(real window, real x, real y, real width, real height);`

Задает позицию и размер окна. Обратите внимание, что изменение размера окна не приводит к изменению размеров подключенного к нему вида, это нужно делать вручную.

- `window` - указатель на окно
- `x`, `y` - позиция окна на экране
- `width`, `height` - ширина и высота окна.

---

## WindowGetPosition

`real WindowGetPosition(real window, real index);`

Возвращает координату позиции окна (левого верхнего угла).

- `window` - указатель на окно
- `index` - если этот параметр равен 0, функция возвращает координату X, если 1 - Y.

---

## WindowGetSize

`real WindowGetSize(real window, real index);`

Возвращает ширину или высоту окна.

- `window` - указатель на окно
- `index` - если этот параметр равен 0, функция возвращает ширину, если 1 - высоту.

---

## WindowGetHandle

`real WindowGetHandle(real window);`

Возвращает идентификатор окна (HWND).

- `window` - указатель на окно.

---

## WindowSetTitle

`real WindowSetTitle(real window, string title);`

Задает заголовок окна.

- `window` - указатель на окно
- `title` - заголовок.

---

## WindowSetIcon

`real WindowSetIcon(real window, string filename);`

Задает иконку окна.

- `window` - указатель на окно
- `filename` - имя файла ICO.

---

## WindowIsShowing

`real WindowIsShowing(real window);`

Возвращает 1, если окно отображается, и 0 в противном случае (если окно закрыто пользователем).

- `window` - указатель на окно

---

## WindowDispatch

`real WindowDispatch(real window);`

Обрабатывает события окна. Эта функция нужна, если вы пишете простое приложение (не Win32) - чтобы ваш игровой цикл не блокировал взаимодействие пользователя с окном. Вызывать ее рекомендуется в начале каждой итерации цикла. При использовании Xtreme3D в GameMaker вызывать эту функцию не нужно.

- `window` - указатель на окно.

---

## WindowDestroy

`real WindowDestroy(real window);`

Удаляет окно.

- `window` - указатель на окно.

---

## WindowIsActive

`real WindowIsActive(real window);`

Возвращает 1 (true), если окно существует, и 0 (false), если окно закрыто пользователем.

- `window` - указатель на окно.

---

## WindowControlCreate

`real WindowControlCreate(real windowHandle, real top, real left, real width, real height);`

Создает контроллер для заданного окна. Контроллер необходим для создания WinAPI-компонентов в окне - в частности, видеоплеера.

- `windowHandle` - дескриптор окна (HWND). Дескриптор окна игры в GML возвращается функцией `window_handle()`. В GameMaker: Studio 2 эта функция возвращает не `real`, а `ptr` (указатель). GML, к сожалению, не поддерживает передачу указателей в DLL-функции напрямую, поэтому нужно перевести это значение в `real`, прежде чем передавать в `WindowControlCreate`. Для этого в Xtreme3D предусмотрена функция `PointerToReal`:

```gml
viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), PointerToReal(windowHandle));
```

- `top`, `left` - координаты контроллера (левый верхний угол) относительно окна;
- `width`, `height` - ширина и высота контроллера.

---

## WindowControlSetBackgroundColor

`real WindowControlSetBackgroundColor(real wincontrol, real color);`

Задает фоновый цвет контроллера окна.

- `wincontrol` - указатель на контроллер;
- `color` - цвет.

---

## WindowControlFree

`real WindowControlFree(real wincontrol);`

Удаляет контроллер окна.

- `wincontrol` - указатель на контроллер;
