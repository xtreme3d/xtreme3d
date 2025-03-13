# PickList

Объект PickList предназначен для хранения списков выборки. Он используется для сохранения результата функции ViewerGetPickedObjectsList, чтобы не пересоздавать в памяти массивы объектов в игровом цикле. При помощи функций PickList вы можете извлечь из списка объект по индексу. 

---

## PickListCreate

`real PickListCreate(real ps);`

Создает новый список выборки и возвращает ссылку на него.
- `ps` - режим сортировки списка. Доступны следующие значения `ps`:
    - `psDefault = 0` - режим по умолчанию
    - `psName = 1` - сортировка по именам объектов
    - `psMinDepth = 2` - сортировка по приближению к камере
    - `psMaxDepth = 3` - сортировка по отдалению от камеры.

---

## PickListClear

`real PickListClear(real picklist);`

Очищает список выборки.
- `picklist` - ссылка на список.

---

## PickListGetCount

`real PickListGetCount(real picklist);`

Возвращает количество объектов в списке выборки.
- `picklist` - ссылка на список.

---

## PickListGetHit

`real PickListGetHit(real picklist, real index);`

Возвращает объект из списка по заданному индексу.
- `picklist` - ссылка на список
- `index` - индекс (начиная с 0).
