# PAK

Xtreme3D поддерживает загрузку ресурсов (моделей, текстур) из PAK-архивов GLScene. Это бывает полезно для оптимизации места, занимаемого игрой на диске, а также для ускорения загрузки ресурсов. Кроме того, использование архива ресурсов усложняет воровство моделей и текстур (хотя и не делает невозможным, поскольку формат PAK не поддерживает шифрование, и его довольно легко прочитать). Для создания архива следует использовать утилиту PakEdit.exe, входящую в состав Xtreme3D SDK.

---
## SetPakArchive

`real SetPakArchive(string filename);`

Задает архив *.PAK, из которого движок должен загружать ресурсы, и возвращает указатель на него. Рекомендуется создавать архив и добавлять эту функцию только при финальной сборке проекта. После вызова этой функции все ресурсы без исключения будут загружаться движком из этого архива. Отменить это во время игры невозможно.
- `filename` - путь к файлу архива.

---
## PakGetFileCount

`real PakGetFileCount(real pak);`

Возвращает количество файлов в архиве.
- `pak` - указатель на объект архива.

---
## PakGetFileName

`real PakGetFileCount(real pak, real index);`

Возвращает имя файла под заданным индексом. Функция полезна, если вы не знаете заранее содержимое архива.
- `pak` - указатель на объект архива
- `index` - индекс файла (порядковый номер в архиве).

---
## PakExtract

`real PakExtract(real pak, string directory);`

Распаковывает содержимое архива в заданную папку, сохраняя его внутреннюю структуру.
- `pak` - указатель на объект архива
- `directory` - путь к папке для распаковки архива.

---
## PakExtractFile

`real PakExtractFile(real pak, real index, string newfilename);`

Распаковывает заданный файл из архива. Функция полезна, если вам нужно вручную прочитать какой-то файл, который напрямую не загружается в Xtreme3D - например, шейдер.
- `pak` - указатель на объект архива
- `index` - индекс файла (порядковый номер в архиве)
- `newfilename` - имя, под которым надо сохранить файл на диске.
