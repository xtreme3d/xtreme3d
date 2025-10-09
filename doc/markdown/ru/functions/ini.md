# INI

Функции для записи и чтения файлов конфигурации *.ini. Их удобно использовать для хранения настроек игры.

---

## IniCreate

`real IniCreate(string filename);`

Создает ini-документ в памяти и возвращает указатель на него. Документ не записывается в файл, пока не будет вызвана `IniUpdateFile`.

- `filename` - путь к существующему или новому файлу *.ini.

---

## IniClose

`real IniClose(real ini);`

Удаляет ini-документ.

- `ini` - указатель на ini-документ.

---

## IniWriteString

`real IniWriteString(real ini, string section, string key, string value);`

Добавляет строковую запись в ini-документ.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `value` - значение.

---

## IniWriteNumber

`real IniWriteNumber(real ini, string section, string key, real value);`

Добавляет числовую запись в ini-документ.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `value` - значение.

---

## IniWriteBool

`real IniWriteBool(real ini, string section, string key, real value);`

Добавляет логическую запись в ini-документ.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `value` - значение записи.

---

## IniReadString

`string IniReadString(real ini, string section, string key, string defaultvalue);`

Читает строковую запись из ini-документа.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `defaultvalue` - значение, которое нужно вернуть по умолчанию, если запись не существует.

---

## IniReadNumber

`real IniReadNumber(real ini, string section, string key, real defaultvalue);`

Читает числовую запись из ini-документа.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `defaultvalue` - значение, которое нужно вернуть по умолчанию, если запись не существует.

---

## IniReadBool

`real IniReadBool(real ini, string section, string key, real defaultvalue);`

Читает логическую запись из ini-документа.

- `ini` - указатель на ini-документ;
- `section` - имя секции;
- `key` - ключ;
- `defaultvalue` - значение, которое нужно вернуть по умолчанию, если запись не существует.

---

## IniUpdateFile

`real IniUpdateFile(real ini);`

Записывает ini-документ в файл. Нужно вызвать эту функцию после всех сделанных изменений.

- `ini` - указатель на ini-документ.
