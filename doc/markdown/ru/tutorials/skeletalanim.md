# Урок 9. Основы скелетной анимации

Порой возможностей вертексной анимации оказывается недостаточно. Это касается, в основном, игр жанра action. Например, вы можете захотеть "дать" своему герою в руки оружие или "надеть" на него броню. При использовании вершинной анимации это невозможно (за редкими исключениями). Кроме того, вершинная анимация может потребовать слишком много памяти для хранения кадров. Поэтому, если вы используете модели с большим количеством полигонов, разумнее будет выбрать скелетную анимацию.

Вместо того чтобы хранить ключевые кадры (как в случае вертексной анимации) для каждой позы персонажа, использование скелетной анимации подразумевает наличие одной модели в нейтральной позе и большого набора матриц, которые трансформируют различные части этой модели. Эти матрицы условно называют костями. К каждой кости привязана группа вершин. Одна вершина может "принадлежать" нескольким костям сразу, с разной степенью влияния, что делает анимацию более естественной (это свойство костей называется развесовкой).

Впервые эта технология использовалась в игре Half-Life, и Xtreme3D поддерживает формат моделей Half-Life - SMD. В качестве альтернативы SMD, поддерживается также формат моделей Doom III - MD5.

Для загрузки моделей со скелетной анимацией используется тот же объект Actor. Вам не нужно ничего дополнительно указывать, Xtreme3D способен самостоятельно распознать тип модели и настроиться на соответствующий тип анимации: 

```gml
actor = ActorCreate("model.smd", matlib, matlib, global.scene);
```

Особенность формата SMD заключается в том, что анимация модели хранится в отдельном файле, который также имеет расширение *.smd. Таких файлов может быть несколько. Теоретически, такой метод позволяет использовать одни и те же файлы анимации для разных моделей (если они имеют одинаковый скелет).

После создания Актера следует добавить эти файлы: 

```gml
ActorAddObject(actor, "animation1.smd");
ActorAddObject(actor, "animation2.smd");
ActorAddObject(actor, "animation3.smd");
```

При добавлении очередного smd-файла, к анимации актера добавляется новая группа кадров, которой присваивается порядковый номер. Отсчет ведется с 1. То есть, если мы теперь переключимся на группу 2:

```gml
ActorSwitchToAnimation(actor, 2, false);
```

...то будет проиграна анимация, загруженная из файла animation2.smd.

К скелетной анимации применимы все функции, которые мы рассмотрели на предыдущем уроке.
