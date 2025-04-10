# Урок 8. Вершинная анимация

Объект Freeform предназначен, в основном, для неодушевленных предметов. Обычно это элементы декорации, транспортные средства, различные интерактивные объекты и т.д. Если мы хотим заселить наш виртуальный мир живыми существами, нам не обойтись без объектов Actor. Название говорит само за себя: актер - это живой персонаж. В Xtreme3D Актеры представляют собой анимированные модели. А анимация, как известно, бывает двух типов - вертексная и скелетная. На этом уроке мы рассмотрим вертексную анимацию.

Вертексная (или вершинная) анимация характерна тем, что для формирования анимационной последовательности движок перемещает каждую вершину модели от одной позиции к другой. Этот тип анимации впервые был применен в Quake, и с тех пор форматы моделей серии Quake (MD2, MD3) стали своего рода стандартом во всех популяных движках. Xtreme3D предоставляет полную поддержку MD2 и MD3. Разница между ними заключается в том, что MD2 хранит всю модель целиком в одном файле, а MD3 - в трех (отдельно голова, туловище и ноги). При помощи специальных матриц туловище синхронизируется с ногами, а голова - с туловищем. Это было сделано для того, чтобы анимировать туловище и ноги по отдельности. Например, во время стрельбы персонаж может как бежать, так и идти медленно, а то и просто стоять на месте.

На этом уроке мы рассмотрим вертексную анимацию с форматом MD2. Актер из модели MD2 создается так:

```gml
actor = ActorCreate("model.md2", matlib, matlib, global.scene);
```

Иногда после загрузки модели оказывается, что она неправильно повернута. Это происходит потому, что в разных редакторах направление осей трактуется по-разному. Обычно "меняются местами" оси Y и Z. Вершины модели записаны так, что ее вектор Up направлен вдоль оси Z (в DirectX-приложениях это означает "вверх"), и, поскольку за направление "вверх" в Xtreme3D отвечает ось Y, а не Z, получается, что модель повернута на -90 градусов по оси X. Мы можем исправить это недоразумение несколькими способами. Самый простой - просто повернуть ее обратно:

```gml
ObjectPitch(actor, 90);
```

Но в некоторых случаях этого недостаточно. Поворачивая модель, мы также поворачиваем ее локальную систему координат. Это значит, что ее вектор Direction теперь указывает вдоль оси Y, а не Z, как должно быть. Если мы теперь переместим модель при помощи `ObjectMove`, она сдвинется вверх, а не вперед. Можно, конечно, вместо `ObjectMove` использовать `ObjectStrafe`, но это сделает программу менее аккуратной, да и запутаться так недолго. Гораздо лучше сначала поместить актера в потомки даммикубу, а уже потом поворачивать. И, соответственно, для перемещения использовать даммикуб, а не актера. Код будет следующий:

```gml
player = DummycubeCreate(global.scene);
actor = ActorCreate("model.md2", matlib, matlib, player);
ObjectPitch(actor, 90);
```

Формат MD2 предусматривает разделение всех кадров анимации на отдельные группы. Это сделано для того, чтобы отделить, скажем, анимацию бега от анимации прыжка. По умолчанию Xtreme3D воспроизводит все кадры один за другим, не обращая внимания на это разделение. Но мы можем в любое время переключиться на желаемую анимацию:

```gml
ActorSwitchToAnimation(actor, 1, false);
```

И тогда будет воспроизводиться только группа кадров под номером 1. Третий параметр этой функции отвечает за плавность смены анимации: если установить его в `true`, то переключение будет постепенным.

Примерно то же самое делает функция, указывающая диапазон кадров для воспроизведения:

```gml
ActorSetAnimationRange(actor, 10, 20);
```

Нетрудно догадаться, что будет проигран только промежуток между десятым и двадцатым кадрами. Правда, эти две функции имеют одно важное различие. `ActorSwitchToAnimation` каждый раз при вызове переключает воспроизведение на первый кадр заданной группы, а `ActorSetAnimationRange` этого не делает (если заданный диапазон уже воспроизводится). Поэтому `ActorSetAnimationRange` можно вызывать многократно - например, внутри цикла, что в некоторых ситуациях оказывается весьма полезным.

По умолчанию анимация воспроизводится циклически - то есть, при достижении последнего кадра, воспроизведение начинается заново. Для большинства случаев это то, что нужно (например, анимация ходьбы или бега всегда зациклена). Но мы можем указать и другие режимы воспроизведения:

```gml
ActorSetAnimationMode(actor, aam);
```

Вместо `aam` подставляется одна из следующих констант:

`aamNone` - анимация не воспроизводится;<br>
`aamPlayOnce` - анимация воспроизводится один раз и останавливается при достижении конечного кадра. Этот режим иногда называют "one shot";<br>
`aamLoop` - анимация повторяется циклически (по умолчанию);<br>
`aamBounceForward` - анимация повторяется циклически вперед до конечного кадра, затем в обратную сторону до начального кадра, затем опять вперед и так далее. Этот режим иногда называют "пинг-понг".<br>
`aamBounceBackward` - то же самое, но в обратную сторону;<br>
`aamLoopBackward` - анимация повторяется циклически в обратную сторону.<br>

Наконец, существует также возможность отключить линейную интерполяцию между кадрами:

```gml
ActorSetFrameInterpolation(actor, false);
```

При этом кадры будут сменять друг друга резко, без плавного "перетекания". Это может быть полезным, например, в гонках, где кузов автомобиля может быть деформирован - в разных кадрах вершинной анимации можно хранить разные варианты повреждений.
