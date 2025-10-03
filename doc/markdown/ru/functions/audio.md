# Audio

Функции Audio предоставляют возможность воспроизводить звуки с использованием [SDL_Mixer](https://github.com/libsdl-org/SDL_mixer) - простого, но достаточно мощного звукового движка. Он поддерживает 8 каналов для звуковых эффектов + 1 канал для фоновой музыки. Поддерживаются все потоковые аудиоформаты (MP3, OGG/Vorbis, FLAC, OPUS, WAVE, VOC), MIDI, трекерные форматы (MOD, XM, S3M, IT), а также секвенсорные форматы игровых консолей (AY, GBS, GYM, HES, KSS, NSF/NSFE, SAP, SPC, VGM/VGZ).

Поддержка некоторых форматов требует наличия дополнительных библиотек в папке с приложением (если вам не нужно воспроизводить эти форматы, то библиотеки необязательны):

- Для OPUS - `libopus-0.dll`, `libopusfile-0.dll`
- Для трекерных форматов - `libxmp.dll`
- Для консольных форматов - `libgme.dll`.

## AudioInit

`real AudioInit();`

Инициализирует звуковую систему. Эту функцию нужно вызвать один раз при старте приложения, после `EngineCreate`.

---

## AudioClose

`real AudioClose();`

Закрывает звуковую систему. Эту функцию нужно вызвать один раз при завершении работы приложения.

---

## AudioChannelIsPlaying

`real AudioChannelIsPlaying(real channel);`

Возвращает `true`, если в заданном канале воспроизводится звук, и `false` в противном случае.

- `channel` - номер канала от 0 до 7.

---

## AudioSetChannelVolume

`real AudioSetChannelVolume(real channel, real volume);`

Задает громкость канала.

- `channel` - номер канала от 0 до 7
- `volume` - громкость от 0.0 до 1.0.

---

## AudioSetChannelPannning

`real AudioSetChannelPannning(real channel, real panning);`

Задает стереобаланс канала.

- `channel` - номер канала от 0 до 7
- `panning` - значение от 0.0 до 1.0. При 0.0 звук будет идти из левой колонки, при 1.0 - из правой.

---

## AudioSetChannelPosition

`real AudioSetChannelPosition(real channel, real angle, real distance);`

Задает пространственную позицию канала относительно слушателя.

- `channel` - номер канала от 0 до 7
- `angle` - угол, в направлении которого доносится звук
- `distance` - расстояние до источника звука. Это значение должно быть от 0 до 255.

---

## AudioSetChannelDistance

`real AudioSetChannelDistance(real channel, real distance);`

Задает расстояние до источника звука.

- `channel` - номер канала от 0 до 7
- `distance` - расстояние до источника звука. Это значение должно быть от 0 до 255.

---

## AudioStopChannel

`real AudioStopChannel(real channel, real fade);`

Останавливает звук в канале.

- `channel` - номер канала от 0 до 7
- `fade` - если этот параметр больше нуля, то он задает длительность затухания звука (fade out) в секундах.

---

## AudioStopChannelDelayed

`real AudioStopChannelDelayed(real channel, real delay);`

Останавливает звук в канале с задержкой.

- `channel` - номер канала от 0 до 7
- `delay` - время задержки в секундах.

---

## AudioMusicIsPlaying

`real AudioMusicIsPlaying();`

Возвращает `true`, если музыка воспроизводится, и `false` в противном случае.

---

## AudioSetMusicVolume

`real AudioSetMusicVolume(real volume);`

Задает громкость музыки.

- `volume` - громкость от 0.0 до 1.0.

---

## AudioSetMusicPosition

`real AudioSetMusicVolume(real position);`

Задает позицию музыкальной дорожки.

- `position` - 

---

## AudioStopMusic

`real AudioStopMusic(real fade);`

Останавливает музыку.

- `fade` - если этот параметр больше нуля, то он задает длительность затухания музыки (fade out) в секундах.

---

## AudioPauseMusic

`real AudioPauseMusic();`

Ставит музыку на паузу.

---

## AudioResumeMusic

`real AudioResumeMusic();`

Продолжает воспроизведение музыки после паузы.

---

## AudioRewindMusic

`real AudioRewindMusic();`

Сбрасывает музыку на начало трека.

---

## SoundLoad

`real SoundLoad(string filename);`

Загружает звук и возвращает указатель на него.

- `filename` - путь к аудиофайлу.

---

## SoundPlay

`real SoundPlay(real sound, real channel, real loops);`

Воспроизводит звук.

- `sound` - указатель на звук
- `channel` - номер канала от 0 до 7 (либо -1, если канал не важен)
- `loops` - количество повторов (0 для однократного воспроизведения, -1 для бесконечного повторения).

---

## MusicLoad

`real MusicLoad(string filename);`

Загружает музыку и возаращает указатель на нее.

- `filename` - путь к фаудиофайлу.

---

## MusicPlay

`real MusicPlay(real music, real loops);`

Воспроизводит музыку.

- `music` - указатель на музыку
- `loops` - количество повторов (0 для однократного воспроизведения, -1 для бесконечного повторения).
