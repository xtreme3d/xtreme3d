function AudioInit(): real; cdecl;
begin
    result := Mix_OpenAudio(44100, $8010, 2, 2048); // Signed 16-bit samples
end;

function AudioClose(): real; cdecl;
begin
    Mix_CloseAudio();
    result := 1.0;
end;

function AudioChannelIsPlaying(channel: real): real; cdecl;
begin
    result := Mix_Playing(trunc(channel));
end;

function AudioMusicIsPlaying(): real; cdecl;
begin
    result := Mix_PlayingMusic();
end;

function AudioSetChannelVolume(channel, vol: real): real; cdecl;
begin
    Mix_Volume(trunc(channel), trunc(vol * 128));
    result := 1.0;
end;

function AudioSetMusicVolume(vol: real): real; cdecl;
begin
    Mix_VolumeMusic(trunc(vol * 128));
    result := 1.0;
end;

function AudioSetChannelPannning(channel, panning: real): real; cdecl;
begin
    Mix_SetPanning(trunc(channel), trunc((1.0 - panning) * 255), trunc(panning * 255));
    result := 1.0;
end;

function AudioSetChannelPosition(channel, angle, distance: real): real; cdecl;
begin
    Mix_SetPosition(trunc(channel), trunc(angle), trunc(distance));
    result := 1.0;
end;

function AudioSetChannelDistance(channel, distance: real): real; cdecl;
begin
    Mix_SetDistance(trunc(channel), trunc(distance));
    result := 1.0;
end;

function AudioStopChannel(channel, fade: real): real; cdecl;
begin
    if fade > 0 then
        Mix_FadeOutChannel(trunc(channel), trunc(fade * 1000))
    else
        Mix_HaltChannel(trunc(channel));
    result := 1.0;
end;

function AudioStopChannelDelayed(channel, delay: real): real; cdecl;
begin
    Mix_ExpireChannel(trunc(channel), trunc(delay * 1000));
    result := 1.0;
end;

function AudioStopMusic(fade: real): real; cdecl;
begin
    if fade > 0 then
        Mix_FadeOutMusic(trunc(fade * 1000))
    else
        Mix_HaltMusic();
    result := 1.0;
end;

function AudioPauseMusic(): real; cdecl;
begin
    Mix_PauseMusic();
    result := 1.0;
end;

function AudioResumeMusic(): real; cdecl;
begin
    Mix_ResumeMusic();
    result := 1.0;
end;

function AudioRewindMusic(): real; cdecl;
begin
    Mix_RewindMusic();
    result := 1.0;
end;

function AudioSetMusicPosition(pos: real): real; cdecl;
begin
    Mix_SetMusicPosition(pos);
    result := 1.0;
end;

function SoundLoad(filename: PAnsiChar): real; cdecl;
var
    chunk: PMix_Chunk;
begin
    chunk := Mix_LoadWAV(filename);
    result := PtrToReal(chunk);
end;

function SoundPlay(s, channel, loops: real): real; cdecl;
var
    chunk: PMix_Chunk;
begin
    chunk := RealToPtr(s);
    Mix_PlayChannel(trunc(channel), chunk, trunc(loops));
    result := 1.0;
end;

function MusicLoad(filename: PAnsiChar): real; cdecl;
var
    music: PMix_Music;
begin
    music := Mix_LoadMUS(filename);
    result := PtrToReal(music);
end;

function MusicPlay(m, loops: real): real; cdecl;
var
    music: PMix_Music;
begin
    music := RealToPtr(m);
    Mix_PlayMusic(music, trunc(loops));
    result := 1.0;
end;


