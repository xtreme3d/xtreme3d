@echo off
if "%~1"=="" goto blank
if not exist %1 goto nofile
if exist dist rmdir dist /s /q
C:/Python270/Scripts/pyinstaller.exe --noconfirm --noconsole --name app --specpath ./build --icon icon.ico %*
xcopy xtreme3d.dll dist\app /y
xcopy SDL2.dll dist\app /y
xcopy freetype.dll dist\app /y
xcopy OpenFBX.dll dist\app /y
mkdir dist\app\data
xcopy data\* dist\app\data /s /i
if exist build rmdir build /s /q
goto done
:blank
echo No argument given! Please, specify a script file to build.
pause
goto done
:nofile
echo Cannot find file "%1"!
pause
goto done
:done
