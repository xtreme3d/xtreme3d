@echo off
if "%~1"=="" goto blank
if not exist %1 goto nofile
python setup.py build --script %1
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
