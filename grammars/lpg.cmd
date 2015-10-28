@echo off

setlocal

if ""%1""=="""" goto es
set GRM=%1
goto run
:es
set GRM=es4.g

:run
lpg.exe -include-directory=. %GRM%

rem if errorlevel 1 goto error
rem copy /Y es4*.h ..\src\
rem copy /Y es4*.cpp ..\src\

:done

endlocal
