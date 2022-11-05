@ECHO off

set CYGWIN_HOME=%HOMEPATH%\.bin\cygwin64
set wget="\Program Files (x86)\WinWget\wget\wget.exe"

cd %CYGWIN_HOME%
echo ======================================
echo Downloading latest cygwin installer...
echo ======================================
echo.
%wget% -N http://cygwin.com/setup-x86_64.exe
echo.
echo ======================================
echo Updating all cygwin packages...
echo ======================================
echo.
setup-x86_64.exe --no-desktop --no-shortcuts --no-startmenu --quiet-mode
echo.
echo ======================================
echo Update finished.
echo ======================================
echo.
pause
