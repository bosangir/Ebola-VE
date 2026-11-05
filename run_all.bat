@echo off
rem ===========================================================================
rem  run_all.bat  --  one-click EVE/VE analysis for Windows.
rem  Double-click this file (or run it from a terminal). It runs the whole
rem  pipeline on the linked files in the data\ folder and writes everything to
rem  outputs\. No data leaves this machine.
rem ===========================================================================
setlocal
cd /d "%~dp0"

rem -- locate Rscript (prefer R 4.6.0, then any R on PATH) --
set "RSCRIPT=C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
if not exist "%RSCRIPT%" (
  for /f "delims=" %%I in ('where Rscript 2^>nul') do set "RSCRIPT=%%I"
)
if not exist "%RSCRIPT%" if "%RSCRIPT%"=="C:\Program Files\R\R-4.6.0\bin\Rscript.exe" (
  echo [ERROR] Could not find Rscript.exe. Install R 4.6.x or edit RSCRIPT in this file.
  pause
  exit /b 1
)

echo ===========================================================================
echo  EVE / VE analysis
echo  R:        %RSCRIPT%
echo  Project:  %CD%
echo  Inputs:   data\  (linked_dataset_MAIN_distinct_cases.csv + merged_eve_lab_wide.csv)
echo  Outputs:  outputs\  (report, tables, figures, coded dataset)
echo ===========================================================================
echo.

"%RSCRIPT%" run_all.R
set "RC=%ERRORLEVEL%"

echo.
if "%RC%"=="0" (
  echo ===== DONE. Open the outputs\ folder for the report, tables and figures. =====
  start "" "%CD%\outputs"
) else (
  echo ===== FINISHED WITH ERRORS ^(exit %RC%^). Scroll up for the message. =====
)
echo.
pause
endlocal
