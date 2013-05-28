@echo off

rmdir target\coverage /S /Q
mkdir target\coverage

C:\Elotech\CodeCoverage.exe -e Debug\Win32\UnitTest.exe -m Debug\Win32\UnitTest.map -od target\coverage -uf UnitTest.dpr -a /text -sp ..\src

call target\coverage\CodeCoverage_summary.html