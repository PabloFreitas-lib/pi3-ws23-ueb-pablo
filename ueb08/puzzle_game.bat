REM Only for Windows!
echo Opening terminal...
REM Open terminal, set code page to 65001 for UTF-8 encoding and start stack ghci
start cmd /k "chcp 65001 && stack ghci"