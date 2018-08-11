@call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x64
@call cl %1 %~dp0/hello.lib %~dp0/test.lib 