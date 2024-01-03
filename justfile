set windows-shell := ["pwsh", "-NoProfile", "-Command"]

cflags := "-Wall -O3"
winlibs := "/DEFAULTLIB:libcmt.lib /DEFAULTLIB:libvcruntime.lib /DEFAULTLIB:libucrt.lib"
objfiles := "build/vm/vm.o build/vm/main.o"

build: builddir (d "pass/daul/variables") (d "pass/daul")

vm: (c "vm/main")
	lld-link /OUT:build/vm.exe /SUBSYSTEM:CONSOLE {{objfiles}} {{winlibs}}

c file:
	clang {{cflags}} -MD -o build/{{file}}.o -c {{file}}.c

d file:
	cd build; luajit ./main.lua ../{{file}}.daul {{file}}.lua

[windows]
builddir:
	#!pwsh -NoProfile
	Remove-Item build -Recurse -ErrorAction Ignore
	mkdir build | Out-Null
	Get-ChildItem -Exclude build -Directory . | % { Copy-Item -Recurse -Filter *.lua $_ build/ }
	Copy-Item main.lua build/
