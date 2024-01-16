set windows-shell := ["pwsh", "-NoProfile", "-Command"]

all: litbundle test

[windows]
luvibundle: build (d "luviloader")
	luvi dist -o build/daul.exe

[windows]
litbundle: build (d "luviloader")
	cp package.lua dist/
	lit make dist
	mv daul.exe build/

build: dirs (d "pass/daul/variables") (d "pass/daul")

test: selfcheck

selfcheck: (testd "pass/daul" "dist/pass/daul") (testd "pass/daul/variables" "dist/pass/daul/variables")

testd in out: (d "test")
	cd dist; luajit ./test.lua ../{{in}}.daul ../{{out}}.lua

d file:
	cd dist; luajit ./main.lua ../{{file}}.daul {{file}}.lua

[windows]
dirs:
	#!pwsh -NoProfile
	Remove-Item dist -Recurse -ErrorAction Ignore
	mkdir dist | Out-Null
	Get-ChildItem -Exclude dist -Directory . | % { Copy-Item -Recurse -Filter *.lua $_ dist/ }
	Copy-Item main.lua dist/
	Remove-Item build -Recurse -ErrorAction Ignore
	mkdir build | Out-Null
