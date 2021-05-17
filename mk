set -x
set -e

wat2wasm=/c/local/wabt/wat2wasm.exe
wasm2wat=/c/local/wabt/wasm2wat.exe
validate=/c/local/wabt/wasm-validate.exe

go install ./cmd/wg
go test

wg x_test.go > x.wat 
$wat2wasm x.wat -o x.wasm
