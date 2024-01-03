if grep -q '^[^%]*start\(\).*' "$1"; then
    echo trying to run: erl -noshell -pa "$3" -s "$2" start -s init stop
    erl erl -noshell -pa "$3" -s "$2" start -s init stop
else
    echo "Function start() not found in $1"
fi
