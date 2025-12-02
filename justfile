build:
    dune build
exec:
    dune exec advent-of-code
test:
    dune test

bw:
    dune build -w
ew:
    dune exec advent-of-code -w
tw:
    dune test -w

run day='0' test='':
    dune exec advent-of-code {{day}} {{test}} -w
