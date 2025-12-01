build:
    dune build
exec:
    dune exec advent-of-code

bw:
    dune build -w
ew:
    dune exec advent-of-code -w

run day='0' test='':
    dune exec advent-of-code {{day}} {{test}} -w
