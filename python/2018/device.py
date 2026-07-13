Registers = list[int]
Instruction = tuple[str, int, int, int]


def addr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] + r[b]


def addi(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] + b


def mulr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] * r[b]


def muli(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] * b


def banr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] & r[b]


def bani(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] & b


def borr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] | r[b]


def bori(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = r[a] | b


def setr(r: Registers, a: int, _: int, c: int) -> None:
    r[c] = r[a]


def seti(r: Registers, a: int, _: int, c: int) -> None:
    r[c] = a


def gtir(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if a > r[b] else 0


def gtri(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if r[a] > b else 0


def gtrr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if r[a] > r[b] else 0


def eqir(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if a == r[b] else 0


def eqri(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if r[a] == b else 0


def eqrr(r: Registers, a: int, b: int, c: int) -> None:
    r[c] = 1 if r[a] == r[b] else 0


OPCODES = {
    "addr": addr,
    "addi": addi,
    "mulr": mulr,
    "muli": muli,
    "banr": banr,
    "bani": bani,
    "borr": borr,
    "bori": bori,
    "setr": setr,
    "seti": seti,
    "gtir": gtir,
    "gtri": gtri,
    "gtrr": gtrr,
    "eqir": eqir,
    "eqri": eqri,
    "eqrr": eqrr,
}
