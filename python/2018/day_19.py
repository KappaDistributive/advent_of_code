from pathlib import Path

Registers = list[int]


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

Instruction = tuple[str, int, int, int]


def parse(data: list[str]) -> tuple[int, list[Instruction]]:
    ip_reg = int(data[0].split()[1])
    program: list[Instruction] = []
    for line in data[1:]:
        if not line:
            continue
        parts = line.split()
        program.append((parts[0], int(parts[1]), int(parts[2]), int(parts[3])))
    return ip_reg, program


def run(
    ip_reg: int,
    program: list[Instruction],
    registers: Registers,
    stop_at: int | None = None,
) -> Registers:
    instruction_pointer = 0
    while 0 <= instruction_pointer < len(program):
        if instruction_pointer == stop_at:
            break
        registers[ip_reg] = instruction_pointer
        name, a, b, c = program[instruction_pointer]
        OPCODES[name](registers, a, b, c)
        instruction_pointer = registers[ip_reg] + 1
    return registers


def sum_of_divisors(n: int) -> int:
    total = 0
    i = 1
    while i * i <= n:
        if n % i == 0:
            total += i
            if i != n // i:
                total += n // i
        i += 1
    return total


def part_one(data: list[str]) -> int:
    ip_reg, program = parse(data)
    registers = run(ip_reg, program, [0] * 6)
    return registers[0]


def part_two(data: list[str]) -> int:
    ip_reg, program = parse(data)
    registers = run(ip_reg, program, [1, 0, 0, 0, 0, 0], stop_at=1)
    target = max(registers)
    return sum_of_divisors(target)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2018/input_19.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    print(f"Part one: {part_one(data)}")
    print(f"Part two: {part_two(data)}")
