from pathlib import Path


class Computer:

    def __init__(self, data: list[str]):
        self.a: int = 0
        self.b: int = 0
        self.c: int = 0
        self.ip: int = 0
        self.program: list[int] = []
        self.output: list[int] = []

        for line in data:
            if line.startswith("Register A:"):
                self.a = int(line.split(":")[1].strip())
            elif line.startswith("Register B:"):
                self.b = int(line.split(":")[1].strip())
            elif line.startswith("Register C:"):
                self.c = int(line.split(":")[1].strip())
            elif line.startswith("Program:"):
                self.program = list(map(int, line.split(":")[1].strip().split(",")))

    def combo(self, n: int) -> int:
        match n:
            case _ if n < 4:
                assert isinstance(n, int)
                return n
            case 4:
                return self.a
            case 5:
                return self.b
            case 6:
                return self.c
            case 7:
                raise ValueError(f"Invalid combo value: {n}")
        return 0

    def step(self) -> bool:
        if self.ip >= len(self.program):
            # print(",".join(map(str, self.output)))
            return True
        opcode = self.program[self.ip]
        match opcode:
            case 0:  # adv
                self.a = self.a // 2 ** self.combo(self.program[self.ip + 1])
                self.ip += 2
            case 1:  # xor
                self.b = self.b ^ self.program[self.ip + 1]
                self.ip += 2
            case 2:  # mod 8
                self.b = self.combo(self.program[self.ip + 1]) % 8
                self.ip += 2
            case 3:  # jnz
                if self.a == 0:
                    self.ip += 2
                else:
                    self.ip = self.program[self.ip + 1]
            case 4:  # bxc
                self.b = self.c ^ self.b
                self.ip += 2
            case 5:  # out
                self.output.append(self.combo(self.program[self.ip + 1]) % 8)
                self.ip += 2
            case 6:  # bdv
                self.b = self.a // 2 ** self.combo(self.program[self.ip + 1])
                self.ip += 2
            case 7:  # cdv
                self.c = self.a // 2 ** self.combo(self.program[self.ip + 1])
                self.ip += 2
        return False


def run(data: list[str], a: int) -> list[int]:
    computer = Computer(data)
    computer.a = a
    while not computer.step():
        pass
    return computer.output


def part_one(data: list[str]) -> str:
    computer = Computer(data)
    while not computer.step():
        pass
    return ",".join(map(str, computer.output))


def part_two(data: list[str]) -> int:
    """
    By decoding the program we see that for every iteration in-between outputs, a is divided by 8.
    So we can find a value of a that produces only the last entry of the wanted program. Then we multiply that value by 8 and add [0-7] that produces the two last entries of the wanted program. We repeat this process until we have the full wanted program.
    """
    a = 0
    want = Computer(data).program
    matching = 1
    while matching < len(want):
        while run(data, a) != want[-matching:]:
            a += 1
        matching += 1
        a = a * 8
    if run(data, a) != want:
        while run(data, a) != want:
            a += 1
    return a


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_17.txt"
    with open(path, "r") as f:
        data = [lines.strip() for lines in f.readlines()]
    print(f"Part one: {part_one(data)}")
    print(f"Part two: {part_two(data)}")
