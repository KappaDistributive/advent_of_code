from pathlib import Path


class Computer:

    def __init__(self, data: list[str]):
        self.a: int = 0
        self.b: int = 0
        self.c: int = 0
        self.ip: int = 0
        self.program: list[int] = []

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
            print("")
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
                print(self.combo(self.program[self.ip + 1]) % 8, end=",")
                self.ip += 2
            case 6:  # bdv
                self.b = self.a // 2 ** self.combo(self.program[self.ip + 1])
                self.ip += 2
            case 7:  # cdv
                self.c = self.a // 2 ** self.combo(self.program[self.ip + 1])
                self.ip += 2
        return False


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_17.txt"
    with open(path, "r") as f:
        data = [lines.strip() for lines in f.readlines()]
    computer = Computer(data)
    while not computer.step():
        pass
