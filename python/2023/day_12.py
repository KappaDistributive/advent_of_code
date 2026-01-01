from itertools import product
from pathlib import Path
from typing import Iterator

def parse(data: list[str]) -> list[tuple[str, tuple[int]]]:
    parsed = []
    for line in data:
        parts = line.split(" ")
        assert len(parts) == 2
        command = parts[0]
        values = tuple(map(int, parts[1].split(',')))
        parsed.append((command, values))
    return parsed

def is_valid(springs: str, groups: tuple[int]) -> bool:
    return tuple(x for x in map(len, springs.split('.')) if x != 0) == groups

def expand_qmarks(s: str) -> Iterator[str]:
    q = s.count('?')
    for repl in product('.#', repeat=q):
        it = iter(repl)
        yield ''.join(next(it) if c == '?' else c for c in s)

def part_one(data: list[tuple[str, tuple[int]]]) -> int:
    result = 0
    for springs, groups in data:
        result += sum([is_valid(x, groups) for x in expand_qmarks(springs)])
    return result 

if __name__ == "__main__":
    with open(Path(__file__).parent.parent.parent / "data/2023/input_12.txt", "r") as file:
        data = parse(file.readlines())
        print(f"Part one: {part_one(data)}")
