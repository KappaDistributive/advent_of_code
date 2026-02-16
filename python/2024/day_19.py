from functools import cache
from pathlib import Path


def solve(design: str, towels: tuple[str, ...]) -> bool:
    @cache
    def can_make(remaining: str) -> bool:
        if not remaining:
            return True
        for towel in towels:
            if remaining.startswith(towel):
                if can_make(remaining[len(towel) :]):
                    return True
        return False

    return can_make(design)


def part_one(designs: list[str], towels: tuple[str, ...]) -> int:
    return sum(solve(design, towels) for design in designs)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_19.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    towels = tuple(t.strip() for t in data[0].split(","))
    designs = data[2:]
    print(f"Part one: {part_one(designs, towels)}")
