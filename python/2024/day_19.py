from functools import cache
from pathlib import Path


def solve(design: str, towels: tuple[str, ...]) -> int:
    @cache
    def count_ways(remaining: str) -> int:
        if not remaining:
            return 1
        total = 0
        for towel in towels:
            if remaining.startswith(towel):
                total += count_ways(remaining[len(towel) :])
        return total

    return count_ways(design)


def part_one(designs: list[str], towels: tuple[str, ...]) -> int:
    return sum(solve(design, towels) > 0 for design in designs)


def part_two(designs: list[str], towels: tuple[str, ...]) -> int:
    return sum(solve(design, towels) for design in designs)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_19.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    towels = tuple(t.strip() for t in data[0].split(","))
    designs = data[2:]
    print(f"Part one: {part_one(designs, towels)}")
    print(f"Part two: {part_two(designs, towels)}")
