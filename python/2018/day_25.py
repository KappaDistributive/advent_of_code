from pathlib import Path
from typing import Iterable


def manhattan_distance(a: tuple[int, ...], b: tuple[int, ...]) -> int:
    return sum(abs(x - y) for x, y in zip(a, b))


def distance(
    source: Iterable[tuple[int, ...]], target: Iterable[tuple[int, ...]]
) -> int:
    return min(manhattan_distance(a, b) for b in target for a in source)


def part_one(data: list[tuple[int, ...]]) -> int:
    constellations: list[set[tuple[int, ...]]] = [{entry} for entry in data]
    while True:
        print(len(constellations))
        for i, constellation in enumerate(constellations):
            for j, other in enumerate(constellations[i + 1 :], start=i + 1):
                if distance(constellation, other) <= 3:
                    constellation.update(other)
                    constellations.pop(j)
                    break
            else:
                continue
            break
        else:
            break
    return len(constellations)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2018/input_25.txt"
    with open(path, "r") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()]
    assert all(len(entry) == 4 for entry in data)
    print(f"Part one: {part_one(data)}")
