from copy import deepcopy
from functools import cache
from pathlib import Path


def parse(input_str: list[str]) -> frozenset[tuple[int, int, str]]:
    grid = set()
    for y in range(len(input_str)):
        for x in range(len(input_str[y])):
            if input_str[y][x] == ".":
                continue
            grid.add((x, y, input_str[y][x]))
    return frozenset(grid)


@cache
def roll_up(grid: frozenset[tuple[int, int, str]], _: int, __: int) -> frozenset[tuple[int, int, str]]:
    new_grid = set(grid)
    sorted_keys = sorted(new_grid, key=lambda k: 100000 * k[1] + k[0])
    for entry in sorted_keys:
        (x, y, val) = entry
        if val == "O":
            ny = y
            while ny > 0 and (x, ny - 1, "O") not in new_grid and (x, ny - 1, "#") not in new_grid:
                ny -= 1
            if ny != y:
                new_grid.remove((x, y, "O"))
                new_grid.add((x, ny, "O"))
    return frozenset(new_grid)

@cache
def roll_down(grid: frozenset[tuple[int, int, str]], _: int, height: int) -> frozenset[tuple[int, int, str]]:
    new_grid = set(grid)
    sorted_keys = sorted(new_grid, key=lambda k: -100000 * k[1] + k[0])
    for entry in sorted_keys:
        (x, y, val) = entry
        if val == "O":
            ny = y
            while ny + 1 < height and (x, ny + 1, "O") not in new_grid and (x, ny + 1, "#") not in new_grid:
                ny += 1
            if ny != y:
                new_grid.remove((x, y, "O"))
                new_grid.add((x, ny, "O"))
    return frozenset(new_grid)


@cache
def roll_right(grid: frozenset[tuple[int, int, str]], width: int, _: int) -> frozenset[tuple[int, int, str]]:
    new_grid = set(grid)
    sorted_keys = sorted(new_grid, key=lambda k: -100000 * k[0] + k[1])
    for entry in sorted_keys:
        (x, y, val) = entry
        if val == "O":
            nx = x
            while nx + 1 < width and (nx + 1, y, "O") not in new_grid and (nx + 1, y, "#") not in new_grid:
                nx += 1
            if nx != x:
                new_grid.remove((x, y, "O"))
                new_grid.add((nx, y, "O"))
    return frozenset(new_grid)


@cache
def roll_left(grid: frozenset[tuple[int, int, str]], _: int, __: int) -> frozenset[tuple[int, int, str]]:
    new_grid = set(grid)
    sorted_keys = sorted(new_grid, key=lambda k: 100000 * k[0] + k[1])
    for entry in sorted_keys:
        (x, y, val) = entry
        if val == "O":
            nx = x
            while nx > 0 and (nx - 1, y, "O") not in new_grid and (nx - 1, y, "#") not in new_grid:
                nx -= 1
            if nx != x:
                new_grid.remove((x, y, "O"))
                new_grid.add((nx, y, "O"))
    return frozenset(new_grid)


def load(grid: frozenset[tuple[int, int, str]], height: int) -> int:
    result = 0
    for entry in grid:
        (_, y, val) = entry
        if val == "O":
            result += height - y
    return result


def part_one(grid: frozenset[tuple[int, int, str]], width: int, height: int) -> int:
    grid = roll_up(grid, width, height)
    return load(grid, height)


def part_two(grid: frozenset[tuple[int, int, str]], width: int, height: int) -> int:
    CYCLES = 1000000000
    seen = []
    seen.append(grid)
    cycle = 0
    i = 0
    for i in range(CYCLES):
        grid = roll_up(grid, width, height)
        grid = roll_left(grid, width, height)
        grid = roll_down(grid, width, height)
        grid = roll_right(grid, width, height)
        if grid in seen:
            cycle = seen.index(grid)
            break
        seen.append(grid)
    final_grid = seen[(CYCLES - cycle) % (i + 1 - cycle) + cycle]
    return load(final_grid, height)


if __name__ == "__main__":
    input_path = Path(__file__).parent.parent.parent / "data/2023/input_14.txt"
    with open(input_path, "r") as f:
        lines = [line.strip() for line in f.readlines()]
        height = len(lines)
        width = len(lines[0])
        grid = parse(lines)

    print("Part One:", part_one(grid, width, height))
    print("Part Two:", part_two(grid, width, height))
