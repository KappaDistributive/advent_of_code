from collections import deque
from pathlib import Path


def parse_grid(
    data: list[str],
) -> tuple[dict[tuple[int, int], str], tuple[int, int], tuple[int, int]]:
    grid: dict[tuple[int, int], str] = {}
    start: tuple[int, int] | None = None
    end: tuple[int, int] | None = None
    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            grid[(r, c)] = ch
            if ch == "S":
                start = (r, c)
            elif ch == "E":
                end = (r, c)
    assert (
        start is not None and end is not None
    ), "Start or end position not found in grid"
    return grid, start, end


def bfs_distances(
    grid: dict[tuple[int, int], str], start: tuple[int, int]
) -> dict[tuple[int, int], int]:
    distances = {start: 0}
    queue = deque([start])
    while queue:
        pos = queue.popleft()
        r, c = pos
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            new_pos = (nr, nc)
            if new_pos in grid and grid[new_pos] != "#" and new_pos not in distances:
                distances[new_pos] = distances[pos] + 1
                queue.append(new_pos)
    return distances


def solve(data: list[str], max_cheat: int, min_save: int = 100) -> int:
    grid, start, _ = parse_grid(data)
    distances = bfs_distances(grid, start)

    cheats = 0
    for (r, c), d1 in distances.items():
        for dr in range(-max_cheat, max_cheat + 1):
            for dc in range(-max_cheat, max_cheat + 1):
                cheat_dist = abs(dr) + abs(dc)
                if cheat_dist < 2 or cheat_dist > max_cheat:
                    continue
                target = (r + dr, c + dc)
                if target in distances:
                    d2 = distances[target]
                    saved = d2 - d1 - cheat_dist
                    if saved >= min_save:
                        cheats += 1
    return cheats


def part_one(data: list[str]) -> int:
    return solve(data, max_cheat=2)


def part_two(data: list[str]) -> int:
    return solve(data, max_cheat=20)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_20.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    print(f"Part one: {part_one(data)}")
    print(f"Part two: {part_two(data)}")
