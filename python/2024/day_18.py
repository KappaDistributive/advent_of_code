import heapq
from pathlib import Path


def part_one(data: list[tuple[int, ...]], grid_size: int = 71, num_bytes: int = 1024) -> int:
    corrupted = set(data[:num_bytes])
    start = (0, 0)
    goal = (grid_size - 1, grid_size - 1)

    # Priority queue: (distance, x, y)
    pq = [(0, start[0], start[1])]
    visited = set()

    while pq:
        dist, x, y = heapq.heappop(pq)

        if (x, y) == goal:
            return dist

        if (x, y) in visited:
            continue
        visited.add((x, y))

        # Check all 4 neighbors
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < grid_size and 0 <= ny < grid_size:
                if (nx, ny) not in corrupted and (nx, ny) not in visited:
                    heapq.heappush(pq, (dist + 1, nx, ny))

    return -1  # No path found


def part_two(data: list[tuple[int, ...]]) -> tuple[int, ...]:
    index = 1024
    while (dist := part_one(data, grid_size=71, num_bytes=index)) != -1:
        index += 1
    return data[index - 1]


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2024/input_18.txt"
    with open(path, "r") as f:
        data = [tuple(map(int, lines.strip().split(","))) for lines in f.readlines()]
    print(f"Part one: {part_one(data)}")
    print(f"Part two: {part_two(data)}")
