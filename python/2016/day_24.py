from pathlib import Path
import heapq
from itertools import permutations

def coords(data: list[str]) -> list[tuple[int, int]]:
    result = []
    for pos in range(10):
        for y in range(len(data)):
            for x in range(len(data[y])):
                if data[y][x] == f"{pos}":
                    result.append((x, y))
    return result

def dijkstra(grid: list[str], start: tuple[int, int], end: tuple[int, int]) -> float:
    rows, cols = len(grid), len(grid[0])
    priority_queue = [(0, start[0], start[1])]
    visited = set()

    while priority_queue:
        dist, x, y = heapq.heappop(priority_queue)
        if (x, y) in visited:
            continue
        if (x, y) == end:
            return dist
        visited.add((x, y))
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < cols and \
                    0 <= ny < rows and \
                    grid[ny][nx] != '#' and \
                    (nx, ny) not in visited:
                heapq.heappush(priority_queue, (dist + 1, nx, ny))
    return float('inf')  # No path found

def calculate_distances(grid: list[str], locations: list[tuple[int, int]]) -> dict[tuple[int, int], float]:
    distances: dict[tuple[int, int], float] = {}
    for i in range(len(locations)):
        for j in range(len(locations)):
            if i != j:
                dist = dijkstra(grid, locations[i], locations[j])
                distances[(i, j)] = dist
    return distances

def part_one(data: list[str]) -> float:
    locations = coords(data)
    distances = calculate_distances(data, locations)
    idx = list(range(1, len(locations)))
    min_distance = float('inf')

    for perm in permutations(idx):
        dist: float = 0
        current: int = 0
        for next_loc in perm:
            dist += distances[(current, next_loc)]
            current = next_loc
        min_distance = min(min_distance, dist)

    return min_distance

if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2016/input_24.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    print(part_one(data))
