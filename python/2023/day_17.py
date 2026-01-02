from pathlib import Path
import heapq

def part_one(data: list[list[int]]) -> int:
    rows, cols = len(data), len(data[0])

    # Directions: 0=right, 1=down, 2=left, 3=up
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    # Priority queue: (heat_loss, row, col, direction, consecutive_steps)
    # Start by going either right or down
    pq = [
        (0, 0, 0, 0, 0),  # Start going right
        (0, 0, 0, 1, 0),  # Start going down
    ]

    # Visited: (row, col, direction, consecutive_steps)
    visited = set()

    while pq:
        heat_loss, r, c, direction, consecutive = heapq.heappop(pq)

        # Check if we reached the destination
        if r == rows - 1 and c == cols - 1:
            return heat_loss

        # Skip if already visited this state
        state = (r, c, direction, consecutive)
        if state in visited:
            continue
        visited.add(state)

        # Try moving in the same direction (if we haven't moved 3 times already)
        if consecutive < 3:
            dr, dc = directions[direction]
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                new_heat = heat_loss + data[nr][nc]
                heapq.heappush(pq, (new_heat, nr, nc, direction, consecutive + 1))

        # Try turning left or right (reset consecutive count)
        for turn in [-1, 1]:
            new_direction = (direction + turn) % 4
            dr, dc = directions[new_direction]
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                new_heat = heat_loss + data[nr][nc]
                heapq.heappush(pq, (new_heat, nr, nc, new_direction, 1))

    return -1
        

if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2023/input_17.txt"
    with open(path, "r") as f:
        data = [list(map(int, lines.strip())) for lines in f.readlines()]
    print(f"Part One: {part_one(data)}")
