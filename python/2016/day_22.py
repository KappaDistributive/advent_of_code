from collections import deque
from pathlib import Path


def parse(input_str: list[str]) -> list[tuple[int, int, int, int, int]]:
    nodes = []
    for line in input_str[2:]:
        parts = line.split()
        _, x_part, y_part = parts[0].split("-")
        x = int(x_part[1:])
        y = int(y_part[1:])
        size = int(parts[1][:-1])
        used = int(parts[2][:-1])
        avail = int(parts[3][:-1])
        nodes.append((x, y, size, used, avail))
    return nodes


def part_one(nodes: list[tuple[int, int, int, int, int]]) -> int:
    viable_pairs = 0
    for i in range(len(nodes)):
        for j in range(len(nodes)):
            if i != j and nodes[i][3] != 0 and nodes[i][3] <= nodes[j][4]:
                viable_pairs += 1
    return viable_pairs


def part_two(nodes: list[tuple[int, int, int, int, int]]) -> int:
    max_x = max(node[0] for node in nodes)
    max_y = max(node[1] for node in nodes)

    walls = set()
    empty_start = None

    for node in nodes:
        x, y, size, used, _ = node
        if used == 0:
            empty_start = (x, y)
        elif size > 100:
            walls.add((x, y))

    target_pos = (max_x - 1, 0)

    queue = deque([(empty_start, 0)])
    visited = {empty_start}

    steps_to_goal = 0
    while queue:
        pos, dist = queue.popleft()
        assert pos is not None

        if pos == target_pos:
            steps_to_goal = dist
            break

        x, y = pos
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx <= max_x and 0 <= ny <= max_y and (nx, ny) not in walls and (nx, ny) not in visited:
                visited.add((nx, ny))
                queue.append(((nx, ny), dist + 1))

    shuffle_steps = 1 + (max_x - 1) * 5

    return steps_to_goal + shuffle_steps


if __name__ == "__main__":
    input_path = Path(__file__).parent.parent.parent / "data/2016/input_22.txt"
    with open(input_path, "r") as f:
        lines = [line.strip() for line in f.readlines()]
        nodes = parse(lines)

    print("Part One:", part_one(nodes))
    print("Part Two:", part_two(nodes))
