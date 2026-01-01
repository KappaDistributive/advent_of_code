from pathlib import Path

def step(position: tuple[int, int, str], data: list[str]) -> list[tuple[int, int, str]]:
    (x, y, direction) = position
    nx, ny = x, y
    result = []
    match direction:
        case ">":
            nx += 1
        case "<":
            nx -= 1
        case "^":
            ny -= 1
        case "v":
            ny += 1
    if not (0 <= nx < len(data[0]) and 0 <= ny < len(data)):
        return []
    match data[ny][nx]:
        case ".":
            result = [(nx, ny, direction)]
        case "/":
            match direction:
                case ">":
                    direction = "^"
                case "<":
                    direction = "v"
                case "^":
                    direction = ">"
                case "v":
                    direction = "<"
            result = [(nx, ny, direction)]
        case "\\":
            match direction:
                case ">":
                    direction = "v"
                case "<":
                    direction = "^"
                case "^":
                    direction = "<"
                case "v":
                    direction = ">"
            result = [(nx, ny, direction)]
        case "-":
            if (direction == ">" or direction == "<"):
                result =[(nx, ny, direction)]
            result = [(nx, ny, "<"), (nx, ny, ">")]
        case "|":
            if (direction == "^" or direction == "v"):
                result = [(nx, ny, direction)]
            result = [(nx, ny, "^"), (nx, ny, "v")]
    return result


def display(positions: list[tuple[int, int, str]], data: list[str]) -> None:
    for y in range(len(data)):
        row = ""
        for x in range(len(data[0])):
            found = False
            for pos in positions:
                if pos[0] == x and pos[1] == y:
                    row += pos[2]
                    found = True
                    break
            if not found:
                row += data[y][x]
        print(row)


def energy(data: list[str], active_positions: list[tuple[int, int, str]]) -> int:
    visited = set()
    n = 0
    while active_positions:
        new_positons = []
        for pos in active_positions:
            new_pos = step(pos, data)
            # print(f"{pos} -> {new_pos}")
            new_positons.extend([x for x in new_pos if x not in visited])
            visited.update(new_pos)
        active_positions = new_positons
        if n == len(visited):
            break
        n = len(visited)
    return len({pos[:2] for pos in visited})


def part_one(data: list[str]) -> int:
    return energy(data, [(-1, 0, ">")])


def part_two(data: list[str]) -> int:
    result = 0
    for x in range(len(data[0])):
        result = max(result, energy(data, [(x, -1, "v")]))
        result = max(result, energy(data, [(x, len(data), "^")]))
    for y in range(len(data)):
        result = max(result, energy(data, [(-1, y, ">")]))
        result = max(result, energy(data, [(len(data[0]), y, "<")]))
    return result


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2023/input_16.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    print(f"Part One: {part_one(data)}")
    print(f"Part Two: {part_two(data)}")
