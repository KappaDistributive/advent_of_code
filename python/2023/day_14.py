from pathlib import Path

def roll_up(lines: list[list[str]]) -> list[list[str]]:
    changed = True
    while changed:
        changed = False
        for y in range(1, len(lines)):
            for x in range(len(lines[y])):
                if lines[y][x] == "O" and lines[y - 1][x] == ".":
                    changed = True
                    lines[y - 1][x] = "O"
                    lines[y][x] = "."
    return lines

def part_one(lines: list[list[str]]) -> int:
    lines = roll_up(lines)
    result = 0
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if lines[y][x] == "O":
                result += len(lines) - y
    return result

if __name__ == "__main__":
    input_path = Path(__file__).parent.parent.parent / "data/2023/input_14.txt"
    with open(input_path, "r") as f:
        lines = [list(line) for line in f.readlines()]

    print("Part One:", part_one(lines))
