from collections import defaultdict
from pathlib import Path


def parse(input_str: str) -> list[str]:
    return input_str.strip().split(",")


def hash_value(s: str) -> int:
    current = 0
    for char in s:
        current += ord(char)
        current *= 17
        current %= 256
    return current


def part_one(steps: list[str]) -> int:
    return sum(hash_value(step) for step in steps)


def part_two(steps: list[str]) -> int:
    boxes = defaultdict(dict)

    for step in steps:
        if "-" in step:
            label = step[:-1]
            box_id = hash_value(label)
            if label in boxes[box_id]:
                del boxes[box_id][label]
        elif "=" in step:
            label, focal_length = step.split("=")
            box_id = hash_value(label)
            boxes[box_id][label] = int(focal_length)

    total = 0
    for box_id, lenses in boxes.items():
        for slot, (label, focal_length) in enumerate(lenses.items(), 1):
            focusing_power = (box_id + 1) * slot * focal_length
            total += focusing_power

    return total


if __name__ == "__main__":
    input_path = Path(__file__).parent.parent.parent / "data/2023/input_15.txt"
    with open(input_path, "r") as f:
        input_str = f.read()
        steps = parse(input_str)

    print("Part One:", part_one(steps))
    print("Part Two:", part_two(steps))
