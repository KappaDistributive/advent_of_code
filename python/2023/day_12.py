from pathlib import Path
from functools import cache

@cache
def num_valid_solutions(record: str, groups: tuple[int, ...]) -> int:
    if not record:
        return len(groups) == 0

    if not groups:
        return "#" not in record

    char, rest_of_record = record[0], record[1:]

    if char == ".":
        return num_valid_solutions(rest_of_record, groups)

    if char == "#":
        group = groups[0]
        if (
            len(record) >= group
            and all(c != "." for c in record[:group])
            and (len(record) == group or record[group] != "#")
        ):
            return num_valid_solutions(record[group + 1 :], groups[1:])

        return 0

    if char == "?":
        return num_valid_solutions(f"#{rest_of_record}", groups) + num_valid_solutions(
            f".{rest_of_record}", groups
        )

    raise ValueError(f"unknown char: {char}")


def solve_line(line: str, with_multiplier=False) -> int:
    record, raw_shape = line.split()
    shape = tuple(map(int, raw_shape.split(",")))

    if with_multiplier:
        record = "?".join([record] * 5)
        shape *= 5

    return num_valid_solutions(record, shape)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2023/input_12.txt"
    print(f"Part one: {sum(solve_line(line) for line in open(path).read().strip().splitlines())}")
    print(f"Part one: {sum(solve_line(line, True) for line in open(path).read().strip().splitlines())}")

