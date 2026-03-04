from collections import deque
from pathlib import Path


class Unit:
    def __init__(self, kind: str, r: int, c: int, hp: int = 200, attack: int = 3):
        self.kind = kind
        self.r = r
        self.c = c
        self.hp = hp
        self.attack = attack

    def pos(self) -> tuple[int, int]:
        return (self.r, self.c)

    def is_alive(self) -> bool:
        return self.hp > 0


def parse_grid(
    data: list[str], elf_attack: int = 4
) -> tuple[set[tuple[int, int]], list[Unit]]:
    walls: set[tuple[int, int]] = set()
    units: list[Unit] = []
    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            if ch == "#":
                walls.add((r, c))
            elif ch in "EG":
                units.append(Unit(ch, r, c, attack=elf_attack if ch == "E" else 3))
    return walls, units


def adjacent(r: int, c: int) -> list[tuple[int, int]]:
    return [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]


def find_targets(unit: Unit, units: list[Unit]) -> list[Unit]:
    enemy_kind = "G" if unit.kind == "E" else "E"
    return [u for u in units if u.kind == enemy_kind and u.is_alive()]


def find_move(
    unit: Unit, targets: list[Unit], walls: set[tuple[int, int]], units: list[Unit]
) -> tuple[int, int] | None:
    occupied = walls | {u.pos() for u in units if u.is_alive() and u != unit}

    in_range: set[tuple[int, int]] = set()
    for target in targets:
        for pos in adjacent(*target.pos()):
            if pos not in occupied:
                in_range.add(pos)

    if not in_range:
        return None

    if unit.pos() in in_range:
        return None

    queue = deque([(unit.pos(), 0)])
    visited = {unit.pos(): (0, unit.pos(), unit.pos())}

    while queue:
        pos, dist = queue.popleft()
        for next_pos in adjacent(*pos):
            if next_pos in occupied or next_pos in visited:
                continue
            if dist == 0:
                first_step = next_pos
            else:
                first_step = visited[pos][2]
            visited[next_pos] = (dist + 1, next_pos, first_step)
            queue.append((next_pos, dist + 1))

    reachable = [(visited[p][0], p, visited[p][2]) for p in in_range if p in visited]
    if not reachable:
        return None

    reachable.sort(key=lambda x: (x[0], x[1], x[2]))
    return reachable[0][2]


def find_attack_target(unit: Unit, units: list[Unit]) -> Unit | None:
    adjacent_positions = set(adjacent(*unit.pos()))
    enemy_kind = "G" if unit.kind == "E" else "E"
    enemies = [
        u
        for u in units
        if u.kind == enemy_kind and u.is_alive() and u.pos() in adjacent_positions
    ]
    if not enemies:
        return None
    enemies.sort(key=lambda u: (u.hp, u.pos()))
    return enemies[0]


def simulate(walls: set[tuple[int, int]], units: list[Unit]) -> tuple[int, list[Unit]]:
    rounds = 0
    while True:
        units.sort(key=lambda u: u.pos())
        for unit in units:
            if not unit.is_alive():
                continue

            targets = find_targets(unit, units)
            if not targets:
                return rounds, units

            if not any(t.pos() in set(adjacent(*unit.pos())) for t in targets):
                move_to = find_move(unit, targets, walls, units)
                if move_to:
                    unit.r, unit.c = move_to

            target = find_attack_target(unit, units)
            if target:
                target.hp -= unit.attack

        units = [u for u in units if u.is_alive()]
        rounds += 1


def part_one(data: list[str]) -> int:
    walls, units = parse_grid(data)
    rounds, remaining = simulate(walls, units)
    total_hp = sum(u.hp for u in remaining if u.is_alive())
    return rounds * total_hp


def part_two(data: list[str]) -> int:
    elf_attack = 4
    walls, units = parse_grid(data, elf_attack=elf_attack)
    num_elfs = sum(1 for u in units if u.kind == "E")

    while True:
        rounds, remaining = simulate(walls, units)
        if sum(1 for u in remaining if u.kind == "E") == num_elfs:
            total_hp = sum(u.hp for u in remaining if u.is_alive())
            return rounds * total_hp
        elf_attack += 1
        walls, units = parse_grid(data, elf_attack=elf_attack)


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2018/input_15.txt"
    with open(path, "r") as f:
        data = [line.strip() for line in f.readlines()]
    print(f"Part one: {part_one(data)}")
    print(f"Part two: {part_two(data)}")
