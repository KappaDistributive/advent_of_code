from dataclasses import dataclass
from heapq import heappop, heappush
from pathlib import Path


@dataclass(frozen=True)
class State:
    player_hp: int
    player_mana: int
    boss_hp: int
    shield_timer: int
    poison_timer: int
    recharge_timer: int
    mana_spent: int

    def __lt__(self, other: "State") -> bool:
        return self.mana_spent < other.mana_spent


@dataclass
class Spell:
    name: str
    cost: int
    damage: int = 0
    heal: int = 0
    armor: int = 0
    armor_turns: int = 0
    poison_turns: int = 0
    recharge_turns: int = 0


SPELLS = [
    Spell("Magic Missile", 53, damage=4),
    Spell("Drain", 73, damage=2, heal=2),
    Spell("Shield", 113, armor=7, armor_turns=6),
    Spell("Poison", 173, poison_turns=6),
    Spell("Recharge", 229, recharge_turns=5),
]


def parse(input_str: list[str]) -> tuple[int, int]:
    boss_hp = int(input_str[0].split(": ")[1])
    boss_damage = int(input_str[1].split(": ")[1])
    return boss_hp, boss_damage


def apply_effects(state: State) -> State:
    player_hp = state.player_hp
    player_mana = state.player_mana
    boss_hp = state.boss_hp
    shield_timer = state.shield_timer
    poison_timer = state.poison_timer
    recharge_timer = state.recharge_timer

    if poison_timer > 0:
        boss_hp -= 3
        poison_timer -= 1

    if recharge_timer > 0:
        player_mana += 101
        recharge_timer -= 1

    if shield_timer > 0:
        shield_timer -= 1

    return State(
        player_hp=player_hp,
        player_mana=player_mana,
        boss_hp=boss_hp,
        shield_timer=shield_timer,
        poison_timer=poison_timer,
        recharge_timer=recharge_timer,
        mana_spent=state.mana_spent,
    )


def simulate_player_turn(state: State, spell: Spell, hard_mode: bool) -> State | None:
    if hard_mode:
        state = State(
            player_hp=state.player_hp - 1,
            player_mana=state.player_mana,
            boss_hp=state.boss_hp,
            shield_timer=state.shield_timer,
            poison_timer=state.poison_timer,
            recharge_timer=state.recharge_timer,
            mana_spent=state.mana_spent,
        )
        if state.player_hp <= 0:
            return None

    state = apply_effects(state)

    if state.boss_hp <= 0:
        return state

    if state.player_mana < spell.cost:
        return None

    if spell.armor_turns > 0 and state.shield_timer > 0:
        return None
    if spell.poison_turns > 0 and state.poison_timer > 0:
        return None
    if spell.recharge_turns > 0 and state.recharge_timer > 0:
        return None

    new_player_hp = state.player_hp + spell.heal
    new_player_mana = state.player_mana - spell.cost
    new_boss_hp = state.boss_hp - spell.damage
    new_shield_timer = state.shield_timer
    new_poison_timer = state.poison_timer
    new_recharge_timer = state.recharge_timer

    if spell.armor_turns > 0:
        new_shield_timer = spell.armor_turns
    if spell.poison_turns > 0:
        new_poison_timer = spell.poison_turns
    if spell.recharge_turns > 0:
        new_recharge_timer = spell.recharge_turns

    return State(
        player_hp=new_player_hp,
        player_mana=new_player_mana,
        boss_hp=new_boss_hp,
        shield_timer=new_shield_timer,
        poison_timer=new_poison_timer,
        recharge_timer=new_recharge_timer,
        mana_spent=state.mana_spent + spell.cost,
    )


def simulate_boss_turn(state: State, boss_damage: int) -> State | None:
    state = apply_effects(state)

    if state.boss_hp <= 0:
        return state

    armor = 7 if state.shield_timer > 0 else 0
    damage = max(1, boss_damage - armor)
    new_player_hp = state.player_hp - damage

    if new_player_hp <= 0:
        return None

    return State(
        player_hp=new_player_hp,
        player_mana=state.player_mana,
        boss_hp=state.boss_hp,
        shield_timer=state.shield_timer,
        poison_timer=state.poison_timer,
        recharge_timer=state.recharge_timer,
        mana_spent=state.mana_spent,
    )


def find_min_mana(boss_hp: int, boss_damage: int, hard_mode: bool = False) -> int:
    initial_state = State(
        player_hp=50,
        player_mana=500,
        boss_hp=boss_hp,
        shield_timer=0,
        poison_timer=0,
        recharge_timer=0,
        mana_spent=0,
    )

    queue = [initial_state]
    visited = set()

    while queue:
        state = heappop(queue)

        state_key = (
            state.player_hp,
            state.player_mana,
            state.boss_hp,
            state.shield_timer,
            state.poison_timer,
            state.recharge_timer,
        )
        if state_key in visited:
            continue
        visited.add(state_key)

        for spell in SPELLS:
            after_player = simulate_player_turn(state, spell, hard_mode)
            if after_player is None:
                continue

            if after_player.boss_hp <= 0:
                return after_player.mana_spent

            after_boss = simulate_boss_turn(after_player, boss_damage)
            if after_boss is None:
                continue

            if after_boss.boss_hp <= 0:
                return after_boss.mana_spent

            heappush(queue, after_boss)

    return -1


def part_one(boss_hp: int, boss_damage: int) -> int:
    return find_min_mana(boss_hp, boss_damage, hard_mode=False)


def part_two(boss_hp: int, boss_damage: int) -> int:
    return find_min_mana(boss_hp, boss_damage, hard_mode=True)


if __name__ == "__main__":
    input_path = Path(__file__).parent.parent.parent / "data/2015/input_22.txt"
    with open(input_path, "r") as f:
        lines = [line.strip() for line in f.readlines()]
        boss_hp, boss_damage = parse(lines)

    print("Part One:", part_one(boss_hp, boss_damage))
    print("Part Two:", part_two(boss_hp, boss_damage))
