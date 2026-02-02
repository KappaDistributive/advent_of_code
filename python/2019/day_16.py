from pathlib import Path


def fft_phase(digits: list[int]) -> list[int]:
    n = len(digits)
    result = []
    base_pattern = [0, 1, 0, -1]

    for pos in range(1, n + 1):
        total = 0
        for i, d in enumerate(digits):
            pattern_val = base_pattern[((i + 1) // pos) % 4]
            total += d * pattern_val
        result.append(abs(total) % 10)

    return result


def part_one(signal: str) -> str:
    digits = [int(c) for c in signal]
    for _ in range(100):
        digits = fft_phase(digits)

    return ''.join(map(str, digits[:8]))


def part_two(signal: str) -> str:
    """
    Apply 100 phases to signal repeated 10000 times.

    Key insight: The offset is in the second half of the signal.
    For positions in the second half, the pattern is all zeros before
    and all ones from that position onward. So each digit becomes
    the sum of itself and all following digits (mod 10).

    We can compute this efficiently by working backwards with cumulative sums.
    """
    offset = int(signal[:7])
    full_length = len(signal) * 10000

    # Verify offset is in second half (required for this optimization)
    assert offset >= full_length // 2

    # We only need digits from offset to end
    # Figure out which part of repeated signal we need
    needed_length = full_length - offset

    # Build the relevant portion of the signal
    # Repeat signal enough times and take what we need
    repeated = (signal * 10000)[offset:]
    digits = [int(c) for c in repeated[:needed_length]]

    # Apply 100 phases using the optimized approach
    for _ in range(100):
        # Work backwards: each digit = (digit + sum of all following) % 10
        # This is a cumulative sum from the right
        for i in range(len(digits) - 2, -1, -1):
            digits[i] = (digits[i] + digits[i + 1]) % 10

    return ''.join(map(str, digits[:8]))


if __name__ == "__main__":
    path = Path(__file__).parent.parent.parent / "data/2019/input_16.txt"
    with open(path, "r") as f:
        signal = f.read().strip()

    print(f"Part One: {part_one(signal)}")
    print(f"Part Two: {part_two(signal)}")
