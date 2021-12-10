#include "../utils/input.hpp"

static const uint64_t UPPER_BOUND = 1000000;

int64_t part_one(const std::string& input) {
  uint64_t target{std::stoul(input)};
  uint64_t house;

  std::array<uint64_t, UPPER_BOUND> houses =
      std::array<uint64_t, UPPER_BOUND>();
  for (uint64_t elf{1}; elf < UPPER_BOUND; elf++) {
    house = elf;
    while (house <= UPPER_BOUND) {
      houses[house - 1] += 10 * elf;
      house += elf;
    }
  }

  for (size_t index{0}; index < houses.size(); index++) {
    if (houses[index] >= target) {
      return index + 1;
    }
  }
  return -1;
}

int64_t part_two(const std::string& input) {
  uint64_t target{std::stoul(input)};
  uint64_t house;
  uint64_t elf_exhaustion{1};

  std::array<uint64_t, UPPER_BOUND> houses =
      std::array<uint64_t, UPPER_BOUND>();
  for (uint64_t elf{1}; elf < UPPER_BOUND; elf++) {
    house = elf;
    elf_exhaustion = 1;
    while (house <= UPPER_BOUND && elf_exhaustion <= 50) {
      houses[house - 1] += 11 * elf;
      house += elf;
      elf_exhaustion++;
    }
  }

  for (size_t index{0}; index < houses.size(); index++) {
    if (houses[index] >= target) {
      return index + 1;
    }
  }
  return -1;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2015/input_20.txt"));
  const auto input = reader.get_lines()[0];

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

