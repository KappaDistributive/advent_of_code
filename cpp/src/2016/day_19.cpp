#include "../utils/input.hpp"

std::list<std::pair<size_t, uint64_t>>::iterator rotate(
    const std::list<std::pair<size_t, uint64_t>>::iterator& it,
    std::list<std::pair<size_t, uint64_t>>& list, const size_t& amount = 1) {
  std::list<std::pair<size_t, uint64_t>>::iterator result = it;
  for (size_t index{0}; index < amount; index++) {
    result = std::next(result);
    if (result == list.end()) {
      result = list.begin();
    }
  }
  return result;
}

uint64_t part_one(const std::string& input) {
  std::list<std::pair<size_t, uint64_t>> elves;
  for (size_t elf{1}; elf <= std::stoul(input); elf++) {
    elves.push_back({elf, 1});
  }
  auto elf = elves.begin();

  while (elves.size() > 1) {
    auto right_neighbor = rotate(elf, elves);
    elf->second += right_neighbor->second;
    elves.erase(right_neighbor);
    elf = rotate(elf, elves);
  }

  return elves.front().first;
}

uint64_t part_two(const std::string& input) {
  std::list<std::pair<size_t, uint64_t>> elves;
  for (size_t elf{1}; elf <= std::stoul(input); elf++) {
    elves.push_back({elf, 1});
  }
  auto elf = elves.begin();
  auto opponent = rotate(elf, elves, elves.size() / 2);

  while (elves.size() > 2) {
    elf->second += opponent->second;
    auto old_opponent = opponent;
    opponent = rotate(opponent, elves, 1 + (elves.size() % 2));
    elves.erase(old_opponent);
    elf = rotate(elf, elves);
  }

  return elf->first;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2016/input_19.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
