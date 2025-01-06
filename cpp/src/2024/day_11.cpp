#include "../utils/input.hpp"

static std::map<std::pair<int64_t, int>, int64_t> cache;

int64_t expansion(int64_t stone, int steps) {
  if (auto it = cache.find({stone, steps}); it != cache.end()) {
    return it->second;
  }
  int64_t result{0};
  if (steps == 0) {
    result = 1;
  } else if (stone == 0) {
    result = expansion(1, steps - 1);
  } else {
    auto text = std::to_string(stone);
    if (text.size() % 2 == 0) {
      int64_t left = std::stoi(text.substr(0, text.size() / 2));
      int64_t right = std::stoi(text.substr(text.size() / 2));
      result = expansion(left, steps - 1) + expansion(right, steps - 1);
    } else {
      result = expansion(stone * 2024, steps - 1);
    }
  }
  cache[{stone, steps}] = result;
  return result;
}

auto part_one(std::list<int64_t> stones) {
  int64_t result{0};
  for (auto stone : stones) {
    result += expansion(stone, 25);
  }
  return result;
}

auto part_two(std::list<int64_t> stones) {
  int64_t result{0};
  for (auto stone : stones) {
    result += expansion(stone, 75);
  }
  return result;
}

std::list<int64_t> parse(const std::vector<std::string> &input) {
  assert(input.size() == 1);
  std::list<int64_t> numbers;
  for (auto &split : utils::split_string(input[0], ' ')) {
    numbers.push_back(std::stoll(split));
  }
  return numbers;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_11_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_11.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
