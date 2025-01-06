#include "../utils/input.hpp"

void step(std::list<int64_t> &stones) {
  for (auto it{stones.begin()}; it != stones.end(); ++it) {
    if (*it == 0) {
      *it = 1;
    } else if (std::to_string(*it).size() % 2 == 0) {
      auto text = std::to_string(*it);
      int64_t left = std::stoi(text.substr(0, text.size() / 2));
      int64_t right = std::stoi(text.substr(text.size() / 2));
      stones.emplace(it, left);
      *it = right;
    } else {
      *it *= 2024;
    }
  }
}

void step(std::list<int64_t> &stones, int64_t steps) {
  while (steps-- > 0) {
    step(stones);
  }
}

auto part_one(std::list<int64_t> stones) {

  for (int s{0}; s < 25; ++s) {
    // std::cout << "Step " << s << ": ";
    // for (auto stone : stones) {
    //     std::cout << stone << ' ';
    //   }
    //   std::cout << std::endl;
    step(stones);
  }
  return stones.size();
}

auto part_two() { return 2; }

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
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
