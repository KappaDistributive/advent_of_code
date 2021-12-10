#include <cassert>

#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::array<size_t, 9> fish{0};

  for (auto timer : input) {
    auto counter = std::stoi(timer);
    assert(0 <= counter && counter <= 8);
    ++fish[counter];
  }
  return fish;
}

auto step(const std::array<size_t, 9>& fish) {
  std::array<size_t, 9> new_fish{0};
  for (size_t counter{0}; counter < 9; ++counter) {
    if (counter == 0) {
      new_fish[8] += fish[0];
      new_fish[6] += fish[0];
    } else {
      new_fish[counter - 1] += fish[counter];
    }
  }

  return new_fish;
}

auto part_one(const std::vector<std::string>& input) {
  auto fish = prepare_input(input);
  for (size_t index{0}; index < 80; ++index) {
    fish = step(fish);
  }
  return std::accumulate(fish.begin(), fish.end(), 0ull);
}

auto part_two(const std::vector<std::string>& input) {
  auto fish = prepare_input(input);
  for (size_t index{0}; index < 256; ++index) {
    fish = step(fish);
  }
  return std::accumulate(fish.begin(), fish.end(), 0ull);
}

int main() {
  // std::filesystem::path input_path{"../../data/2021/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2021/input_06.txt"};
  utils::Reader reader(input_path);
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
