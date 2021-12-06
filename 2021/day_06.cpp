#include "../utils/input.hpp"

struct Lanternfish {
  int counter;
};

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<Lanternfish> fish;
  for (auto timer : input) {
    fish.push_back(Lanternfish{std::stoi(timer)});
  }
  return fish;
}

auto step(const std::vector<Lanternfish>& fish) {
  std::vector<Lanternfish> new_fish;
  std::vector<Lanternfish> children;

  for (auto f : fish) {
    if (f.counter == 0) {
      new_fish.push_back(Lanternfish{6});
      children.push_back(
          Lanternfish{8});
    } else {
      new_fish.push_back(Lanternfish{f.counter - 1});
    }
  }

  for (auto child : children) {
    new_fish.push_back(child);
  }

  return new_fish;
}

auto part_one(const std::vector<std::string>& input) {
  auto fish = prepare_input(input);
  for (size_t index{0}; index < 80; ++index) {
    fish = step(fish);
    // for (auto f : fish) {
    //   std::cout << f.counter << ", ";
    // }
    // std::cout << std::endl;
  }
  return fish.size();
}

// auto part_two(const std::vector<std::string>& input) {
// }

int main() {
  // std::filesystem::path input_path{"../2021/data/input_06_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_06.txt"};
  utils::Reader reader(input_path);
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
