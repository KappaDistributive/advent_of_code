#include "../utils/input.hpp"

auto part_one(const std::vector<std::string> &data) {
  int result{0}, position{50};
  for (const auto &line : data) {
    int rotation = line[0] == 'R' ? 1 : -1;
    position += rotation * std::stoi(line.substr(1));
    position += 100000;
    position = position % 100;
    if (position == 0)
      ++result;
  }
  return result;
}

auto part_two(const std::vector<std::string> &data) {
  int result{0}, position{50};
  for (const auto &line : data) {
    int rotation = line[0] == 'R' ? 1 : -1;
    for (int click{0}; click < std::stoi(line.substr(1)); ++click) {
      position += rotation;
      position += 100000;
      position = position % 100;
      if (position == 0)
        ++result;
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_01.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
