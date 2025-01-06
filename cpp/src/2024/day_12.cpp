#include "../utils/input.hpp"


class Map {
 private:
  const std::vector<std::string>& m_map;
  
 public:
  Map (const std::vector<std::string>& map) : m_map(map) {}

  char cat(int x, int y) {
    return m_map[y][x];
  }
};

auto part_one(const std::vector<std::string>& input) {
  for (const auto& line : input) {
    std::cout << line << std::endl;
  }
  return 1;
}

auto part_two() {
  return 2;
}

int main() {
  std::filesystem::path input_path{"../../data/2024/input_12_mock.txt"};
  // std::filesystem::path input_path{"../../data/2024/input_12.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
