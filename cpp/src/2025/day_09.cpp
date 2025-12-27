#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int64_t, 2>;

std::vector<Point> parse_input(const std::vector<std::string> &data) {
  std::vector<Point> points;
  for (const auto &line : data) {
    auto splits = utils::split_string(line, ',');
    points.emplace_back(Point({std::stoi(splits[0]), std::stoi(splits[1])}));
  }
  return points;
}

auto part_one(std::vector<Point> &data) {
  int64_t result{0};
  for (size_t i{0}; i < data.size(); ++i) {
    for (size_t j{i + 1}; j < data.size(); ++j) {
      auto diff = data[i] - data[j];
      auto coords = diff.coordinates();
      result = std::max(result,
                        (std::abs(coords[0]) + 1) * (std::abs(coords[1]) + 1));
    }
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_09_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_09.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
