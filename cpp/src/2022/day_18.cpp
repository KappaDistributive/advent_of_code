#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 3>;

Point parse(const std::string &description) {
  auto splits = utils::split_string(description, ',');
  assert(splits.size() == 3);
  return Point{
      {std::stoi(splits[0]), std::stoi(splits[1]), std::stoi(splits[2])}};
}

std::vector<Point> parse(const std::vector<std::string> &input) {
  std::vector<Point> result;
  std::transform(input.cbegin(), input.cend(), std::back_inserter(result),
                 [](const std::string &line) { return parse(line); });
  return result;
}

auto part_one(const std::vector<std::string> &input) {
  size_t result{0};
  auto points = parse(input);
  std::vector<Point> steps = {{Point{{1, 0, 0}}, Point{{-1, 0, 0}},
                               Point{{0, 1, 0}}, Point{{0, -1, 0}},
                               Point{{0, 0, 1}}, Point{{0, 0, -1}}}};

  for (const auto &p : points) {
    for (const auto &step : steps) {
      if (std::find(points.cbegin(), points.cend(), p + step) ==
          points.cend()) {
        ++result;
      }
    }
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_18_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_18.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
