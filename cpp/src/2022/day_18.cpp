#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 3>;

Point parse(const std::string &description) {
  auto splits = utils::split_string(description, ',');
  assert(splits.size() == 3);
  return Point{
      {std::stoi(splits[0]), std::stoi(splits[1]), std::stoi(splits[2])}};
}

std::set<Point> parse(const std::vector<std::string> &input) {
  std::set<Point> result;
  for (const auto &line : input) {
    result.insert(parse(line));
  }
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
      if (!points.count(p + step)) {
        ++result;
      }
    }
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) {
  size_t result{0};
  const auto points = parse(input);
  const std::vector<Point> steps = {{Point{{1, 0, 0}}, Point{{-1, 0, 0}},
                                     Point{{0, 1, 0}}, Point{{0, -1, 0}},
                                     Point{{0, 0, 1}}, Point{{0, 0, -1}}}};
  std::set<Point> outside;
  outside.insert(Point{{-1, -1, -1}});
  assert(!points.count(*outside.begin()));
  std::set<Point> expanding = outside;
  Point temp{{0, 0, 0}};
  Point lower{*points.begin()}, upper{*points.begin()};
  for (const auto &point : points) {
    lower[0] = std::min(lower[0], point[0] - 2);
    lower[1] = std::min(lower[1], point[1] - 2);
    lower[2] = std::min(lower[2], point[2] - 2);
    upper[0] = std::max(upper[0], point[0] + 2);
    upper[1] = std::max(upper[1], point[1] + 2);
    upper[2] = std::max(upper[2], point[2] + 2);
  }
  while (expanding.size()) {
    std::set<Point> new_points;
    for (const auto &point : expanding) {
      for (const auto &step : steps) {
        temp = point + step;
        if (lower < temp && temp < upper && !points.count(temp) &&
            !outside.count(temp)) {
          new_points.insert(std::move(temp));
        }
      }
    }
    for (auto point : new_points) {
      outside.insert(std::move(point));
    }
    expanding = new_points;
  }

  for (const auto &p : points) {
    for (const auto &step : steps) {
      if (!points.count(p + step) && outside.count(p + step)) {
        ++result;
      }
    }
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_18_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_18.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
