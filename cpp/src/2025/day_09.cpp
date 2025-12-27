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

std::vector<std::pair<Point, Point>>
calc_edges(const std::vector<Point> &data) {
  std::vector<std::pair<Point, Point>> result;
  for (size_t i{0}; i < data.size(); ++i) {
    for (size_t j{i + 1}; j < data.size(); ++j) {
      Point p1{data[i]}, p2{data[j]};
      int64_t x1{std::min(p1[0], p2[0])}, x2{std::max(p1[0], p2[0])},
          y1{std::min(p1[1], p2[1])}, y2{std::max(p1[1], p2[1])};
      result.emplace_back(std::make_pair(Point{{x1, y1}}, Point{{x2, y2}}));
    }
  }
  Point p1{data.back()}, p2{data.front()};
  int64_t x1{std::min(p1[0], p2[0])}, x2{std::max(p1[0], p2[0])},
      y1{std::min(p1[1], p2[1])}, y2{std::max(p1[1], p2[1])};
  result.emplace_back(std::make_pair(Point{{x1, y1}}, Point{{x2, y2}}));
  return result;
}

bool edge_crosses_rectangle(const Point &edge_start, const Point &edge_end,
                            const Point &rect_min, const Point &rect_max) {
  // verify
  if (edge_start[0] == edge_end[0]) {
    return rect_min[0] < edge_start[0] && edge_start[0] < rect_max[0] &&
           edge_end[1] > rect_min[1] && edge_start[1] < rect_max[1];
  }
  assert(edge_start[1] == edge_end[1]);
  return rect_min[1] < edge_start[1] && edge_start[1] < rect_max[1] &&
         edge_end[0] > rect_min[0] && edge_start[0] < rect_max[0];
}

bool rectangle_is_valid(const Point a, const Point b,
                        const std::vector<std::pair<Point, Point>> &edges) {
  Point rect_min{{std::min(a[0], b[0]), std::min(a[1], b[1])}};
  Point rect_max{{std::max(a[0], b[0]), std::max(a[1], b[1])}};
  for (const auto &[edge_start, edge_end] : edges) {
    if (edge_crosses_rectangle(edge_start, edge_end, rect_min, rect_max)) {
      return false;
    }
  }
  return true;
}

auto part_one(const std::vector<Point> &data) {
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

auto part_two(const std::vector<Point> &data) {
  auto edges = calc_edges(data);
  Point k{{5600, 67661}}, l{{94601, 50063}};
  std::cout << rectangle_is_valid(k, l, edges) << std::endl;

  int64_t result{0};
  for (size_t i{0}; i < data.size(); ++i) {
    for (size_t j{i + 1}; j < data.size(); ++j) {
      auto diff = data[i] - data[j];
      auto coords = diff.coordinates();
      auto area = (std::abs(coords[0]) + 1) * (std::abs(coords[1]) + 1);
      if (area < result || !rectangle_is_valid(data[i], data[j], edges)) {
        continue;
      }
      std::cout << "Valid rectangle between " << data[i] << " and " << data[j]
                << " with area "
                << (std::abs(coords[0]) + 1) * (std::abs(coords[1]) + 1)
                << std::endl;
      result = area;
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_09_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_09.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
