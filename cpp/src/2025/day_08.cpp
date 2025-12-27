#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int64_t, 3>;

std::vector<Point> parse_input(const std::vector<std::string> &input) {
  std::vector<Point> points;
  for (const auto &line : input) {
    auto splits = utils::split_string(line, ',');
    assert(splits.size() == 3);
    points.emplace_back(Point{
        {std::stoi(splits[0]), std::stoi(splits[1]), std::stoi(splits[2])}});
  }
  return points;
}

auto solve(const std::vector<Point> &data, int limit = -1) {
  std::map<std::pair<Point, Point>, double> distances;
  for (size_t i = 0; i < data.size(); ++i) {
    for (size_t j = i + 1; j < data.size(); ++j) {
      distances[{data[i], data[j]}] =
          data[i].euclidean_distance_squared(data[j]);
    }
  }
  std::vector<std::pair<std::pair<Point, Point>, double>> distance_vec(
      distances.begin(), distances.end());
  std::sort(distance_vec.begin(), distance_vec.end(),
            [](const auto &a, const auto &b) {
              if (a.second == b.second) {
                return a.first < b.first;
              }
              return a.second < b.second;
            });
  std::vector<std::vector<Point>> clusters;

  // every point starts as its own cluster
  for (auto point : data) {
    clusters.push_back({point});
  }
  int steps{0};
  for (const auto &[point_pair, _] : distance_vec) {
    ++steps;
    auto [left, right] = point_pair;
    int left_idx = -1;
    int right_idx = -1;
    for (int idx{0}; idx < static_cast<int>(clusters.size()); ++idx) {
      if (std::find(clusters[idx].begin(), clusters[idx].end(), left) !=
          clusters[idx].end()) {
        left_idx = idx;
      }
      if (std::find(clusters[idx].begin(), clusters[idx].end(), right) !=
          clusters[idx].end()) {
        right_idx = idx;
      }
    }
    if (left_idx != right_idx) {
      for (const auto &point : clusters[right_idx]) {
        clusters[left_idx].push_back(point);
      }
      clusters.erase(clusters.begin() + right_idx);
    }
    if (limit >= 0 && steps >= limit) {
      break;
    }
  }

  std::sort(clusters.begin(), clusters.end(),
            [](const auto &a, const auto &b) { return a.size() > b.size(); });
  for (auto cluster : clusters) {
    std::cout << "Cluster of size " << cluster.size() << std::endl;
  }

  return clusters[0].size() * clusters[1].size() * clusters[2].size();
}

auto part_one(const std::vector<Point> &data) { return solve(data, 1000); }

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_08.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
