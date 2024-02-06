#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int64_t, 2>;

std::vector<size_t> empty(const std::vector<std::string> &map,
                          bool row = true) {
  std::vector<std::size_t> indices;
  if (row) {
    for (size_t i{0}; i < map.size(); ++i) {
      if (std::find(map[i].cbegin(), map[i].cend(), '#') == map[i].cend()) {
        indices.push_back(i);
      }
    }
  } else {
    for (size_t i{0}; i < map[0].size(); ++i) {
      bool empty{true};
      for (size_t j{0}; j < map.size(); ++j) {
        if (map[j][i] != '#') {
          empty = false;
          break;
        }
        if (empty) {
          indices.push_back(i);
        }
      }
    }
  }
  return indices;
}

std::vector<Point> extract_galaxies(const std::vector<std::string> &input) {
  std::vector<Point> galaxy_coordinates;
  for (size_t y{0}; y < input.size(); ++y) {
    for (size_t x{0}; x < input[y].size(); ++x) {
      if (input[y][x] == '#') {
        galaxy_coordinates.push_back(
            Point{{static_cast<int64_t>(x), static_cast<int64_t>(y)}});
      }
    }
  }

  return galaxy_coordinates;
}

std::vector<Point> calculate_offsets(const std::vector<Point> &galaxies,
                                     size_t size) {
  std::vector<Point> result(size);
  int64_t offset_x{0};
  for (int64_t index{0}; index < static_cast<int64_t>(size); ++index) {
    bool empty{true};
    for (const auto &galaxy : galaxies) {
      if (galaxy[0] == index) {
        empty = false;
        break;
      }
    }
    if (empty) {
      ++offset_x;
    }
    result[index][0] = offset_x;
  }
  int64_t offset_y{0};
  for (int64_t index{0}; index < static_cast<int64_t>(size); ++index) {
    bool empty{true};
    for (const auto &galaxy : galaxies) {
      if (galaxy[1] == index) {
        empty = false;
        break;
      }
    }
    if (empty) {
      ++offset_y;
    }
    result[index][1] = offset_y;
  }

  return result;
}

auto part_one(const std::vector<std::string> &input,
              int64_t expansion_factor = 2) {
  auto galaxies = extract_galaxies(input);
  auto offsets =
      calculate_offsets(galaxies, std::max(input.size(), input[0].size()));

  int64_t result{0};
  for (size_t left{0}; left < galaxies.size(); ++left) {
    Point offset_left{{(expansion_factor - 1) * offsets[galaxies[left][0]][0],
                       (expansion_factor - 1) * offsets[galaxies[left][1]][1]}};
    for (size_t right{left + 1}; right < galaxies.size(); ++right) {
      Point offset_right{
          {(expansion_factor - 1) * offsets[galaxies[right][0]][0],
           (expansion_factor - 1) * offsets[galaxies[right][1]][1]}};
      result += (galaxies[left] + offset_left)
                    .manhatten_distance(galaxies[right] + offset_right);
    }
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) {
  return part_one(input, 1000000);
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_11_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_11.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
