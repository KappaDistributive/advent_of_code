#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<Point> coordinates;
  std::vector<Point> folds;
  bool parsing_coords{true};
  for (auto line : input) {
    if (line == "") {
      parsing_coords = false;
    } else if (parsing_coords) {
      auto splits = utils::split_string(line, ',');
      coordinates.push_back(Point{
          std::array<int, 2>{std::stoi(splits[0]), std::stoi(splits[1])}});
    } else {
      auto splits = utils::split_string(line, '=');
      if (splits[0].back() == 'x') {
        folds.push_back(Point{std::array<int, 2>{std::stoi(splits[1]), 0}});
      } else {
        folds.push_back(Point{std::array<int, 2>{0, std::stoi(splits[1])}});
      }
    }
  }

  return std::make_pair(coordinates, folds);
}

auto apply_fold(Point point, Point folding_line) {
  auto point_coordinates = point.coordinates();
  auto line_coordinates = folding_line.coordinates();
  assertm(line_coordinates[0] == 0 || line_coordinates[1] == 0,
          "Invalid folding line.");
  if (line_coordinates[0] == 0) {
    return Point{
        std::array<int, 2>{point_coordinates[0],
                           point_coordinates[1] > line_coordinates[1]
                               ? -point_coordinates[1] + 2 * line_coordinates[1]
                               : point_coordinates[1]}};
  } else {
    return Point{
        std::array<int, 2>{point_coordinates[0] > line_coordinates[0]
                               ? -point_coordinates[0] + 2 * line_coordinates[0]
                               : point_coordinates[0],
                           point_coordinates[1]}};
  }
}

auto apply_folds(const std::set<Point>& points,
                 const std::vector<Point>& folding_lines) {
  std::set<Point> result;
  for (auto point : points) {
    for (auto folding_line : folding_lines) {
      point = apply_fold(point, folding_line);
    }
    result.insert(point);
  }

  return result;
}

auto print(const std::set<Point>& coordinates) {
  std::string board;
  for (int y{0}; y < 8; ++y) {
    for (int x{0}; x < 50; ++x) {
      Point pos{std::array<int, 2>{x, y}};
      if (std::find(coordinates.begin(), coordinates.end(), pos) !=
          coordinates.end()) {
        board += '#';
      } else {
        board += '.';
      }
    }
    board += '\n';
  }
  return board;
}

auto part_one(const std::vector<std::string>& input) {
  auto [coordinates, folds] = prepare_input(input);

  std::set<Point> new_coordinates;
  for (auto coordinate : coordinates) {
    new_coordinates.insert(coordinate);
  }
  new_coordinates = apply_folds(new_coordinates, {folds[0]});
  return new_coordinates.size();
}

auto part_two(const std::vector<std::string>& input) {
  auto [coordinates, folds] = prepare_input(input);

  std::set<Point> new_coordinates;
  for (auto coordinate : coordinates) {
    new_coordinates.insert(coordinate);
  }
  new_coordinates = apply_folds(new_coordinates, folds);
  return print(new_coordinates);
}

int main(int argc, char* argv[]) {
  std::string extension;

  if (argc > 1) {
    extension = "_";
    extension.append(argv[1]);
  }
  std::filesystem::path input_path{
      std::format("../../data/2021/input_13{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
