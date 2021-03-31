#include "../utils/hexgrid.hpp"
#include "../utils/input.hpp"

using utils::hexgrid::Direction;
using utils::hexgrid::Point;

std::vector<Direction>
prepare_input(const std::vector<std::string>& input) {
  std::vector<Direction> directions;
  for (auto entry : input) {
    if (entry == "nw") {
      directions.push_back(Direction::kNorthWest);
    } else if (entry == "n") {
      directions.push_back(Direction::kNorth);
    } else if (entry == "ne") {
      directions.push_back(Direction::kNorthEast);
    } else if (entry == "se") {
      directions.push_back(Direction::kSouthEast);
    } else if (entry == "s") {
      directions.push_back(Direction::kSouth);
    } else if (entry == "sw") {
      directions.push_back(Direction::kSouthWest);
    } else {
      throw std::runtime_error("Encountered unexpected direction: " + entry);
    }
  }
  return directions;
}

size_t part_one(const std::vector<std::string>& input) {
  auto directions = prepare_input(input);
  Point point(0, 0);
  for (auto direction : directions) {
    point.step(direction);
  }

  return point.distance(Point(0, 0));
}

size_t part_two(const std::vector<std::string>& input) {
  auto directions = prepare_input(input);
  size_t result{0};
  Point point(0, 0);
  for (auto direction : directions) {
    point.step(direction);
    result = std::max(result, point.distance(Point(0, 0)));
  }

  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_11.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

