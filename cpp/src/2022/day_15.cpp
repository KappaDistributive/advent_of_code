#include <regex>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
static const std::regex sensor_regex{
    "^Sensor at x=(\\-?\\d+), y=(\\-?\\d+): closest beacon is at x=(\\-?\\d+), "
    "y=(\\-?\\d+)$"};

struct Sensor {
  Point position;
  Point closest_beacon;

  Sensor(const std::string &description) {
    std::smatch matches;
    std::regex_match(description, matches, sensor_regex);
    position =
        Point{{std::stoi(matches[1].str()), std::stoi(matches[2].str())}};
    closest_beacon =
        Point{{std::stoi(matches[3].str()), std::stoi(matches[4].str())}};
  }

  bool in_reach(const Point &position) const {
    return this->position.manhatten_distance(position) <=
           this->position.manhatten_distance(this->closest_beacon);
  }

  friend std::ostream &operator<<(std::ostream &os, const Sensor &sensor) {
    os << fmt::format("Sensor at x={}, y={}: closest beacon is at x={}, y={}",
                      sensor.position[0], sensor.position[1],
                      sensor.closest_beacon[0], sensor.closest_beacon[1]);
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  std::vector<Sensor> sensors;
  for (const auto &line : input) {
    sensors.push_back(Sensor{line});
  }

  Point pos{{0, 2000000}};
  size_t result{0};
  for (int x{-100000000}; x < 100000000; ++x) {
    pos[0] = x;
    for (const auto &sensor : sensors) {
      if (pos != sensor.closest_beacon && sensor.in_reach(pos)) {
        ++result;
        break;
      }
    }
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_15_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_15.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
