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

  std::vector<Point> border() const {
    int radius{this->position.manhatten_distance(this->closest_beacon)};
    Point pos{{this->position[0], this->position[1] - (radius + 1)}};
    std::vector<Point> result;
    for (int step{0}; step < radius + 1; ++step) {
      pos += Point{{1, 1}};
      result.push_back(pos);
    }
    assert(pos[1] == this->position[1]);
    for (int step{0}; step < radius + 1; ++step) {
      pos += Point{{-1, 1}};
      result.push_back(pos);
    }
    assert(pos[0] == this->position[0]);
    for (int step{0}; step < radius + 1; ++step) {
      pos += Point{{-1, -1}};
      result.push_back(pos);
    }
    assert(pos[1] == this->position[1]);
    for (int step{0}; step < radius + 1; ++step) {
      pos += Point{{1, -1}};
      result.push_back(pos);
    }
    assert(pos[0] == this->position[0]);
    assert(pos[1] == this->position[1] - radius + 1);

    auto center = this->position;
    assert(std::all_of(result.cbegin(), result.cend(),
                       [center, radius](const Point pos) {
                         return pos.manhatten_distance(center) == radius + 1;
                       }));

    return result;
  }

  friend std::ostream &operator<<(std::ostream &os, const Sensor &sensor) {
    os << std::format("Sensor at x={}, y={}: closest beacon is at x={}, y={}",
                      sensor.position[0], sensor.position[1],
                      sensor.closest_beacon[0], sensor.closest_beacon[1]);
    return os;
  }
};

auto parse_input(const std::vector<std::string> &input) {
  std::vector<Sensor> sensors;
  for (const auto &line : input) {
    sensors.push_back(Sensor{line});
  }

  return sensors;
}

auto part_one(const std::vector<Sensor> &sensors) {
  int min_x{std::numeric_limits<int>::max()},
      max_x{std::numeric_limits<int>::min()};
  for (const auto &sensor : sensors) {
    min_x = std::min(
        min_x, sensor.position[0] -
                   sensor.position.manhatten_distance(sensor.closest_beacon) -
                   1);
    max_x = std::max(
        max_x, sensor.position[0] +
                   sensor.position.manhatten_distance(sensor.closest_beacon) +
                   1);
  }
  Point pos{{0, 2000000}};
  size_t result{0};
  for (int x{min_x}; x <= max_x; ++x) {
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

auto part_two(const std::vector<Sensor> &sensors) {
  for (const auto &sensor : sensors) {
    for (const auto &pos : sensor.border()) {
      if (pos[0] < 0 || pos[0] > 4000000 || pos[1] < 0 || pos[1] > 4000000) {
        continue;
      }
      if (!std::any_of(sensors.cbegin(), sensors.cend(),
                       [&pos](const Sensor s) { return s.in_reach(pos); })) {
        return static_cast<int64_t>(pos[0]) * int64_t{4000000} +
               static_cast<int64_t>(pos[1]);
      }
    }
  }
  return int64_t{-1};
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_15_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_15.txt"};
  utils::Reader reader(input_path);
  auto input = parse_input(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
