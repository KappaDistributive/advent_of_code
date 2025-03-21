#include <cassert>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using utils::geometry::Direction;

class Ship {
private:
  Direction direction;
  std::pair<int, int> position;
  std::pair<int, int> waypoint;

  void turn(bool clockwise, bool part_two = false) {
    if (!part_two) {
      switch (direction) {
      case Direction::Up:
        direction = clockwise ? Direction::Right : Direction::Left;
        break;
      case Direction::Right:
        direction = clockwise ? Direction::Down : Direction::Up;
        break;
      case Direction::Down:
        direction = clockwise ? Direction::Left : Direction::Right;
        break;
      case Direction::Left:
        direction = clockwise ? Direction::Up : Direction::Down;
      }
    } else {
      int x{std::get<0>(waypoint)}, y{std::get<1>(waypoint)};
      if (clockwise) {
        waypoint = {y, -x};
      } else {
        waypoint = {-y, x};
      }
    }
  }

  void forward(int distance, bool part_two = false) {
    if (!part_two) {
      switch (direction) {
      case Direction::Up:
        std::get<1>(position) += distance;
        break;
      case Direction::Right:
        std::get<0>(position) += distance;
        break;
      case Direction::Down:
        std::get<1>(position) -= distance;
        break;
      case Direction::Left:
        std::get<0>(position) -= distance;
        break;
      }
    } else {
      std::get<0>(position) += distance * std::get<0>(waypoint);
      std::get<1>(position) += distance * std::get<1>(waypoint);
    }
  }

public:
  Ship() : direction(Direction::Right), position({0, 0}), waypoint({10, 1}) {}

  void turn(bool clockwise, int degrees, bool part_two = false) {
    assert(degrees % 90 == 0);

    for (int count{0}; count < degrees; count += 90) {
      turn(clockwise, part_two);
    }
  }

  void move(const std::string &instruction, bool part_two = false) {
    int data = std::stoi(instruction.substr(1));
    switch (instruction[0]) {
    case 'N':
      if (!part_two) {
        std::get<1>(position) += data;
      } else {
        std::get<1>(waypoint) += data;
      }
      break;
    case 'S':
      if (!part_two) {
        std::get<1>(position) -= data;
      } else {
        std::get<1>(waypoint) -= data;
      }
      break;
    case 'E':
      if (!part_two) {
        std::get<0>(position) += data;
      } else {
        std::get<0>(waypoint) += data;
      }
      break;
    case 'W':
      if (!part_two) {
        std::get<0>(position) -= data;
      } else {
        std::get<0>(waypoint) -= data;
      }
      break;
    case 'L':
      turn(false, data, part_two);
      break;
    case 'R':
      turn(true, data, part_two);
      break;
    case 'F':
      forward(data, part_two);
      break;
    default:
      throw std::runtime_error("Unrecognized instruction: " + instruction);
      break;
    }
  }

  std::pair<int, int> get_position() const { return position; }

  Direction get_direction() const { return direction; }
};

int part_one(const std::vector<std::string> &input) {
  Ship ship;
  for (auto instruction : input) {
    ship.move(instruction);
  }
  return std::abs(std::get<0>(ship.get_position())) +
         std::abs(std::get<1>(ship.get_position()));
}

int part_two(const std::vector<std::string> &input) {
  Ship ship;
  for (auto instruction : input) {
    ship.move(instruction, true);
  }
  return std::abs(std::get<0>(ship.get_position())) +
         std::abs(std::get<1>(ship.get_position()));
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2020/input_12.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
