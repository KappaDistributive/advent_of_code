#include<cassert>

#include "../utils/input.hpp"

enum Direction {
  north,
  east,
  south,
  west
};

std::ostream& operator<< (std::ostream& os, Direction direction) {
  switch (direction) {
    case north: os << "North"; break;
    case east: os << "East"; break;
    case south: os << "South"; break;
    case west: os << "West"; break;
  }

  return os;
}

class Ship {
 private:
  Direction direction;
  std::pair<int, int> position;
  std::pair<int, int> waypoint;

  void turn(bool clockwise, bool part_two = false) {
    if (!part_two) {
      switch (direction) {
        case north:
          direction = clockwise ? east : west;
          break;
        case east:
          direction = clockwise ? south : north;
          break;
        case south:
          direction = clockwise ? west : east;
          break;
        case west:
          direction = clockwise ? north : south;
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
        case north:
          std::get<1>(position) += distance;
          break;
        case east:
          std::get<0>(position) += distance;
          break;
        case south:
          std::get<1>(position) -= distance;
          break;
        case west:
          std::get<0>(position) -= distance;
          break;
      }
    } else {
      std::get<0>(position) += distance * std::get<0>(waypoint);
      std::get<1>(position) += distance * std::get<1>(waypoint);
    }
  }

 public:
  Ship(): direction(east), position({0, 0}), waypoint({10, 1}) {
  }

  void turn(bool clockwise , int degrees, bool part_two = false) {
    assert(degrees % 90 == 0);

    for (int count{0}; count < degrees; count+=90) {
      turn(clockwise, part_two);
    }
  }

  void move(const std::string& instruction, bool part_two = false) {
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

  std::pair<int, int> get_position() const {
    return position;
  }

  Direction get_direction() const {
    return direction;
  }
};

int part_one(const std::vector<std::string>& input) {
  Ship ship;
  for (auto instruction: input) {
    ship.move(instruction);
  }
  return std::abs(std::get<0>(ship.get_position())) + std::abs(std::get<1>(ship.get_position()));
}

int part_two(const std::vector<std::string>& input) {
  Ship ship;
  for (auto instruction: input) {
    ship.move(instruction, true);
  }
  return std::abs(std::get<0>(ship.get_position())) + std::abs(std::get<1>(ship.get_position()));
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_12.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

