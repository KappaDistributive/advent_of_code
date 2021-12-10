#include "../utils/input.hpp"

class Keypad {
 private:
  const std::array<int, 9> keypad = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  const int width{3}, height{3};
  std::pair<int, int> position{1, 1};

 public:
  Keypad() = default;

  int step(char direction) {
    switch (direction) {
      case 'U':
        if (position.second > 0) {
          position.second--;
        }
        break;
      case 'D':
        if (position.second + 1 < height) {
          position.second++;
        }
        break;
      case 'L':
        if (position.first > 0) {
          position.first--;
        }
        break;
      case 'R':
        if (position.first + 1 < width) {
          position.first++;
        }
        break;
      default:
        throw std::invalid_argument("Invalid direction.");
        break;
    }
    return keypad[position.second * width + position.first];
  }
};

class FancyKeypad {
 private:
  // clang-format off
  const std::array<char, 25> keypad = {
    'X', 'X', '1', 'X', 'X',
    'X', '2', '3', '4', 'X',
    '5', '6', '7', '8', '9',
    'X', 'A', 'B', 'C', 'X',
    'X', 'X', 'D', 'X', 'X'
  };
  // clang-format on
  const int width{5}, height{5};
  std::pair<int, int> position{2, 2};

 public:
  FancyKeypad() = default;

  char get(int x, int y) const {
    assert(x >= 0 && y >= 0);
    return keypad[(y % height) * width + (x % width)];
  }

  int step(char direction) {
    switch (direction) {
      case 'U':
        if (position.second > 0 &&
            get(position.first, position.second - 1) != 'X')
          position.second--;
        break;
      case 'D':
        if (position.second + 1 < height &&
            get(position.first, position.second + 1) != 'X')
          position.second++;
        break;
      case 'L':
        if (position.first > 0 &&
            get(position.first - 1, position.second) != 'X')
          position.first--;
        break;
      case 'R':
        if (position.first + 1 < width &&
            get(position.first + 1, position.second) != 'X')
          position.first++;
        break;
      default:
        throw std::invalid_argument("Invalid direction.");
        break;
    }
    return get(position.first, position.second);
  }
};

int part_one(const std::vector<std::string>& input) {
  Keypad keypad;
  int data;
  std::string result;

  for (auto line : input) {
    for (auto direction : line) {
      data = keypad.step(direction);
    }
    result += std::to_string(data);
  }
  return std::stoi(result);
}

std::string part_two(const std::vector<std::string>& input) {
  FancyKeypad keypad;
  char data;
  std::string result;

  for (auto line : input) {
    for (auto direction : line) {
      data = keypad.step(direction);
    }
    result += data;
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2016/input_02.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}

