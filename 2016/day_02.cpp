#include <array>

#include "../utils/input.hpp"

class Keypad
{
private:
  const std::array<int, 9> keypad = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  const int width{3}, height{3};
  std::pair<int, int> position{1, 1};

public:
  Keypad() = default;

  int step(char direction)
  {
    switch (direction)
    {
      case 'U': if (position.second > 0) position.second--; break;
      case 'D': if (position.second + 1 < height) position.second++; break;
      case 'L': if (position.first > 0) position.first--; break;
      case 'R': if (position.first + 1 < width) position.first++; break;
      default: throw std::invalid_argument("Invalid direction."); break;
    }
    // switch (direction)
    //
    // {
    //   case 'U': position.second = (position.second - 1 + height) % height; break;
    //   case 'D': position.second = (position.second + 1) % height; break;
    //   case 'L': position.first = (position.first - 1 + width) % width; break;
    //   case 'R': position.first= (position.first + 1) % width; break;
    //   default: throw std::invalid_argument("Invalid direction."); break;
    // }
    return keypad[position.second * width + position.first];
  }
};

int part_one(const std::vector<std::string>& input)
{
  Keypad keypad;
  int data;
  std::string result;

  for (auto line: input)
  {
    for (auto direction: line)
    {
      data = keypad.step(direction);
      std::cout << data << " ";
    }
    std::cout << std::endl;
    result += std::to_string(data);
  }
  return std::stoi(result);
}

int part_two(const std::vector<std::string>& input)
{
  return -77;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_02.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
