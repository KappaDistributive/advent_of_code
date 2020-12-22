#include <regex>

#include "../utils/input.hpp"

enum Direction
{
  north,
  east,
  south,
  west
};

class Path
{
private:
  std::pair<int, int> location;
  std::vector<std::pair<int, int>> path;
  Direction direction;
  std::regex re{"^\\s?([LR])(\\d+)$"};
  std::smatch matches;

public:
  Path ()
  {
    location = {0, 0};
    path.push_back(location);
    direction = north;
  }

  void step(const std::string& instruction)
  {
    std::regex_match(instruction, matches, re);
    assert (matches.size() == 3);
    char rotation = matches[1].str()[0];
    int distance = std::stoi(matches[2].str());

    switch (rotation)
    {
      case 'L':
        switch (direction)
        {
          case north: direction = west; break;
          case east: direction = north; break;
          case south: direction = east; break;
          case west: direction = south; break;
        }
        break;
      case 'R':
        switch (direction)
        {
          case north: direction = east; break;
          case east: direction = south; break;
          case south: direction = west; break;
          case west: direction = north; break;
        }
        break;
      default:
        throw std::invalid_argument("Invalid rotation.");
        break;
    }

    switch (direction)
    {
      case north: location.second += distance; break;
      case east: location.first += distance; break;
      case south: location.second -= distance; break;
      case west: location.first -= distance; break;
    }
    path.push_back(location);
  }

  std::pair<int, int> get_location() const
  {
    return location;
  }
};

int part_one(const std::vector<std::string>& input)
{
  Path path;
  for (auto instruction: input)
  {
    path.step(instruction);
  }
  auto location = path.get_location();
  return abs(location.first) + abs(location.second);
}

int part_two(const std::vector<std::string>& input)
{
  return 555;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_01.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
