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

    for(int step{0}; step < distance; step++)
    {
      switch (direction)
      {
        case north: location.second += 1; break;
        case east: location.first += 1; break;
        case south: location.second -= 1; break;
        case west: location.first -= 1; break;
      }
      path.push_back(location);
    }
  }

  std::pair<int, int> get_location() const
  {
    return location;
  }

  std::pair<bool, std::pair<int, int>> first_duplicate() const
  {
    for (size_t left_index{0}; left_index + 1 < path.size(); left_index++)
    {
      for (size_t right_index{left_index+1}; right_index < path.size(); right_index++)
      {
        if (path[left_index] == path[right_index])
        {
          return std::make_pair(true, path[left_index]);
        }
      }
    }
    return std::make_pair(false, std::make_pair(0, 0));
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
  Path path;
  for (auto instruction: input)
  {
    path.step(instruction);
  }
  auto [found, location] = path.first_duplicate();
  assert (found);
  return abs(location.first) + abs(location.second);
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_01.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
