#include <array>
#include <cassert>
#include <set>

#include "../utils/input.hpp"

enum Direction
{
  east,
  southeast,
  southwest,
  west,
  northwest,
  northeast
};

std::ostream& operator<< (std::ostream& os, const Direction& direction)
{
  switch (direction)
  {
    case east: os << "east"; break;
    case southeast: os << "southeast"; break;
    case southwest: os << "southwest"; break;
    case west: os << "west"; break;
    case northwest: os << "northwest"; break;
    case northeast: os << "northeast"; break;
  }
  return os;
}

Direction operator- (const Direction& direction)
{
  Direction result;

  switch (direction)
  {
    case east: result = west; break;
    case southeast: result = northwest; break;
    case southwest: result = northeast; break;
    case west: result = east; break;
    case northwest: result = southeast; break;
    case northeast: result = southwest; break;
  }
  return result;
}

std::vector<std::vector<Direction>> prepare_input(const std::vector<std::string>& raw_input)
{
  std::vector<std::vector<Direction>> result;
  for (auto line: raw_input)
  {
    std::vector<Direction> direction;
    for (size_t index{0}; index < line.size(); index++)
    {
      switch (line[index])
      {
        case 'e': direction.push_back(east); break;
        case 's':
          assert (index + 1 < line.size());
          if (line[index+1] == 'e')
          {
            direction.push_back(southeast);
          }
          else if (line[index+1] == 'w')
          {
            direction.push_back(southwest);
          }
          else
          {
            throw std::invalid_argument("Invalid direction: " + line.substr(index, 2));
          }
          index++;
          break;
        case 'w': direction.push_back(west); break;
        case 'n':
          assert (index + 1 < line.size());
          if (line[index+1] == 'e')
          {
            direction.push_back(northeast);
          }
          else if (line[index+1] == 'w')
          {
            direction.push_back(northwest);
          }
          else
          {
            throw std::invalid_argument("Invalid direction: " + line.substr(index, 2));
          }
          index++;
          break;
        default:
          throw std::invalid_argument("Invalid direction: " + line.substr(index, 1));
          break;
      }
    }
    result.push_back(direction);
  }
  return result;
}

template <size_t _width, size_t _height>
class Hexgrid
{
private:
  std::array<bool, _width * _height> grid;
  const size_t width, height;
  const std::pair<size_t, size_t> center;
  std::pair<int, int> location; 

  void check_if_location_is_in_bounds(std::pair<int, int> location)
  {
    assert (center.first + location.first >= 0);
    assert (center.first + location.first < width);
    assert (center.second + location.second >= 0);
    assert (center.second + location.second < height);
  }

public:
  Hexgrid()
    : width(_width), height(_height), center({_width / 2, _height / 2}), location({0, 0})
  {
    for (size_t y{0}; y < height; y++)
    {
      for (size_t x{0}; x < width; x++)
      {
        grid[y * width + x] = true;
      }
    }
  }

  std::pair<int, int> step(Direction direction)
  {
    switch (direction)
    {
      case east:      location.first++;                    break;
      case southeast: location.first++; location.second--; break;
      case southwest: location.second--;                   break;
      case west:      location.first--;                    break;
      case northwest: location.first--; location.second++; break;
      case northeast: location.second++;                   break;
    }
    check_if_location_is_in_bounds(location);
    return location;
  }

  bool flip(std::pair<int, int> location)
  {
    this->operator[](location) = !this->operator[](location);
    return this->operator[](location);
  }

  std::pair<int, int> to_origin ()
  {
    location = {0, 0};
    return location;
  }

  void update()
  {
    std::set<std::pair<int, int>> flips;
    int num_blacks;
    std::vector<std::pair<int, int>> offsets = {
      { 1,  0}, // east
      { 1, -1}, // southeast
      { 0, -1}, // southwest
      {-1,  0}, // west
      {-1,  1}, // northwest
      { 0,  1}, // northeast
    };
    size_t x_lower, x_upper, y_lower, y_upper;

    for (size_t y{0}; y < height; y++)
    {
      for (size_t x{0}; x < width; x++)
      {
        num_blacks = 0;

        for (auto offset: offsets)
        {
          if (
            static_cast<int>(y) + offset.second >= 0 && 
            static_cast<int>(y) + offset.second < height && 
            static_cast<int>(x) + offset.first >= 0 && 
            static_cast<int>(x) + offset.first < width
            )
          {
            if (!grid[(static_cast<int>(y) + offset.second) * width + (static_cast<int>(x) + offset.first)])
            {
              num_blacks++;
            }
          } 
        }
        
        if (!grid[y * width + x] && (num_blacks == 0 || num_blacks > 2))
        {
          flips.insert({x, y});
        }
        else if (grid[y * width + x] && num_blacks == 2)
        {
          flips.insert({x, y});
        }
      }
    }

    for (auto location: flips)
    {
      grid[location.second * width + location.first] = !grid[location.second * width + location.first];
    }
  }

  bool& operator[] (std::pair<int, int> location)
  {
    check_if_location_is_in_bounds(location);
    return grid[(static_cast<int>(center.second) + location.second) * width + (static_cast<int>(center.first) + location.first)];
  }

  int count_blacks() const
  {
    int count{0};
    for (size_t y{0}; y < height; y++)
    {
      for (size_t x{0}; x < width; x++)
      {
        if (!grid[y * width + x])
        {
          count++;
        }
      }
    }
    return count;
  }

  friend std::ostream& operator<< (std::ostream& os, Hexgrid<_width, _height>& hexgrid)
  {
    for (size_t y{0}; y < hexgrid.height; y++)
    {
      for (size_t offset{0}; offset < (hexgrid.height - y - 1); offset++)
      {
        os << " ";
      }
      for (size_t x{0}; x < hexgrid.width; x++)
      {
        os << (hexgrid.grid[(hexgrid.height - y - 1) * hexgrid.width + x] ? '#' : '.');
        if (x + 1 < hexgrid.width)
        {
          if ((hexgrid.height - y - 1) == hexgrid.center.second)
          {
            if (x + 1 == hexgrid.center.first )
            {
               os << "(";
            }
            else if(hexgrid.center.first == x)
            {
              os << ")";
            }
            else
            {
              os << " ";
            }
          }
          else
          {
            os << " ";
          }
        }
      }
      if (y + 1 < hexgrid.height)
      {
        os << std::endl;
      }
    }
    return os;
  }
};

int part_one(const std::vector<std::string>& input)
{
  bool verbose = true;
  auto directions = prepare_input(input);
  Hexgrid<41, 41> hexgrid;
  std::pair<int, int> location{0, 0};

  for (auto direction: directions)
  {
    for (auto step: direction)
    {
      location = hexgrid.step(step);
    }
    hexgrid.flip(location);
    hexgrid.to_origin();
  }
  if (verbose)
  {
    std::cout << "\n" << hexgrid << std::endl;
  }
  return hexgrid.count_blacks();
}

int part_two(const std::vector<std::string>& input)
{
  bool verbose = false;
  auto directions = prepare_input(input);
  Hexgrid<151, 151> hexgrid;
  std::pair<int, int> location{0, 0};

  for (auto direction: directions)
  {
    for (auto step: direction)
    {
      location = hexgrid.step(step);
    }
    hexgrid.flip(location);
    hexgrid.to_origin();
  }
  if (verbose)
  {
    std::cout << "\n" << hexgrid << std::endl;
  }
  for (size_t day{1}; day <= 100; day++)
  {
    hexgrid.update();
    if (verbose)
    {
      std::cout << "Day " << day << ": " << hexgrid.count_blacks() << std::endl;
      std::cout << "\n" << hexgrid << std::endl;
    }
  }
  return hexgrid.count_blacks();
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_24.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
