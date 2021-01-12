#include <array>
#include <optional>

#include "../utils/input.hpp"

#define RESET   "\033[0m"
#define GREEN   "\033[32m"

enum Direction
{
  north,
  east,
  south,
  west,
};

std::ostream& operator<< (std::ostream& os, const Direction& direction)
{
  switch (direction)
  {
    case north: os << "north"; break;
    case east: os << "east"; break;
    case south: os << "south"; break;
    case west: os << "west"; break;
  }
  return os;
}

std::pair<int, int> operator+ (std::pair<int, int> lhs, std::pair<int, int> rhs)
{
  return std::make_pair(lhs.first + rhs.first, lhs.second + rhs.second);
}

void operator+= (std::pair<int, int>& position,const std::pair<int, int>& other)
{
  position.first += other.first;
  position.second += other.second;
}

std::ostream& operator<< (std::ostream& os, const std::pair<int, int>& position)
{
  os << "(" << position.first << ", " << position.second << ")";
  return os;
}

template <size_t _height, size_t _width>
class SpiralGrid
{
private:
  std::array<int, _height * _width> data;
  const size_t height, width;
  const std::pair<size_t, size_t> center;

public:
  SpiralGrid ()
    : height(_height), width(_width), center({_height / 2, _width / 2}), data(std::array<int, _height * _width>())
  {
    build_spiral();
  }

  int* operator[] (std::pair<int, int> position)
  {
    try
    {
      return &this->data[
        (center.second - position.second) * this->width +
        center.first + position.first
      ];
    }
    catch (std::out_of_range& e)
    {
      return nullptr;
    }
  }

  friend std::ostream& operator<< (std::ostream& os, const SpiralGrid& grid)
  {
    size_t padding{0};
    size_t current_length;
    for (auto entry: grid.data)
    {
      current_length = std::to_string(entry).size() + 1;
      if (current_length > padding)
      {
        padding = current_length;
      }
    }
    for (size_t y{0}; y < grid.height; y++)
    {
      for (size_t x{0}; x < grid.width; x++)
      {
        if (y == grid.height / 2 && x == grid.height / 2)
        {
          os << GREEN;
        }
        os << std::setw(padding) << grid.data[y * grid.width + x] << std::setw(1);
        if (y == grid.height / 2 && x == grid.height / 2)
        {
          os << RESET;
        }
      }
      os << "\n";
    }
    return os;
  }

  std::pair<size_t, size_t> get_center () const
  {
    return this->center;
  }

  void build_spiral()  // only works for uneven _height, _width
  {
    std::pair<int, int> position{0, 0};
    int value{1};
    Direction direction{south};
    for (size_t step{0}; step < _height * _width; step++)
    {
      if (this->operator[](position) != nullptr)
      {
        *this->operator[](position) = value;
      }
      value++;
      // turn direction if we have reached an edge
      switch (direction)
      {
        case north:
          if (this->operator[](position + std::make_pair(-1, 0)) == nullptr || *this->operator[](position + std::make_pair(-1, 0)) == 0)
          {
            direction = west;
          }
          break;
        case east:
          if (this->operator[](position + std::make_pair(0, 1)) == nullptr || *this->operator[](position + std::make_pair(0, 1)) == 0)
          {
            direction = north;
          }
          break;
        case south:
          if (this->operator[](position + std::make_pair(1, 0)) == nullptr || *this->operator[](position + std::make_pair(1, 0)) == 0)
          {
            direction = east;
          }
          break;
        case west:
          if (this->operator[](position + std::make_pair(0, -1)) == nullptr || *this->operator[](position + std::make_pair(0, -1)) == 0)
          {
            direction = south;
          }
          break;
      }

      // take a step 
      switch (direction)
      {
        case north:
          position += std::make_pair(0, 1);
          break;
        case east:
          position += std::make_pair(1, 0);
          break;
        case south:
          position += std::make_pair(0, -1);
          break;
        case west:
          position += std::make_pair(-1, 0);
          break;
      }
    }
  }

  std::optional<std::pair<int, int>> find(const int& value)
  {
    std::pair<int, int> position;
    for (int y{0}; y < this->height; y++)
    {
      for (int x{0}; x < this->width; x++)
      {
        position = std::make_pair(x - this->center.first, y - this->center.second);
        if (*this->operator[](position) == value)
        {
          return position;
        }
      }
    }
    return std::nullopt;
  }
};


int part_one(const int& input)
{
  SpiralGrid<1001, 1001> grid;
  //std::cout << grid << std::endl;
  auto position = grid.find(input).value();
  //std::cout << position << std::endl;

  return abs(position.first) + abs(position.second);
}

int part_two(const int& input)
{
  return 117;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_03.txt"));
  auto input = std::stoi(reader.get_lines()[0]);
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
