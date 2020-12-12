#include "../utils/input.hpp"

enum Direction
{
  north,
  east,
  south,
  west
};

std::ostream& operator<< (std::ostream& os, Direction direction)
{
  switch (direction)
  {
    case north: os << "North"; break;
    case east: os << "East"; break;
    case south: os << "South"; break;
    case west: os << "West"; break;
  }

  return os;
}

class Ship
{
private:
  Direction direction;
  std::pair<int, int> position;

  void turn(bool clockwise)
  {
    switch (direction)
    {
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
  }

  void forward(int distance)
  {
    switch (direction)
    {
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
  }

public:
  Ship(): direction(east), position({0, 0})
  {
  }

  void turn(bool clockwise , int degrees)
  {
    assert (degrees % 90 == 0);

    for (int count{0}; count < degrees; count+=90)
    {
      turn(clockwise);
    }
  }

  void move(const std::string& instruction)
  {
    int data = std::stoi(instruction.substr(1));
    switch(instruction[0])
    {
      case 'N':
        std::get<1>(position) += data;
        break;
      case 'S':
        std::get<1>(position) -= data;
        break;
      case 'E':
        std::get<0>(position) += data;
        break;
      case 'W':
        std::get<0>(position) -= data;
        break;
      case 'L':
        turn(false, data);
        break;
      case 'R':
        turn(true, data);
        break;
      case 'F':
        forward(data);
        break;
      default:
        throw std::runtime_error("Unrecognized instruction: " + instruction);
        break;

    }
  }

  std::pair<int, int> get_position() const
  {
    return position;
  }

  Direction get_direction() const
  {
    return direction;
  }
};

int part_one(const std::vector<std::string>& input)
{
  Ship ship;
  for (auto instruction: input)
  {
    ship.move(instruction);
  }
  return std::abs(std::get<0>(ship.get_position())) + std::abs(std::get<1>(ship.get_position()));
}

int part_two(const std::vector<std::string>& input)
{
  int result{0};

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_12.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
