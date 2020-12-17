#include <map>
#include <set>

#include "../utils/input.hpp"

using Coordinate = std::tuple<int, int, int>;

class GameOfCubes
{
private:
  std::set<Coordinate> active_cubes;

  std::set<Coordinate> get_neighbors(const Coordinate& cube) const
  {
    std::set<Coordinate> neighbors;
    Coordinate neighbor;

    for (int offset_z{-1}; offset_z <= 1; offset_z++)
    {
      for (int offset_y{-1}; offset_y <= 1; offset_y++)
      {
        for (int offset_x{-1}; offset_x <= 1; offset_x++)
        {
          if (std::make_tuple(offset_x, offset_y, offset_z) != std::make_tuple(0, 0, 0))
          {
            neighbor = {
              std::get<0>(cube) + offset_x,
              std::get<1>(cube) + offset_y,
              std::get<2>(cube) + offset_z
            };
            neighbors.insert(neighbor);
          }
        }
      }
    }
    assert (neighbors.size() == 26);

    return neighbors;
  }

  size_t num_neighbors(const Coordinate& cube) const
  {
    size_t count{0};
    auto neighbors = get_neighbors(cube);

    for (auto neighbor: neighbors)
    {
      if (active_cubes.count(neighbor))
      {
        count++;
      }
    }
    return count;
  }

  std::map<Coordinate, bool> get_populated_coordinates() const
  {
    std::map<Coordinate, bool> populated_coordinates;
    for (auto cube: active_cubes)
    {
      populated_coordinates.insert(std::make_pair(cube, true));
    }
    std::set<Coordinate> neighbors;
    for (auto [cube, _]: populated_coordinates)
    {
      for (auto neighbor: get_neighbors(cube))
      {
        neighbors.insert(neighbor);
      }
    }
    for (auto neighbor: neighbors)
    {
      if (populated_coordinates.count(neighbor) == 0)
      {
        populated_coordinates.insert(std::make_pair(neighbor, false));
      }
    }
    return populated_coordinates;
  }

public:
  GameOfCubes(const std::vector<std::string>& input)
  {
    for (size_t y{0}; y < input.size(); y++)
    {
      for (size_t x{0}; x < input[y].size(); x++)
      {
        switch (input[y][x])
        {
          case '#':
            active_cubes.insert({x, y, 0});
            break;
          case '.':
            break;
          default:
            throw std::invalid_argument("Invalid cube: " + std::to_string(input[y][x]));
            break;
        }
      }
    }
  }

  void step()
  {
    std::map<Coordinate, bool> updates;
    auto populated_coordinates = get_populated_coordinates();
    size_t count;

    for (auto [coordinate, active]: populated_coordinates)
    {
      count = num_neighbors(coordinate);
      if (active && !(count == 2 || count == 3))
      {
        updates.insert(std::make_pair(coordinate, false));
      }
      else if (!active && count == 3)
      {
        updates.insert(std::make_pair(coordinate, true));
      }
    }
   
    for (auto [coordinate, active]: updates)
    {
      if (active)
      {
        active_cubes.insert(coordinate);
      }
      else
      {
        active_cubes.erase(coordinate);
      }
    }
  }

  bool get(const Coordinate& coordinate) const
  {
    return active_cubes.count(coordinate);
  }

  bool get(int x, int y, int z) const
  {
    Coordinate coordinate(x, y, z);
    return get(coordinate);
  }

  size_t num_active_cubes() const
  {
    return active_cubes.size();
  }
};

int part_one(const std::vector<std::string>& input)
{
  GameOfCubes game(input);
  // std::cout << game.num_active_cubes() << std::endl;
  for (size_t index{0}; index < 6; index++)
  {
    game.step();
    // std::cout << game.num_active_cubes() << std::endl;
  }
  return game.num_active_cubes();
}

int part_two(const std::vector<std::string>& input)
{
  return 1;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_17.txt"));
  const std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
