#include <map>

#include "../utils/input.hpp"

class GameOfLights
{
private:
  size_t width, height;
  std::string grid;

  size_t get_num_neighbors(size_t x, size_t y) const
  {
    size_t count{0};
    assert(y < height);
    assert(x < width);

    size_t x_lower{x > 0 ? x-1 : x}, x_upper{x+1 < width ? x+1 : x},
           y_lower{y > 0 ? y-1 : y}, y_upper{y+1 < height ? y+1 : y};

    for (size_t y_index{y_lower}; y_index <= y_upper; y_index++)
    {
      for (size_t x_index{x_lower}; x_index <= x_upper; x_index++)
      {
        if (y == y_index && x == x_index)
        {
          continue;
        }
        switch (grid[y_index * width + x_index])
        {
          case '#':
            count++;
            break;
          case '.':
            break;
          default:
            throw std::invalid_argument("Invalid grid entry: " + std::to_string(grid[y_index * width + x_index]));
            break;
        }
      }
    }
    return count;
  }

public:
  GameOfLights() = default;

  GameOfLights(const std::vector<std::string>& input)
  {
    width = input[0].size();
    height = 0;
    for (auto line: input)
    {
      assert (line.size() == width);
      grid += line;
      height++;
    }
  }

  bool get(size_t x, size_t y) const
  {
    switch (grid[y * width + x])
    {
      case '#':
        return true;
        break;
      case '.':
        return false;
        break;
      default:
        throw std::invalid_argument("Invalid grid entry: " + std::to_string(grid[y * width + x]));
        break;
    }
  }

  void step()
  {
    std::map<std::pair<size_t, size_t>, char> updates;
    for (size_t y{0}; y < height; y++)
    {
      for (size_t x{0}; x < width; x++)
      {
        size_t num_neighbors = this->get_num_neighbors(x, y);
        if (this->get(x, y) && (num_neighbors < 2 || num_neighbors > 3))
        {
          updates.insert(std::make_pair(std::make_pair(x, y), '.'));
        }
        else if(!this->get(x, y) && num_neighbors == 3)
        {
          updates.insert(std::make_pair(std::make_pair(x, y), '#'));
        }
      }
    }

    for (auto [coords, value]: updates)
    {
      auto [x, y] = coords;
      grid[y * width + x] = value;
    }
  }

  unsigned long long brightness() const {
    unsigned long long lights{0};
    for (auto value: grid)
    {
      if (value == '#')
      {
        lights++;
      }
    }
    return lights;
  }

  friend std::ostream& operator<< (std::ostream& os, const GameOfLights& game)
  {
    for (size_t y{0}; y < game.height; y++)
    {
      for (size_t x{0}; x < game.width; x++)
      {
        os << (game.get(x, y) ? '#' : '.');
      }
      os << "\n";
    }
  return os;
  }
};

int part_one(const std::vector<std::string>& input)
{
  GameOfLights game(input);
  std::cout << game << std::endl;

  for(size_t count{0}; count < 100; count++)
  {
    game.step();
    std::cout << game << std::endl;
  }
  return game.brightness();
}

int part_two(const std::vector<std::string>& input)
{
  int result{-1};
  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2015/data/input_18.txt"));
  const auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
