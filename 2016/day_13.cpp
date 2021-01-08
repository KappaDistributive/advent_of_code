#include <set>

#include "../utils/input.hpp"

bool is_wall(const size_t& offset, const size_t& x, const size_t& y)
{
  unsigned long value{x*x + 3*x + 2*x*y + y + y*y + offset};
  int ones{0};
  for (size_t index{0}; index < 8*sizeof(value); index++)
  {
    ones += (value >> index) % 2;
  }
  return ones % 2;
}

void show_maze(const size_t offset, const size_t& max_x, const size_t& max_y, std::vector<std::pair<size_t, size_t>> path)
{
  std::cout << "  ";
  for (size_t x{0}; x <= max_x; x++)
  {
    std::cout << x;
  }
  std::cout << std::endl;
  for (size_t y{0}; y < max_y; y++)
  {
    std::cout << y << " ";
    for (size_t x{0}; x < max_x; x++)
    {
      if (std::find(path.begin(), path.end(), std::make_pair(x, y)) != path.end())
      {
        std::cout << 'O';
      }
      else
      {
        std::cout << (is_wall(offset, x, y) ? '#': '.');
      }
    }
    std::cout << std::endl;
  }
}

std::set<std::pair<size_t, size_t>> path_extensions(const size_t& offset, const std::vector<std::pair<size_t, size_t>>& path)
{
  std::set<std::pair<size_t, size_t>> extensions;
  auto [x, y] = path.back();
  size_t candidate_x, candidate_y;
  for (int offset_y{-1}; offset_y <= 1; offset_y++)
  {
    for (int offset_x{-1}; offset_x <= 1; offset_x++)
    {
      if (offset_x == 0 && offset_y == 0 || (offset_x != 0 && offset_y != 0) || (offset_x < 0 && x < -offset_x) || (offset_y < 0 && y < -offset_y))
      {
        continue;
      }
      candidate_x = x + offset_x;
      candidate_y = y + offset_y;
      if (!is_wall(offset, candidate_x, candidate_y) && std::find(path.begin(), path.end(), std::make_pair(candidate_x, candidate_y)) == path.end())
      {
        extensions.insert({candidate_x, candidate_y});
      }
    }
  }

  return extensions;
}


int part_one(const size_t& input)
{
  const size_t target_x{31}, target_y{39};
  std::set<std::vector<std::pair<size_t, size_t>>> paths = {{{1, 1}}};
  std::vector<std::pair<size_t, size_t>> shortest_path;
  bool found{false};
  while (!found)
  {
    std::set<std::vector<std::pair<size_t, size_t>>> new_paths;
    for (auto path: paths)
    {
      auto [x, y] = path.back();
      if (x == target_x && y == target_y)
      {
        found = true;
        shortest_path = path;
        break;
      }
      auto extensions = path_extensions(input, path);
      for (auto extension: extensions)
      {
        auto path_copy = path;
        path_copy.push_back(extension);
        new_paths.insert(path_copy);
      }
    }
    paths = new_paths;
  }
  show_maze(input, 50, 50, shortest_path);
  return shortest_path.size() - 1;
}

int part_two(const int& input)
{
  return 4321;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_13.txt"));
  auto input = std::stoi(reader.get_lines()[0]);
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
