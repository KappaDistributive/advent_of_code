#include <regex>

#include "../utils/input.hpp"

bool is_abba(const std::string& candidate)
{
  for (size_t index{0}; index + 3 < candidate.size(); index++)
  {
    if (
      candidate[index] != candidate[index+1] &&
      candidate[index] == candidate[index+3] &&
      candidate[index+1] == candidate[index+2]
    )
    {
      return true;
    }
  }
  return false;
}

int part_one(const std::vector<std::string>& input)
{
  std::regex re_outside{"\\]\\w+|\\w+\\["};
  std::regex re_inside{"\\[\\w+\\]"};
  std::smatch matches;
  int result{0};
  bool is_valid;

  for (auto line: input)
  {
    is_valid = true;
    for (auto it = std::sregex_iterator(line.begin(), line.end(), re_inside); it != std::sregex_iterator(); it++)
    {
      if (is_abba(it->str()))
      {
        is_valid = false;
        break;
      }
    }
    if (is_valid)
    {
      is_valid = false;
      for (auto it = std::sregex_iterator(line.begin(), line.end(), re_outside); it != std::sregex_iterator(); it++)
      {
        if (is_abba(it->str()))
        {
          is_valid = true;
          break;
        }
      }
    }
    if (is_valid)
    {
      result++;
    }
  }

  return result;
}

int part_two(const std::vector<std::string>& input)
{
  return 7;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_07.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}
