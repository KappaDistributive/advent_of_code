#include <map>
#include "../utils/input.hpp"

template<class T>
std::vector<std::vector<T>> subsequences(const std::vector<T>& sequence)
{
  std::vector<std::vector<T>> subsequences;

  for (size_t mask{0}; mask < (1 << sequence.size()); mask++)
  {
    std::vector<T> subsequence;
    for (size_t index{0}; index < sequence.size(); index++)
    {
      if ((1 << index) & mask)
      {
        subsequence.push_back(sequence[index]);
      }
    }
    subsequences.push_back(subsequence);
  }

  return subsequences;
}
int part_one(const std::vector<std::string>& input)
{
  int result{0};
  std::vector<int> containers;

  for (auto line: input)
  {
    containers.push_back(std::stoi(line));
  }

  auto s = subsequences(containers);

  for (auto subsequence: s)
  {
    int total_volume{0};
    for (auto volume: subsequence)
    {
      total_volume += volume;
    }
    if (total_volume  == 150)
    {
      result += 1;
    }
  }

  return result;
}

int part_two(const std::vector<std::string>& input)
{
  int result{-1};
  std::vector<int> containers;

  for (auto line: input)
  {
    containers.push_back(std::stoi(line));
  }

  auto s = subsequences(containers);

  for (auto subsequence: s)
  {
    int total_volume{0};
    int containers_used{0};
    for (auto volume: subsequence)
    {
      containers_used += 1;
      total_volume += volume;
    }
    if (total_volume  == 150 && (result == -1 || containers_used < result))
    {
      result = containers_used;
    }
  }

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2015/data/input_17.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
