#include <list>

#include "../utils/input.hpp"

long part_one(const std::string& input)
{
  std::list<std::pair<size_t, long>> elves;
  for (size_t elf{1}; elf <= std::stoi(input); elf++)
  {
    elves.push_back({elf, 1});
  }
  auto elf = elves.begin();

  while (elves.size() > 1)
  {
    auto right_neighbor = std::next(elf);
    if (right_neighbor == elves.end())
    {
      right_neighbor = elves.begin();
    }

    // std::cout << elf->first << ": " << elf->second << " <- " << right_neighbor->first << ": " << right_neighbor->second << std::endl;
    elf->second += right_neighbor->second;
    elves.erase(right_neighbor);
    elf = std::next(elf);
    if (elf == elves.end())
    {
      elf = elves.begin();
    }
  }

  return elves.front().first;
}

int part_two(const std::string& input)
{
  return 9;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_19.txt"));
  auto input = reader.get_lines()[0];
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
