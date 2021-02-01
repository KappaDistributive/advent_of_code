#include <map>

#include "../utils/input.hpp"

template<size_t multiplicity>
bool contains_multiple(const std::string& candidate)
{
  std::map<char, size_t> counts;
  for (auto character: candidate)
  {
    counts.try_emplace(character, 0);
    counts.at(character)++;
  }

  for (auto [key, value]: counts)
  {
    if (value == multiplicity)
    {
      return true;
    }
  }

  return false;
}


size_t part_one(const std::vector<std::string>& input)
{
  size_t twin_count{0}, tripled_count{0};
  for (auto line: input)
  {
    twin_count += contains_multiple<2>(line);
    tripled_count += contains_multiple<3>(line);
  }

  return twin_count * tripled_count;
}

int part_two(const std::vector<std::string>& input)
{
  return 6666;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2018/data/input_02.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
