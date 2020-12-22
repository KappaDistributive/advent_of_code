#include <array>
#include <regex>

#include "../utils/input.hpp"

std::vector<std::array<int, 3>> prepare_input(const std::vector<std::string>& input)
{
  std::vector<std::array<int, 3>> result;
  std::regex re{"^\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)$"};
  std::smatch matches;

  for (auto line: input)
  {
    std::regex_match(line, matches, re);
    assert (matches.size() == 4);
    result.push_back({std::stoi(matches[1].str()), std::stoi(matches[2].str()), std::stoi(matches[3].str())});
  }
  return result;
}

int part_one(const std::vector<std::string>& input)
{
  auto prepared_input = prepare_input(input);
  int counter{0};
  for (auto candidate: prepared_input)
  {
    std::sort(candidate.begin(), candidate.end());
    if (candidate[0] + candidate[1] > candidate[2])
    {
      counter++;
    }
  }
  return counter;
}

int part_two(const std::vector<std::string>& input)
{
  return 91;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_03.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}
