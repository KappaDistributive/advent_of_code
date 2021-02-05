#include <array>
#include <cassert>
#include <regex>

#include "../utils/input.hpp"

std::vector<std::array<int, 3>> prepare_input(const std::vector<std::string>& input, bool part_one=true)
{
  std::vector<std::array<int, 3>> result;
  std::regex re{"^\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)$"};
  std::smatch matches;
  if (part_one)
  {
    for (auto line: input)
    {
      std::regex_match(line, matches, re);
      assert (matches.size() == 4);
      result.push_back({std::stoi(matches[1].str()), std::stoi(matches[2].str()), std::stoi(matches[3].str())});
    }
  }
  else
  {
    assert (input.size() % 3 == 0);
    std::array<int, 9> buffer;
    for (size_t index{0}; index + 2 < input.size(); index+=3)
    {
      for (size_t row{0}; row < 3; row++)
      {
        std::regex_match (input[index+row], matches, re);
        assert (matches.size() == 4);
        buffer[row * 3 + 0] = std::stoi(matches[1].str());
        buffer[row * 3 + 1] = std::stoi(matches[2].str());
        buffer[row * 3 + 2] = std::stoi(matches[3].str());
      }

      result.push_back({buffer[0 * 3 + 0], buffer[1 * 3 + 0], buffer[2 * 3 + 0]});
      result.push_back({buffer[0 * 3 + 1], buffer[1 * 3 + 1], buffer[2 * 3 + 1]});
      result.push_back({buffer[0 * 3 + 2], buffer[1 * 3 + 2], buffer[2 * 3 + 2]});
    }
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
  auto prepared_input = prepare_input(input, false);
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

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_03.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}
