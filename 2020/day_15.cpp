#include <cassert>
#include <map>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input)
{
  assert (input.size() == 1);
  auto splits = utils::split_string(input[0], ',');
  std::vector<int> result;

  for (auto split: splits)
  {
    result.push_back(std::stoi(split));
  }

  return result;
}

int solve(const std::vector<int>& numbers, int rounds)
{
  int number = numbers[0];
  std::vector<int> memory(rounds, 0);

  int round;
  for (round = 1; round < static_cast<int>(numbers.size()); round++)
  {
    memory[number] = round;
    number = numbers[round];
  }

  for (; round < rounds; round++)
  {
    int index = number;
    number = (memory[index] == 0) ? 0 : (round - memory[index]);
    memory[index] = round;
  }

  return number;
}

int part_one(const std::vector<std::string>& input)
{
  auto numbers = prepare_input(input);
  int number{numbers[numbers.size() - 1]};
  return solve(numbers, 2020);
}

int part_two(const std::vector<std::string>& input)
{
  auto numbers = prepare_input(input);
  int number{numbers[numbers.size() - 1]};
  return solve(numbers, 30000000);
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_15.txt"));
  const std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
