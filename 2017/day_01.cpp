#include "../utils/input.hpp"

long part_one(const std::string& input)
{
  std::vector<int> digits;
  for (auto character: input)
  {
    digits.push_back(character - '0');
  }
  long result{0};
  for (size_t index{0}; index <= digits.size(); index++)
  {
    if (digits[index % digits.size()] == digits[(index + 1) % digits.size()])
    {
      result += digits[index % digits.size()];
    }
  }
  return result;
}

int part_two(const std::string& input)
{
  return 2;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_01.txt"));
  auto input = reader.get_lines()[0];
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
