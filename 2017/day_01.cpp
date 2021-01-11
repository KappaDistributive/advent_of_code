#include "../utils/input.hpp"

std::vector<int> get_digits(const std::string& input)
{
  std::vector<int> digits;
  for (auto character: input)
  {
    digits.push_back(character - '0');
  }
  return digits;
}

long calculate_answer(const std::vector<int>& digits, size_t offset)
{
  long result{0};
  for (size_t index{0}; index <= digits.size(); index++)
  {
    if (digits[index % digits.size()] == digits[(index + offset) % digits.size()])
    {
      result += digits[index % digits.size()];
    }
  }
  return result;
}

long part_one(const std::string& input)
{
  auto digits = get_digits(input);
  return calculate_answer(digits, 1);
}

int part_two(const std::string& input)
{
  auto digits = get_digits(input);
  return calculate_answer(digits, digits.size() / 2);
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
