#include "../utils/input.hpp"

std::string dragon(const std::string& input)
{
  std::string result = input + '0';
  for (auto it = input.rbegin(); it != input.rend(); it++)
  {
    result += *it == '0' ? '1' : '0';
  }
  return result;
}

std::string checksum(const std::string& input, bool recursive = false)
{
  std::string result;
  for (size_t index{0}; index + 1 < input.size(); index += 2)
  {
    if (input[index] == input[index+1])
    {
      result += '1';
    }
    else
    {
      result += '0';
    }
  }
  if (recursive && result.size() % 2 == 0)
  {
    return checksum(result, recursive);
  }
  else
  {
    return result;
  }
}

std::string part_one(const std::string& input)
{
  std::string dragon_checksum = input;
  while (dragon_checksum.size() < 272)
  {
    dragon_checksum = dragon(dragon_checksum);
  }
  dragon_checksum = dragon_checksum.substr(0, 272);
  return checksum(dragon_checksum, true);
}

std::string part_two(const std::string& input)
{
  std::string dragon_checksum = input;
  while (dragon_checksum.size() < 35651584)
  {
    dragon_checksum = dragon(dragon_checksum);
  }
  dragon_checksum = dragon_checksum.substr(0, 35651584);
  return checksum(dragon_checksum, true);
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_16.txt"));
  auto input = reader.get_lines()[0];
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
