#include "../utils/input.hpp"

long part_one(const std::vector<std::string>& input)
{
  const long subject_number{7};
  const long card_public_key{std::stoi(input[0])};
  const long door_public_key{std::stoi(input[1])};
  const long divider{20201227};

  long a{1}, b{1};
  while (true)
  {
    a = (a * subject_number) % divider;
    b = (b * card_public_key) % divider;
    if (a == door_public_key)
    {
      return b;
    }
  }
  return -1;
}

long part_two(const std::vector<std::string>& input)
{
  return 17;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_25.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
