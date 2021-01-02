#include <array>

#include "../utils/input.hpp"

static const long UPPER_BOUND = 1000000;

long part_one(const std::string& input)
{
  long target{std::stol(input)};
  long house;

  std::array<long, UPPER_BOUND> houses = std::array<long, UPPER_BOUND>();
  for (long elf{1}; elf < UPPER_BOUND; elf++)
  {
    house = elf;
    while (house <= UPPER_BOUND)
    {
      houses[house - 1] += 10 * elf;
      house += elf;
    }
  }

  for (size_t index{0}; index < houses.size(); index++)
  {
    if (houses[index] >= target)
    {
      return index + 1;
    }
  }
  return -1;
}

int part_two(const std::string& input)
{
  return -111;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2015/data/input_20.txt"));
  const auto input = reader.get_lines()[0];
 
  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
