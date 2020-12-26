#include "../includes/md5.hpp"
#include "../utils/input.hpp"

const int PASSWORD_LENGTH = 8;
const int LEADING_ZEROS = 5;

std::pair<char, int> next_character(const std::string& input, const int offset)
{
  int shift{offset};
  std::string candidate;
  bool done{false};

  while (!done)
  {
    done = true;
    candidate = md5(input + std::to_string(shift));
    if ( candidate.size() < LEADING_ZEROS + 1)
    {
      done = false;
    }
    if (done)
    {
      for (size_t index{0}; index < LEADING_ZEROS; index++)
      {
        if ( candidate[index] != '0')
        {
          done = false;
          break;
        }
      }
    }
    shift++;
  }
  return {candidate[LEADING_ZEROS], shift};
}

std::string part_one(const std::string& input)
{
  std::string result;
  int offset{0};
  bool verbose{false};

  while (result.size() < PASSWORD_LENGTH)
  {
    auto [character, new_offset] = next_character(input, offset);
    offset = new_offset;
    result += character;
    if (verbose)
    {
      std::cout << result << std::endl;
    }
  }
  return result;
}

int part_two(const std::string& input)
{
  return 9;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_05.txt"));
  auto input = reader.get_lines()[0];
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}
