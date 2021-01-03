#include <regex>

#include "../utils/input.hpp"

static const std::regex RE{"\\((\\d+)x(\\d+)\\)"};

std::string decompress(std::string input)
{
  std::smatch matches;
  std::string result;
  std::regex_search(input, matches, RE);

  if (matches[1].str().size() > 0)
  {
    size_t sequence_length = std::stoi(matches[1].str());
    size_t repetitions = std::stoi(matches[2].str());
    std::string chunk = matches.suffix().str().substr(0, sequence_length);
    std::string tail = matches.suffix().str().substr(sequence_length);
    result = matches.prefix().str();
    for (size_t index{0}; index < repetitions; index++)
    {
      result += chunk;
    }
    result += decompress(tail);
  }
  else
  {
    result = input;
  }
  return result;
}

long part_one(const std::vector<std::string>& input)
{
  long result{0};
  for (auto line: input)
  {
    result += decompress(line).size();
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  return 3;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_09.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
