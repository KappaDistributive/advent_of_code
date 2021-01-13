#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input)
{
  std::vector<int> blocks;
  for (auto line: input)
  {
    blocks.push_back(std::stoi(line));
  }
  return blocks;
}

int part_one(const std::vector<std::string>& input)
{
  auto blocks = prepare_input(input);
  std::vector<std::vector<int>> states{{blocks}};
  size_t cycle{0};
  size_t index_max;
  int max;

  while (true)
  {
    max = blocks[0];
    index_max = 0;
    for (size_t index{0}; index < blocks.size(); index++)
    {
      if (blocks[index] > max)
      {
        index_max = index;
        max = blocks[index_max];
      }
    }
    blocks[index_max] = 0;
    while (max > 0)
    {
      index_max = (index_max + 1) % blocks.size();
      blocks[index_max]++;
      max--;
    }
    cycle++;
    if (std::find(states.begin(), states.end(), blocks) == states.end())
    {
      states.push_back(blocks);
    }
    else
    {
      break;
    }
  }
  return cycle;
}

int part_two(const std::vector<std::string>& input)
{
  return 77;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_06.txt"));
  auto input = utils::split_string(reader.get_lines()[0], '\t');
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
