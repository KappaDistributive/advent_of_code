#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input)
{
  std::vector<int> jumps;
  for (auto line: input)
  {
    jumps.push_back(std::stoi(line));
  }
  return jumps;
}

long part_one(const std::vector<std::string>& input)
{
  auto jumps = prepare_input(input);
  int position{0};
  long step{0};
  int jump_value;

  do
  {
    position += jumps[position]++;
    step++;
  }
  while (0 <= position && position < jumps.size());
  return step;
}

int part_two(const std::vector<std::string>& input)
{
  return 8;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_05.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
