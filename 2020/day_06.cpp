#include <cassert>

#include "../utils/input.hpp"

std::pair<std::vector<int>, std::vector<int>> gather_answers(const std::vector<std::string>& input)
{
  std::vector<int> answers;
  int group_index{0};
  std::vector<int> group_sizes;
  int group_size{0};

  for (size_t index{0}; index <= 'z' - 'a'; index++)
  {
    answers.push_back(0);
  }
  for (auto line: input)
  {
    if (line.size() == 0)
    {
      if(group_size > 0)
      {
        group_sizes.push_back(group_size);
        group_size = 0;
      }
      group_index++;
      for (size_t index{0}; index <= 'z' - 'a'; index++)
      {
        answers.push_back(0);
      }
    }
    else
    {
      group_size++;
      assert(answers.size() == (group_index + 1) * ('z' - 'a' + 1));
      for (auto character: line)
      {
        answers[group_index * ('z' - 'a' + 1) + character - 'a']++;
      }
    }
  }
  if (group_size > 0)
  {
    group_sizes.push_back(group_size);
  }
  return std::make_pair(group_sizes, answers);
}


int part_one(const std::vector<std::string>& input)
{
  int result{0};
  auto temp = gather_answers(input);
  auto group_sizes = std::get<0>(temp);
  auto group_answers = std::get<1>(temp);

  assert(group_answers.size() == group_sizes.size() * ('z' - 'a' + 1));

  for (auto answer: group_answers)
  {
    result += static_cast<int>(answer > 0);
  }

  return result;
}

int part_two(const std::vector<std::string>& input)
{
  int result{0};
  auto temp = gather_answers(input);
  auto group_sizes = std::get<0>(temp);
  auto group_answers = std::get<1>(temp);

  assert(group_answers.size() == group_sizes.size() * ('z' - 'a' + 1));

  for (size_t group_index{0}; group_index < group_sizes.size(); group_index++)
  {
    for (size_t index{0}; index < 'z' - 'a' + 1; index++)
    {
      if(group_answers[group_index * ('z' - 'a' + 1) + index] == group_sizes[group_index])
      {
        result++;
      }
    }
  }

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_06.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
