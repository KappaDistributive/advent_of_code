#include "../utils/input.hpp"

std::vector<std::vector<int>> prepare_input(const std::vector<std::string>& input)
{
  std::vector<std::vector<int>> data;
  for (auto line: input)
  {
    std::vector<int> row;
    for (auto entry: utils::split_string(line, '\t'))
    {
      row.push_back(std::stoi(entry));
    }
    data.push_back(row);
  }
  return data;
}

long part_one(const std::vector<std::string>& input)
{
  auto data = prepare_input(input);
  long result{0};
  for (auto row: data)
  {
    std::sort(row.begin(), row.end());
    result += row.back() - row[0];
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  auto data = prepare_input(input);
  long result{0};
  for (auto row: data)
  {
    for (size_t left_index{0}; left_index + 1 < row.size(); left_index++)
    {
      for (size_t right_index{left_index+1}; right_index < row.size(); right_index++)
      {
        if (row[left_index] % row[right_index] == 0)
        {
          result += row[left_index] / row[right_index];
          break;
        }
        else if (row[right_index] % row[left_index] == 0)
        {
          result += row[right_index] / row[left_index];
          break;
        }
      }
    }
  }
  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_02.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
