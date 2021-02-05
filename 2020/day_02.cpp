#include <cassert>
#include <regex>
  
#include "../utils/input.hpp"

class Rule
{
private:
  std::pair<int, int> range;
  char letter;

public:
  Rule(std::string line)
  {
    std::smatch matches;
    std::regex_match(line, matches, std::regex("^(\\d+)-(\\d+)\\s(\\w)\\:\\s(\\w+)$"));
    assert(matches.size() == 5);

    range = std::make_pair(std::stoi(matches[1].str()), std::stoi(matches[2].str()));
    letter = matches[3].str()[0];
  }

  bool accepts(std::string line)
  {
    int count{0};

    for (auto character: line)
    {
      count += character == letter;
    }

    return std::get<0>(range) <= count && count <= std::get<1>(range);
  }

  bool new_accepts(std::string line)
  {
    return (line[std::get<0>(range)-1] == letter) ^ (line[std::get<1>(range)-1] == letter);
  }

  std::pair<int, int> get_range() const
  {
    return range;
  }

  char get_letter() const
  {
    return letter;
  }
};

std::vector<Rule> get_rules(std::vector<std::string> input)
{
  std::vector<Rule> rules;

  for (auto line: input)
  {
    rules.push_back(Rule(line));
  }

  return rules;
}

std::vector<std::string> get_passwords(std::vector<std::string> input)
{
  std::regex re("^(\\d+)-(\\d+)\\s(\\w)\\:\\s(\\w+)$");
  std::smatch matches;
  std::vector<std::string> passwords ;

  for (auto line: input)
  {
    std::regex_match(line, matches, re);
    assert(matches.size() == 5);
    passwords.push_back(matches[4]);
  }

  return passwords;
}

std::ostream& operator<< (std::ostream& os, const Rule& rule)
{
  os << std::get<0>(rule.get_range()) << "-" << std::get<1>(rule.get_range()) << " " << rule.get_letter();

  return os;
}

int part_one(std::vector<std::string> input)
{
  auto rules = get_rules(input);
  auto passwords = get_passwords(input);
  bool valid_password;
  int result{0};

  assert(rules.size() == passwords.size());

  for (size_t index{0}; index < rules.size(); index++)
  {
    result += rules[index].accepts(passwords[index]);
  }
  return result;
}

int part_two(std::vector<std::string> input)
{
  auto rules = get_rules(input);
  auto passwords = get_passwords(input);
  bool valid_password;
  int result{0};

  assert(rules.size() == passwords.size());

  for (size_t index{0}; index < rules.size(); index++)
  {
    result += rules[index].new_accepts(passwords[index]);
  }
  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_02.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
