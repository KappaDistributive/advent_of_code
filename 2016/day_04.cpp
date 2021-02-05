#include <cassert>
#include <map>
#include <regex>

#include "../utils/input.hpp"

class Room
{
private:
  const std::string code;
  std::regex re_sector_id{"^\\D*(\\d+)\\D*$"};
  std::regex re_checksum{"^.*\\[(\\w+)\\]$"};

public:
  explicit Room (const std::string& code)
    : code(code)
  {
  }

  int get_sector_id() const
  {
    std::smatch matches;
    std::regex_match(code, matches, re_sector_id);
    assert (matches.size() == 2);

    return std::stoi(matches[1].str());
  }

  std::string get_name() const
  {
    std::string name;

    for (auto character: code)
    {
      if (character == '[' || isdigit(character))
      {
        break;
      }
      else if (character != '-')
      {
        name += character;
      }
    }
    return name;
  }

  std::string calculate_checksum() const
  {
    auto name = get_name();
    std::map<char, int> frequency;

    for (auto character: name)
    {
      if (frequency.count(character) > 0)
      {
        frequency.at(character)++;
      }
      else
      {
        frequency.insert(std::make_pair(character, 1));
      }
    }
    std::vector<std::pair<char, int>> sorted_frequency;

    for (auto [character, count]: frequency)
    {
      sorted_frequency.push_back(std::make_pair(character, count));
    }
    std::sort(sorted_frequency.begin(), sorted_frequency.end(), [] (const std::pair<char, int>& lhs, const std::pair<char, int>& rhs)
        {
          return (lhs.second > rhs.second || (lhs.second == rhs.second && lhs.first < rhs.first));
        }
    );

    std::string checksum;
    for (auto [character, _]: sorted_frequency)
    {
      checksum += character;
    }
    return checksum.substr(0, 5);
  }

  std::string get_checksum() const
  {
    std::smatch matches;
    std::regex_match(code, matches, re_checksum);
    assert (matches.size() == 2);
    return matches[1].str();
  }

  bool is_valid() const
  {
    return calculate_checksum() == get_checksum();
  }
};

std::string rot(const std::string& input, int rotation)
{
  assert (rotation >= 0);
  std::string result;
  for (auto character: input)
  {
    assert (character >= 'a' && character <= 'z');
    result += 'a' + ((character - 'a' + rotation) % ('z' + 1 - 'a'));
  }
  return result;
}

int part_one(const std::vector<std::string>& input)
{
  int result{0};
  for (auto code: input)
  {
    Room room(code);
    if (room.is_valid())
    {
      result += room.get_sector_id();
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  int result{-1};
  bool verbose{false};
  for (auto code: input)
  {
    Room room(code);
    if (verbose)
    {
      std::cout << room.get_sector_id() << ": " << rot(room.get_name(), room.get_sector_id()) << std::endl;
    }
    if (rot(room.get_name(), room.get_sector_id()) == "northpoleobjectstorage")
    {
      result = room.get_sector_id();
    }
  }
  return result; 
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_04.txt"));
  auto input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}
