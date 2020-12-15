#include <bitset>
#include <map>
#include <regex>

#include "../utils/input.hpp"

class Mask
{
private:
  std::bitset<36> data;
  std::bitset<36> specified;

public:
  Mask () : data(0), specified(0)
  {
  }

  Mask (const std::string& input)
  {
    assert(input.size() == 36);
    for (size_t index{0}; index < input.size(); index++)
    {
      switch (input[35 - index])
      {
        case '1':
          data[index] = 1;
          specified[index] = 1;
          break;
        case '0':
          data[index] = 0;
          specified[index] = 1;
          break;
        case 'X':
          data[index] = 0;
          specified[index] = 0;
          break;
        default:
          throw std::invalid_argument("Invalid mask: " + input);
          break;
      }
    }
  }

  bool is_specified(size_t bit) const
  {
    assert (bit < 36);
    return specified[bit];
  }

  bool get_bit(size_t bit) const
  {
    assert (bit < 36);
    return data[bit];
  }

  std::string str()
  {
    std::string result;
    for (size_t index{0}; index < 36; index++)
    {
      if (specified[index])
      {
        result = (data[index] ? '1' : '0') + result;
      }
      else
      {
        result = 'X' + result;
      }
    }
    return result;
  }
};

class Value
{
private:
  std::bitset<36> data;
public:
  Value () = default;
  Value (const std::string& input)
  {
    data = std::stoull(input);
  }

  void apply (const Mask& mask)
  {
    for (size_t index{0}; index < 36; index++)
    {
      if (mask.is_specified(index))
      {
        data[index] = mask.get_bit(index);
      }
    }
  }

  operator unsigned long long()
  {
    unsigned long long value{0};
    for (size_t index{0}; index < 36; index++)
    {
      if (data[index])
      {
        value += (1ull << index);
      }
    }
    return value;
  }

  std::string str()
  {
    std::string result{""};
    for (size_t index{0}; index < 36; index++)
    {
      result = (data[index] ? '1' : '0') + result;
    }
    return result;
  }

  bool operator< (const Value& other) const
  {
    for (size_t index{0}; index < 36; index++)
    {
      if (this->data[35 - index] < other.data[35 - index])
      {
        return true;
      }
      else if (this->data[35 - index] > other.data[35 - index])
      {
        return false;
      }
    }
    return false;
  }
};

Value apply_mask(const Mask& mask, const Value& value)
{
  return value;
}

unsigned long long part_one(const std::vector<std::string>& input)
{
  Mask mask;
  Value value, address;
  std::map<Value, Value> memory;
  std::regex regex_mask{"^mask\\s=\\s([X01]{36})$"};
  std::regex regex_memory{"^mem\\[(\\d+)\\]\\s=\\s(\\d+)$"};
  std::smatch matches;

  for (auto line: input)
  {
    if (std::regex_match(line, matches, regex_mask))
    {
      assert (matches.size() == 2);
      mask = Mask(matches[1].str());
    }
    else
    {
      assert (std::regex_match(line, matches, regex_memory));
      assert (matches.size() == 3);
      address = Value(matches[1].str());
      value = Value(matches[2].str());
      value.apply(mask); 
      try
      {
        memory.at(address) = value;
      }
      catch (const std::out_of_range& e)
      {
        memory.insert(std::make_pair(address, value));
      }
    }
  }
  unsigned long long result{0};
  for (auto [_, value]: memory)
  {
    result += static_cast<unsigned long long>(value);
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  return 0;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_14.txt"));
  const std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
