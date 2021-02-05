#include <array>
#include <cassert>
#include <map>
#include <regex>

#include "../utils/input.hpp"

const size_t NUM_FIELDS = 20;

// Separate input into (rules, your_ticket, nearby_tickets)
std::tuple<std::vector<std::string>, std::string, std::vector<std::string>> prepare_input (const std::vector<std::string>& input)
{
  std::string your_ticket;
  std::vector<std::string> rules, nearby_tickets;
  int section{0};

  for (auto line: input)
  {
    if (line == "your ticket:")
    {
      section = 1;
    }
    else if (line == "nearby tickets:")
    {
      section = 2;
    }
    else if (line == "")
    {
      continue;
    }
    else
    {
      switch (section)
      {
        // rules
        case 0:
          rules.push_back(line);
          break;

        // your_ticket
        case 1:
          assert (your_ticket == "");
          your_ticket = line;
          break;

        // nearby_tickets
        case 2:
          nearby_tickets.push_back(line);
          break;
        default:
          throw std::invalid_argument("Invalid input.");
          break;
      }
    }
  }

  return std::make_tuple(rules, your_ticket, nearby_tickets);
}

class Rule
{
private:
  std::map<std::string, std::array<std::pair<int, int>, 2>> requirements;

public:
  Rule (const std::vector<std::string>& rules)
  {
    std::regex re{"^([\\w\\s]+):\\s(\\d+)-(\\d+)\\sor\\s(\\d+)-(\\d+)$"};
    std::smatch matches;
    for (auto rule: rules)
    {
      std::regex_match(rule, matches, re);
      assert (matches.size() == 6);
      std::array<std::pair<int, int>, 2> ranges = {
        std::make_pair(std::stoi(matches[2].str()), std::stoi(matches[3].str())),
        std::make_pair(std::stoi(matches[4].str()), std::stoi(matches[5].str()))
      };
      requirements.insert(std::make_pair(matches[1].str(), ranges));
    }
  }

  std::map<std::string, std::array<std::pair<int, int>, 2>> get_requirements() const
  {
    return requirements;
  }

  friend std::ostream& operator<< (std::ostream& os, const Rule& rule)
  {
    for (auto [name, ranges]: rule.requirements)
    {
      os << name << ": ";
      for (size_t index{0}; index < ranges.size(); index++)
      {
        os << std::get<0>(ranges[index]) << "-" << std::get<1>(ranges[index]);
        if (index + 1 < ranges.size())
        {
          os << " or ";
        }
      }
      os << "\n";
    }
    return os;
  }
};

class Ticket
{
private:
  std::array<int, NUM_FIELDS> fields;

public:
  explicit Ticket(const std::string& field_values)
  {
    auto splits = utils::split_string(field_values, ',');
    assert (splits.size() == fields.size());

    for (size_t index{0}; index < splits.size(); index++)
    {
      fields[index] = std::stoi(splits[index]);
    }
  }

  std::pair<bool, int> is_completely_invalid(const Rule& rule) const
  {
    bool matches_range;
    for (auto field: fields)
    {
      matches_range = false;

      for (auto [_, ranges]: rule.get_requirements())
      {
        for (auto range: ranges)
        {
          if (field >= std::get<0>(range) && field <= std::get<1>(range))
          {
            matches_range = true;
            break;
          }
        }
      }
      if (!matches_range)
      {
        return std::make_pair(true, field);
      }
    }
    return std::make_pair(false, -1);
  }

  friend std::ostream& operator<< (std::ostream& os, const Ticket& ticket)
  {
    for (size_t index{0}; index < ticket.fields.size(); index++)
    {
      os << ticket.fields[index];
      if (index + 1 < ticket.fields.size())
      {
        os << ", ";
      }
    }
    return os;
  }
};

int part_one(const std::vector<std::string>& input)
{
  auto [rules_str, your_ticket_str, nearby_tickets_str] = prepare_input(input);
  Ticket your_ticket(your_ticket_str);
  // std::cout << "your_ticket:\n" << your_ticket << std::endl;
  std::vector<Ticket> nearby_tickets;
  // std::cout << "nearby tickets:" << std::endl;
  for (auto nearby_ticket_str: nearby_tickets_str)
  {
    nearby_tickets.push_back(Ticket(nearby_ticket_str));
    // std::cout << Ticket(nearby_ticket_str) << std::endl;
  }
  Rule rule(rules_str);
  
  int result{0};
  for (auto nearby_ticket: nearby_tickets)
  {
    auto [is_invalid, value] = nearby_ticket.is_completely_invalid(rule);
    if (is_invalid)
    {
      result += value;
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  return 1;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_16.txt"));
  const std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
