#include <map>
#include <regex>

#include "../utils/input.hpp"

class Bag
{
private:
  std::string description, style, color;
  std::vector<std::pair<int, Bag*>> content;

public:
  Bag() = default;

  explicit Bag(std::string description, std::string style, std::string color) : description(description), style(style), color(color)
  {

  }

  std::string get_type() const 
  {
    return style + " " + color;
  }

  std::string get_description() const
  {
    return description;
  }

  int get_bag_count() const
  {
    if (content.size() == 0)
    {
      return 0;
    }
    else
    {
      int result{0};

      for (auto [amount, bag]: content)
      {
        result += amount * (1 + bag->get_bag_count());
      }

      return result;
    }
  }

  void add_content(int amount, Bag* bag)
  {
    content.push_back(std::make_pair(amount, bag));
  }

  bool contains(const std::string& type) const {
    if (this->get_type() == type)
    {
      return true;
    }
    for (auto [amount, bag]: content)
    {
      if (bag->contains(type))
      {
        return true;
      }
    }
    return false;
  }
};

std::map<std::string, Bag*> create_bags(const std::vector<std::string>& input)
{
  std::regex root_regex{"^(pale|drab|mirrored|posh|striped|bright|dim|light|dull|dark|dotted|wavy|vibrant|shiny|muted|clear|faded|plaid)\\s(chartreuse|gold|magenta|black|yellow|aqua|silver|beige|cyan|bronze|purple|orange|coral|plum|olive|maroon|violet|salmon|teal|tan|red|crimson|indigo|green|brown|gray|lime|fuchsia|lavender|turquoise|white|tomato|blue|)\\sbags.*$"};
  std::regex content_regex{"(\\d+)\\s(pale|drab|mirrored|posh|striped|bright|dim|light|dull|dark|dotted|wavy|vibrant|shiny|muted|clear|faded|plaid)\\s(chartreuse|gold|magenta|black|yellow|aqua|silver|beige|cyan|bronze|purple|orange|coral|plum|olive|maroon|violet|salmon|teal|tan|red|crimson|indigo|green|brown|gray|lime|fuchsia|lavender|turquoise|white|tomato|blue|)\\sbag"};
  int amount;
  std::smatch matches;
  std::string style, color;
  std::map<std::string, Bag*> bag_index;

  for (auto description: input)
  {
    std::regex_match(description, matches, root_regex);
    assert(matches.size() == 3);
    style = matches[1].str();
    color = matches[2].str();
    Bag* bag = new Bag(description, style, color);
    bag_index.insert(std::make_pair(bag->get_type(), bag));
  }

  for (auto [index, bag]: bag_index)
  {
    std::string content_description = bag->get_description();
    while (std::regex_search(content_description, matches, content_regex))
    {
      assert (matches.size() == 4);
      amount = std::stoi(matches[1].str());
      style = matches[2].str();
      color = matches[3].str();

      bag->add_content(amount, bag_index.at(style + " " + color));
      content_description = matches.suffix().str();
    }
  }

  return bag_index;
}

int part_one(const std::vector<std::string>& input)
{
  int result{0};
  auto bags = create_bags(input);

  for (auto [_, bag]: bags)
  {
    result += static_cast<int>(bag->get_type() != "shiny gold" && bag->contains("shiny gold"));
  }

  for (auto [_, bag]: bags)
  {
    delete bag;
  }

  return result;
}

int part_two(const std::vector<std::string>& input)
{
  auto bags = create_bags(input);

  int result = bags.at("shiny gold")->get_bag_count();

  for (auto [_, bag]: bags)
  {
    delete bag;
  }

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_07.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
