#include <cassert>
#include <iomanip>
#include <regex>
#include <set>

#include "../utils/input.hpp"

static const std::string SHOP_DESCRIPTION =
  "Weapons:    Cost  Damage  Armor" "\n"
  "Dagger        8     4       0" "\n"
  "Shortsword   10     5       0" "\n"
  "Warhammer    25     6       0" "\n"
  "Longsword    40     7       0" "\n"
  "Greataxe     74     8       0" "\n"
  "" "\n"
  "Armor:      Cost  Damage  Armor" "\n"
  "Leather      13     0       1" "\n"
  "Chainmail    31     0       2" "\n"
  "Splintmail   53     0       3" "\n"
  "Bandedmail   75     0       4" "\n"
  "Platemail   102     0       5" "\n"
  "" "\n"
  "Rings:      Cost  Damage  Armor" "\n"
  "Damage +1    25     1       0" "\n"
  "Damage +2    50     2       0" "\n"
  "Damage +3   100     3       0" "\n"
  "Defense +1   20     0       1" "\n"
  "Defense +2   40     0       2" "\n"
  "Defense +3   80     0       3" "\n";

class Item
{
private:
  std::string name;
  int armor, cost, damage;

public:
  Item (const std::string& name, const int& armor, const int& cost, const int& damage)
    : name(name), armor(armor), cost(cost), damage(damage)
  {
  }
  friend std::ostream& operator<< (std::ostream& os, const Item& item)
  {
    os << item.name << ":: " << "Armor: " << item.armor << " Cost: " << item.cost << " : Damage " << item.damage;
    return os;
  }

  bool operator< (const Item& other) const
  {
    return (this->name < other.name);
  }

  int get_armor() const
  {
    return armor;
  }

  int get_cost() const
  {
    return cost;
  }

  int get_damage() const
  {
    return damage;
  }
};

class Armor : public Item
{
  using Item::Item;
};

class Ring : public Item
{
  using Item::Item;
};

class Weapon : public Item
{
  using Item::Item;
};

std::tuple<std::set<Armor>, std::set<Ring>, std::set<Weapon>> init_shop () 
{
  std::set<Armor> armor;
  std::set<Ring> rings;
  std::set<Weapon> weapons;
  std::regex item_regex{"^(\\w+(?:\\s\\+\\d)?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)$"};
  std::regex category_regex{"^(\\w+):\\s+Cost\\s+Damage\\s+Armor$"};
  std::smatch matches;
  std::string category;

  for (auto line: utils::split_string(SHOP_DESCRIPTION, '\n'))
  {
    if (line.size() == 0)
    {
      continue;
    }
    else if (std::regex_match(line, matches, category_regex))
    {
      assert (matches.size() == 2);
      category = "";
      for (char c: matches[1].str())
      {
        category += std::tolower(c);
      }
    }
    else
    {
      std::regex_match(line, matches, item_regex);
      {
        assert (matches.size() == 5);
        std::string name = matches[1].str();
        int defense = std::stoi(matches[4].str());
        int cost = std::stoi(matches[2].str());
        int damage = std::stoi(matches[3].str());

        if (category == "armor")
        {
          armor.insert(Armor(name, defense, cost, damage));
        }
        else if (category == "rings")
        {
          rings.insert(Ring(name, defense, cost, damage));
        }
        else if (category == "weapons")
        {
          weapons.insert(Weapon(name, defense, cost, damage));
        }
        else
        {
          throw std::invalid_argument("Invalid category: " + category);
        }
      }
    }
  }
    return {armor, rings, weapons};
}

class Character
{
private:
  std::string name;
  int hit_points;
  Weapon weapon;
  std::optional<Armor> armor;
  std::pair<std::optional<Ring>, std::optional<Ring>> rings;

public:
  explicit Character (const std::string& name, const int& hit_points, const Weapon& weapon, const std::optional<Armor>& armor, const std::optional<Ring>& left_ring, const std::optional<Ring>& right_right)
    : name(name), hit_points(hit_points), weapon(weapon), armor(armor), rings({left_ring, right_right})
  {
  }

  std::set<Item> get_items () const
  {
    std::set<Item> items;
    items.insert(weapon);
    if (armor.has_value())
    {
      items.insert(armor.value());
    }
    if (rings.first.has_value())
    {
      items.insert(rings.first.value());
    }
    if (rings.second.has_value())
    {
      items.insert(rings.second.value());
    }

    return items;
  }

  int get_hit_points () const
  {
    return hit_points;
  }

  int get_armor() const
  {
    int armor{0};
    for (auto item: get_items())
    {
      armor += item.get_armor();
    }
    return armor;
  }

  int get_damage() const
  {
    int damage{0};
    for (auto item: get_items())
    {
      damage += item.get_damage();
    }
    return damage;
  }

  int get_worth() const
  {
    int worth{0};
    for (auto item: get_items())
    {
      worth += item.get_cost();
    }
    return worth;
  }

  int take_damage (const int& damage)
  {
    int effective_damage = damage > this->get_armor() ? (damage - this->get_armor()) : 1;

    if (this->hit_points >= effective_damage)
    {
      this->hit_points -= effective_damage;
    }
    else
    {
      this ->hit_points = 0;
    }
    return this->get_hit_points();
  }

  bool attack(Character& other)
  {
    return other.take_damage(this->get_damage()) == 0;
  }

  friend std::ostream& operator<< (std::ostream& os, const Character& character)
  {
    os << character.name << ": " << "\n";
    os << "  Hitpoints:  " << character.hit_points << "\n";
    os << "  Armor:      " << character.get_armor() << "\n";
    os << "  Damage:     " << character.get_damage() << "\n";
    os << "  Weapon:     " << character.weapon << "\n";
    os << "  Armor:      ";
    if (character.armor.has_value())
    {
      os << character.armor.value();
    }
    os << "\n  Left Ring:  ";
    if (character.rings.first.has_value())
    {
      os << character.rings.first.value();
    }
    os << "\n  Right Ring: ";
    if (character.rings.second.has_value())
    {
      os << character.rings.second.value();
    }
    return os;
  }
};

Character init_villain (const std::vector<std::string>& input)
{
  std::vector<std::string> splits;
  int armor, damage, hit_points;

  for (auto line: input)
  {
    splits = utils::split_string(line, ':');
    if (splits[0] == "Armor")
    {
      armor = std::stoi(splits[1]);
    }
    else if (splits[0] == "Damage")
    {
      damage = std::stoi(splits[1]);
    }
    else if (splits[0] == "Hit Points")
    {
      hit_points = std::stoi(splits[1]);
    }
    else
    {
      throw std::invalid_argument("Invalid line in input: " + line);
    }
  }
  return Character("Villain", hit_points, Weapon("Frostmourne", 0, 0, damage), Armor("Helm of Domination", armor, 0, 0), {}, {});
}

bool fight(const Character& first, const Character& second)
{
  Character hero = first;
  Character villain = second;
  while (hero.get_hit_points() > 0 && villain.get_hit_points() > 0)
  {
    hero.attack(villain);
    if (villain.get_hit_points() > 0)
    {
      villain.attack(hero);
    }
  }
  return hero.get_hit_points() > 0;
}


int part_one(const std::vector<std::string>& input)
{
  auto [armor, rings, weapons] = init_shop();
  std::vector<Ring> ring_list;
  for (auto ring: rings)
  {
    ring_list.push_back(ring);
  }
  std::vector<int> wins;
  for (auto weapon: weapons)
  {
    for (auto armor: armor)
    {
      for (bool include_armor: {0, 1})
      {
        for (size_t left_ring{0}; left_ring <= ring_list.size(); left_ring++)
        {
          for (size_t right_ring{left_ring + 1}; right_ring <= ring_list.size() + 1; right_ring++)
          {
            std::optional<Armor> hero_armor = {};
            std::optional<Ring> hero_left_ring = {};
            std::optional<Ring> hero_right_ring = {};
            if (include_armor)
            {
              hero_armor = armor;
            }
            if (left_ring < ring_list.size())
            {
              hero_left_ring = ring_list[left_ring];
            }
            if (right_ring < ring_list.size())
            {
              hero_right_ring = ring_list[right_ring];
            }
            Character hero("Hero", 100, weapon, hero_armor, hero_left_ring , hero_right_ring);

            auto villain = init_villain(input);
            if(fight(hero, villain))
            {
              wins.push_back(hero.get_worth());
            }
          }
        }
      }
    }
  }

  return *std::min_element(wins.begin(), wins.end());
}

int part_two(const std::vector<std::string>& input)
{
  auto [armor, rings, weapons] = init_shop();
  std::vector<Ring> ring_list;
  for (auto ring: rings)
  {
    ring_list.push_back(ring);
  }
  std::vector<int> losses;
  for (auto weapon: weapons)
  {
    for (auto armor: armor)
    {
      for (bool include_armor: {0, 1})
      {
        for (size_t left_ring{0}; left_ring <= ring_list.size(); left_ring++)
        {
          for (size_t right_ring{left_ring + 1}; right_ring <= ring_list.size() + 1; right_ring++)
          {
            std::optional<Armor> hero_armor = {};
            std::optional<Ring> hero_left_ring = {};
            std::optional<Ring> hero_right_ring = {};
            if (include_armor)
            {
              hero_armor = armor;
            }
            if (left_ring < ring_list.size())
            {
              hero_left_ring = ring_list[left_ring];
            }
            if (right_ring < ring_list.size())
            {
              hero_right_ring = ring_list[right_ring];
            }
            Character hero("Hero", 100, weapon, hero_armor, hero_left_ring , hero_right_ring);

            auto villain = init_villain(input);
            if(!fight(hero, villain))
            {
              losses.push_back(hero.get_worth());
            }
          }
        }
      }
    }
  }
  return *std::max_element(losses.begin(), losses.end());
}

int main(int argc, char** argv)
{
  utils::Reader reader(std::filesystem::path("../2015/data/input_21.txt"));
  const auto input = reader.get_lines();
 
  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
