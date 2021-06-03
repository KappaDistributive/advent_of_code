#include <map>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

std::pair<int, int> prepare_input(const std::vector<std::string>& input) {
  std::regex hit_points_regex{"^Hit Points: (\\d+)$"};
  std::regex damage_regex{"^Damage: (\\d+)$"};
  std::smatch matches;
  int hit_points, damage;

  for (auto line : input) {
    if (std::regex_match(line, matches, hit_points_regex)) {
      hit_points = std::stoi(matches[1].str());
    } else if (std::regex_match(line, matches, damage_regex)) {
      damage = std::stoi(matches[1].str());
    } else {
      throw std::runtime_error("Encountered invalid line in input: " + line);
    }
  }

  return {hit_points, damage};
}

enum class Spell {
  magic_missile,
  drain,
  shield,
  poison,
  recharge,
};

static const Spell ALL_SPELLS[] = {
    Spell::magic_missile, Spell::drain,    Spell::shield,
    Spell::poison,        Spell::recharge,
};

size_t mana_cost(Spell spell) {
  switch (spell) {
    case Spell::magic_missile:
      return 53;
      break;
    case Spell::drain:
      return 73;
      break;
    case Spell::shield:
      return 113;
      break;
    case Spell::poison:
      return 173;
      break;
    case Spell::recharge:
      return 229;
      break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }
}

size_t duration(Spell spell) {
  switch (spell) {
    case Spell::magic_missile:
      return 0;
      break;
    case Spell::drain:
      return 0;
      break;
    case Spell::shield:
      return 6;
      break;
    case Spell::poison:
      return 6;
      break;
    case Spell::recharge:
      return 5;
      break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }
}

enum class Effect {
  shield,
  poison,
  recharge,
};

static const Effect ALL_EFFECTS[] = {
    Effect::shield,
    Effect::poison,
    Effect::recharge,
};

struct Stats {
  size_t duration;
  size_t armor;
  size_t damage;
  size_t mana;
};

Stats stats(Effect effect, size_t duration = 0) {
  switch (effect) {
    case Effect::shield:
      return Stats{duration, 7, 0, 0};
      break;
    case Effect::poison:
      return Stats{duration, 0, 3, 0};
      break;
    case Effect::recharge:
      return Stats{duration, 0, 0, 101};
      break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }
}

class Mob {
 protected:
  std::string m_name;
  size_t m_hit_points;
  size_t m_armor;
  std::map<Effect, Stats> m_effects;

 public:
  Mob(std::string name, size_t hit_points, size_t armor)
      : m_name(name), m_hit_points(hit_points), m_armor(armor) {
    for (auto effect : ALL_EFFECTS) {
      this->m_effects.insert({effect, stats(effect)});
    }
  }

  size_t armor() const noexcept {
    size_t true_armor{this->m_armor};
    for (auto [_, stats] : this->m_effects) {
      if (stats.duration > 0) {
        true_armor += stats.armor;
      }
    }

    return true_armor;
  }

  bool take_damage(size_t damage, bool ingore_armor = false) noexcept {
    size_t true_damage = damage;
    if (!ingore_armor) {
      true_damage =
          this->armor() < true_damage ? (true_damage - this->armor()) : 0;
    }

    if (true_damage < this->m_hit_points) {
      this->m_hit_points -= true_damage;
      return false;
    } else {
      this->m_hit_points = 0;
      return true;
    }
  }

  Stats& effect(Effect effect) { return this->m_effects.at(effect); }
};

class Warrior : public Mob {
 private:
  size_t m_attack;

 public:
  void attack(Mob* opponent) { opponent->take_damage(this->m_attack); }
};

class Wizard : public Mob {
 private:
  size_t m_mana;

  std::vector<Spell> available_spells() const noexcept {
    std::vector<Spell> spells;
    for (auto spell : ALL_SPELLS) {
      if (mana_cost(spell) <= this->m_mana) {
        spells.push_back(spell);
      }
    }

    return spells;
  }

 public:
  bool cast_spell(Spell spell, Mob* opponent) {
    if (this->m_mana >= mana_cost(spell)) {
      this->m_mana -= mana_cost(spell);
      switch (spell) {
        case Spell::magic_missile:
          opponent->take_damage(4, true);
        case Spell::drain:
          opponent->take_damage(2, true);
          this->m_hit_points += 2;
          break;
        case Spell::shield:
          this->effect(Effect::shield).duration = 6;
          break;
        case Spell::poison:
          opponent->effect(Effect::poison).duration = 6;
          break;
        case Spell::recharge:
          this->effect(Effect::recharge).duration = 5;
          break;
        default:
          throw std::runtime_error("This should never happen.");
          break;
      }

      return true;
    }
    return false;
  }
};

auto part_one(const std::vector<std::string>& input) {
  auto [hit_points, damage] = prepare_input(input);
  return hit_points * damage;
}

// auto part_two(const std::vector<std::string>& input) { return ".."; }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_22.txt"));
  const auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

