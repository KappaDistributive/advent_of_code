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
      throw std::runtime_error("Found invalid line in input: " + line);
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

enum class Effect {
  drain_damage,
  drain_heal,
  shield,
  poison,
  recharge,
};

static const Effect ALL_EFFECTS[] = {
  Effect::drain_damage,
  Effect::drain_heal,
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
    case Effect::drain_damage:
      return Stats{duration, 0, 2, 0};
      break;
    case Effect::drain_heal:
      return Stats{duration, 0, 2, 0};
      break;
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
    : m_name(name),
      m_hit_points(hit_points),
      m_armor(armor) {
      for (auto effect : ALL_EFFECTS) {
        this->m_effects.insert({effect, stats(effect)});
      }
  }

  size_t armor() const noexcept {
    size_t true_amor{this->m_armor};
    for (auto [_, stats] : this->m_effects) {
      if (stats.duration > 0) {
        true_amor += stats.armor;
      }
    }

    return true_amor;
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
};

class Warrior : public Mob {
 private:
  size_t m_attack;
};

class Wizard : public Mob {
 private:
  size_t m_mana;

 public:
  bool cast_spell(Spell spell, Mob* opponent) {
    switch (spell) {
      case Spell::magic_missile:
        if (this->m_mana >= mana_cost(spell)) {
          this->m_mana -= mana_cost(spell);
          opponent->take_damage(4, true);
          return true;
        }
      default:
        throw std::runtime_error("This should never happen.");
        break;
    }
    return false;
  }
};

auto part_one(const std::vector<std::string>& input) {
  auto [hit_points, damage] = prepare_input(input);
  return hit_points * damage;
}

auto part_two(const std::vector<std::string>& input) { return ".."; }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_22.txt"));
  const auto input = reader.get_lines();

  std::cout << "This will take a few minutes..." << std::endl;
  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  std::cout << "Bear with me..." << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

