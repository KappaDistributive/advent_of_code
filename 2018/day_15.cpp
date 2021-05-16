#include <cassert>
#include <map>
#include <optional>

#include "../utils/input.hpp"


struct Point {
  size_t x;
  size_t y;

  bool
  operator<(const Point& other) const noexcept {
    return this->y < other.y || (this->y == other.y && this->x < other.x);
  }
};


class Entity {
 private:
  char m_symbol;

 public:
  explicit Entity(char symbol)
    : m_symbol(symbol) {
  }

  friend std::ostream&
  operator<<(std::ostream& os, const Entity& entity) {
    os << entity.m_symbol;
    return os;
  }
};


class Wall : public Entity {
 public:
  Wall() : Entity('#') {}
};


class Unit : public Entity {
 private:
  int m_hit_points;

 public:
  explicit Unit(char symbol)
    : Entity(symbol),
      m_hit_points(200) {
  }
};


class Elf : public Unit {
 public:
  Elf() : Unit('E') {}
};


class Goblin : public Unit {
 public:
  Goblin() : Unit('G') {}
};


enum class Direction: size_t {
  north,
  east,
  south,
  west,
};


std::optional<Entity>
create_entity(char character) {
  switch (character) {
    case '.': return std::nullopt; break;
    case '#': return Wall();       break;
    case 'E': return Elf();        break;
    case 'G': return Goblin();     break;
    default:
      throw std::invalid_argument("There is no entity of kind: " + std::to_string(character));
      break;
  }
}


class Battlefield {
 private:
  std::map<Point, Entity> m_entities;
  size_t m_width{0}, m_height{0};

 public:
  explicit Battlefield(const std::vector<std::string>& input) {
    for (size_t y{0}; y < input.size(); ++y) {
      auto line = input[y];
      m_height++;
      if (m_width == 0) {
        m_width = line.size();
      } else {
        assert(m_width == line.size());
      }
      for (size_t x{0}; x < line.size(); ++x) {
        auto character = line[x];
        auto entity = create_entity(character);
        if (entity.has_value()) {
          m_entities.insert({Point{x, y}, entity.value()});
        }
      }
    }
  }

  Entity
  operator[](Point point) const noexcept {
    return m_entities.count(point) > 0 ? m_entities.at(point) : Entity('.');
  }

  friend std::ostream&
  operator<<(std::ostream& os, const Battlefield& battlefield) {

    for (size_t y{0}; y < battlefield.m_height; ++y) {
      for (size_t x{0}; x < battlefield.m_width; ++x) {
        os << battlefield.operator[](Point{x, y});
      }
      if (y+1 < battlefield.m_height) {
        os << "\n";
      }
    }

    return os;
  }
};


auto
part_one(const std::vector<std::string>& input) {
  Battlefield battlefield(input);
  std::cout << battlefield << std::endl;
  return "";
}


// auto
// part_two(const std::vector<std::string>& input) {
//   return "";
// }


int
main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_15.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two =  part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
