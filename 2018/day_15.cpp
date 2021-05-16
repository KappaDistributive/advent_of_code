#include <cassert>
#include <map>

#include "../utils/input.hpp"


struct Point {
  size_t x;
  size_t y;

  bool
  operator<(const Point& other) const noexcept {
    return this->y < other.y || (this->y == other.y && this->x < other.x);
  }
};


enum class Entity: size_t {
  open,
  wall,
  elf,
  goblin,
};


std::ostream&
operator<<(std::ostream& os, Entity entity) {
  switch (entity) {
    case Entity::open:   os << "."; break;
    case Entity::wall :  os << "#"; break;
    case Entity::elf:    os << "E"; break;
    case Entity::goblin: os << "G"; break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }

  return os;
}


Entity
create_entity(char character) {
  switch (character) {
    case '.': return Entity::open;   break;
    case '#': return Entity::wall;   break;
    case 'E': return Entity::elf;    break;
    case 'G': return Entity::goblin; break;
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
        if (entity != Entity::open) {
          m_entities.insert({Point{x, y}, entity});
        }
      }
    }
  }

  Entity
  operator[](Point point) const noexcept {
    return m_entities.count(point) > 0 ? m_entities.at(point) : Entity::open;
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
