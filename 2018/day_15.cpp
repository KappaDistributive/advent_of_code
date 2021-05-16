#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>

#include "../utils/input.hpp"


enum class Direction: size_t {
  north,
  east,
  south,
  west,
};


static const std::vector<Direction> DIRECTIONS{
  Direction::north, Direction::east, Direction::south, Direction::west};


std::ostream&
operator<<(std::ostream& os, Direction direction) {
  switch (direction) {
    case Direction::north: os << "Direction::north"; break;
    case Direction::east:  os << "Direction::east";  break;
    case Direction::south: os << "Direction::south"; break;
    case Direction::west:  os << "Direction::west"; break;
    default:
      throw std::runtime_error("This should never happen.");
      break;
  }

  return os;
}


struct Point {
  size_t x;
  size_t y;

  bool
  operator<(const Point& other) const noexcept {
    return this->y < other.y || (this->y == other.y && this->x < other.x);
  }
};


std::ostream&
operator<<(std::ostream& os, const Point& point) noexcept {
  os << "Point(x=" << point.x << ", y=" << point.y << ")";
  return os;
}


std::optional<Point>
operator+(Point point, Direction direction) {
  switch (direction) {
    case Direction::north:
      if (point.y > 0) {
        return Point{point.x, point.y - 1};
      } else {
        return std::nullopt;
      }
      break;
    case Direction::east:
      return Point{point.x + 1, point.y};
      break;
    case Direction::south:
      return Point{point.x, point.y + 1};
      break;
    case Direction::west:
      if (point.x > 0) {
        return Point{point.x - 1, point.y};
      } else {
        return std::nullopt;
      }
      break;
    default:
      throw std::runtime_error("This should never happen.");
      break;
  }
}



class Entity {
 protected:
  char m_symbol;

 public:
  explicit Entity(char symbol)
    : m_symbol(symbol) {
  }

  virtual ~Entity() = default;

  virtual bool
  is_unit() const noexcept {
    return false;
  }

  friend std::ostream&
  operator<<(std::ostream& os, const Entity& entity) {
    os << entity.m_symbol;
    return os;
  }

  virtual std::string
  to_string() const noexcept {
    std::string representation;
    representation += m_symbol;
    representation += "()";

    return representation;
  }
};


class Wall : public Entity {
 public:
  Wall() : Entity('#') {}
};


class Unit : public Entity {
 protected:
  int m_hit_points;

 public:
  explicit Unit(char symbol)
    : Entity(symbol),
      m_hit_points(200) {
  }

  bool
  is_unit() const noexcept {
    return true;
  }

  std::string
  to_string() const noexcept {
    std::string representation;
    representation += m_symbol;
    representation += "(" + std::to_string(m_hit_points) + ")";

    return representation;
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
  std::map<Point, std::shared_ptr<Entity>> m_entities;
  std::shared_ptr<Entity> m_wall;
  size_t m_width{0}, m_height{0};

  std::set<Point>
  open_squares(Point point) {
    std::set<Point> result;
    for (auto direction : DIRECTIONS) {
      auto candidate = point + direction;
      if (candidate.has_value() && m_entities.count(candidate.value()) == 0) {
        result.insert(candidate.value());
      }
    }

    return result;
  }

 public:
  explicit Battlefield(const std::vector<std::string>& input)
    : m_wall(std::make_shared<Entity>('.')) {
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
        Point point{x, y};

        switch (character) {
          case '.':
            break;
          case '#':
            m_entities.insert({point, std::make_unique<Wall>()});
            break;
          case 'E':
            m_entities.insert({point, std::make_unique<Elf>()});
            break;
          case 'G':
            m_entities.insert({point, std::make_unique<Goblin>()});
            break;
          default:
            throw std::invalid_argument("There is no entity of kind: " + std::to_string(character));
            break;
        }
      }
    }
  }

  std::shared_ptr<Entity>
  operator[](Point point) const noexcept {
    if (m_entities.count(point) > 0) {
      return m_entities.at(point);
    } else {
      return m_wall;
    }
  }

  friend std::ostream&
  operator<<(std::ostream& os, const Battlefield& battlefield) {
    for (size_t y{0}; y < battlefield.m_height; ++y) {
      std::stringstream ss_map, ss_meta;
      bool has_entered_meta{false};
      for (size_t x{0}; x < battlefield.m_width; ++x) {
        auto entity = battlefield.operator[](Point{x, y});
        ss_map << *entity;
        if (entity->is_unit()) {
          if (has_entered_meta) {
            ss_meta << ", ";
          }
          ss_meta << entity->to_string();
          has_entered_meta = true;
        }
      }
      os << ss_map.str() << "\t" << ss_meta.str();
      if (y+1 < battlefield.m_height) {
        os << "\n";
      }
    }

    return os;
  }

  void test() {
    auto open = open_squares(Point{1, 21});
    for (auto point : open) {
      std::cout << point << std::endl;
    }
  }
};


auto
part_one(const std::vector<std::string>& input) {
  Battlefield battlefield(input);
  battlefield.test();
  // std::cout << battlefield << std::endl;
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

