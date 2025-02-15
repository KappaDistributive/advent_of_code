#include <array>
#include <cassert>
#include <optional>
#include <sstream>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using utils::geometry::Direction;

char repr(const Direction &direction) {
  std::stringstream ss;
  ss << direction;
  return ss.str()[0];
}

class Cart {
private:
  size_t intersections_seen{0};
  std::pair<size_t, size_t> _position;
  Direction _direction;
  bool _is_removed{false};

  char ahead(const char &up, const char &right, const char &down,
             const char &left) {
    switch (_direction) {
    case Direction::Up:
      return up;
      break;
    case Direction::Right:
      return right;
      break;
    case Direction::Down:
      return down;
      break;
    case Direction::Left:
      return left;
      break;
    }

    throw std::runtime_error("This should never happen.");
  }

public:
  Cart(const std::pair<size_t, size_t> &position, const Direction &direction)
      : _position(position), _direction(direction) {}

  std::pair<size_t, size_t> position() const { return _position; }

  Direction direction() const { return _direction; }

  void remove() { _is_removed = true; }

  bool is_removed() const { return _is_removed; }

  void move(const char &up, const char &right, const char &down,
            const char &left) {
    if (is_removed()) {
      return;
    }
    auto in_front = ahead(up, right, down, left);
    switch (_direction) {
    case Direction::Up:
      std::get<1>(_position)--;
      break;
    case Direction::Right:
      std::get<0>(_position)++;
      break;
    case Direction::Down:
      std::get<1>(_position)++;
      break;
    case Direction::Left:
      std::get<0>(_position)--;
      break;
    }
    switch (in_front) {
    case '-':
      switch (_direction) {
      case Direction::Right:
        break;
      case Direction::Left:
        break;
      default:
        throw std::runtime_error("Invalid move!");
      }
      break;
    case '|':
      switch (_direction) {
      case Direction::Up:
        break;
      case Direction::Down:
        break;
      default:
        throw std::runtime_error("Invalid move!");
      }
      break;
    case '/':
      switch (_direction) {
      case Direction::Up:
        _direction = Direction::Right;
        break;
      case Direction::Right:
        _direction = Direction::Up;
        break;
      case Direction::Down:
        _direction = Direction::Left;
        break;
      case Direction::Left:
        _direction = Direction::Down;
        break;
      }
      break;
    case '\\':
      switch (_direction) {
      case Direction::Up:
        _direction = Direction::Left;
        break;
      case Direction::Right:
        _direction = Direction::Down;
        break;
      case Direction::Down:
        _direction = Direction::Right;
        break;
      case Direction::Left:
        _direction = Direction::Up;
        break;
      }
      break;
    case '+':
      switch (_direction) {
      case Direction::Up:
        switch (intersections_seen % 3) {
        case 0: // turn left
          _direction = Direction::Left;
          break;
        case 1: // go straight
          break;
        case 2: // turn right
          _direction = Direction::Right;
          break;
        }
        break;
      case Direction::Right:
        switch (intersections_seen % 3) {
        case 0: // turn left
          _direction = Direction::Up;
          break;
        case 1: // go straight
          break;
        case 2: // turn right
          _direction = Direction::Down;
          break;
        }
        break;
      case Direction::Down:
        switch (intersections_seen % 3) {
        case 0: // turn left
          _direction = Direction::Right;
          break;
        case 1: // go straight
          break;
        case 2: // turn right
          _direction = Direction::Left;
          break;
        }
        break;
      case Direction::Left:
        switch (intersections_seen % 3) {
        case 0: // turn left
          _direction = Direction::Down;
          break;
        case 1: // go straight
          break;
        case 2: // turn right
          _direction = Direction::Up;
          break;
        }
        break;
      }
      intersections_seen++;
      break;
    default:
      throw std::runtime_error("Crash!");
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Cart &cart) {
    os << cart._direction;

    return os;
  }
};

template <size_t _width, size_t _height> class Track {
private:
  const size_t height{_height}, width{_width};
  std::vector<Cart> carts;
  std::array<char, _width * _height> _track;
  std::pair<size_t, size_t> crash_site{_width + 1, _height + 1};

  void extract_carts(const std::vector<std::string> &input) {
    assert(input.size() == height);
    for (size_t y{0}; y < height; y++) {
      auto line = input[y];
      assert(line.size() == width);
      for (size_t x{0}; x < width; x++) {
        std::pair<size_t, size_t> position{x, y};
        switch (line[x]) {
        case '^':
          carts.push_back(Cart(position, Direction::Up));
          break;
        case '>':
          carts.push_back(Cart(position, Direction::Right));
          break;
        case 'v':
          carts.push_back(Cart(position, Direction::Down));
          break;
        case '<':
          carts.push_back(Cart(position, Direction::Left));
          break;
        default:
          break;
        }
      }
    }
  }

  void extract_track(const std::vector<std::string> &input) {
    assert(input.size() == height);
    for (size_t y{0}; y < height; y++) {
      auto line = input[y];
      assert(line.size() == width);
      for (size_t x{0}; x < width; x++) {
        auto character = line[x];
        std::pair<size_t, size_t> position{x, y};
        if (character == '^' || character == '>' || character == 'v' ||
            character == '<') {
          if (x == 0 || x + 1 == width) {
            track(position) = '|';
          } else if (y == 0 || y + 1 == height) {
            track(position) = '-';
          } else if (track({x - 1, y}) == '-' || track({x - 1, y}) == '/' ||
                     track({x - 1, y}) == '\\' || track({x - 1, y}) == '+') {
            track(position) = '-';
          } else {
            track(position) = '|';
          }
        } else {
          track(position) = character;
        }
      }
    }
  }

public:
  explicit Track(const std::vector<std::string> &input) {
    extract_carts(input);
    extract_track(input);
  }

  bool step() {
    bool crashed{false};
    crash_site = {_width + 1, _height + 1};
    std::sort(carts.begin(), carts.end(), [](Cart lhs, Cart rhs) {
      auto lhs_pos = lhs.position();
      auto rhs_pos = rhs.position();
      if (std::get<1>(lhs_pos) < std::get<1>(rhs_pos)) {
        return true;
      } else if (std::get<1>(lhs_pos) == std::get<1>(rhs_pos) &&
                 std::get<0>(lhs_pos) < std::get<0>(rhs_pos)) {
        return true;
      } else {
        return false;
      }
    });
    // std::cout << "Sorted carts:" << std::endl;
    for (auto &cart : carts) {
      auto [x, y] = cart.position();
      try {
        cart.move(y > 0 ? map({x, y - 1}, false) : ' ',
                  (x + 1) < width ? map({x + 1, y}, false) : ' ',
                  (y + 1) < height ? map({x, y + 1}, false) : ' ',
                  x > 0 ? map({x - 1, y}, false) : ' ');
      } catch (const std::runtime_error &e) {
        crashed = true;
        if (std::string(e.what()) == "Crash!") {
          crash_site = cart.position();
          for (auto it = carts.begin(); it != carts.end(); it++) {
            if (it->position() == crash_site) {
              it->remove();
            }
          }
        } else {
          throw e;
        }
      }
      // std::cout << "(" << std::get<0>(cart.position()) << ", "
      //           << std::get<1>(cart.position()) << ")" << std::endl;
    }

    return !crashed;
  }

  std::optional<Cart *> cart(const std::pair<size_t, size_t> &position) {
    for (auto cart : carts) {
      if (cart.position() == position) {
        return &cart;
      }
    }
    return std::nullopt;
  }

  size_t num_carts() const {
    size_t num_carts{0};
    for (auto cart : carts) {
      if (!cart.is_removed()) {
        num_carts++;
      }
    }

    return num_carts;
  }

  char &track(const std::pair<size_t, size_t> &position) {
    assert(std::get<0>(position) < width);
    assert(std::get<1>(position) < height);

    return _track[std::get<1>(position) * width + std::get<0>(position)];
  }

  char map(const std::pair<size_t, size_t> &position,
           bool show_crashes = true) {
    assert(std::get<0>(position) < width);
    assert(std::get<1>(position) < height);
    auto potential_cart = cart(position);
    if (show_crashes && crash_position() == position) {
      return 'X';
    } else if (potential_cart.has_value() &&
               !potential_cart.value()->is_removed()) {
      return repr(potential_cart.value()->direction());
    } else {
      return _track[std::get<1>(position) * width + std::get<0>(position)];
    }
  }

  std::pair<size_t, size_t> crash_position() const { return crash_site; }

  std::pair<size_t, size_t> final_cart_position() const {
    assert(num_carts() == 1);
    for (auto cart : carts) {
      if (!cart.is_removed()) {
        return cart.position();
      }
    }
    throw std::runtime_error("This show never happen.");
  }

  friend std::ostream &operator<<(std::ostream &os,
                                  Track<_width, _height> &track) {
    for (size_t y{0}; y < track.height; y++) {
      for (size_t x{0}; x < track.width; x++) {
        std::pair<size_t, size_t> position{x, y};
        auto potential_cart = track.cart(position);
        os << track.map(position);
      }
      os << std::endl;
    }

    return os;
  }
};

std::string part_one(const std::vector<std::string> &input) {
  Track<150, 150> track(input);
  // debug track:
  // Track<13, 6> track(std::vector<std::string>({
  //     std::string("/->-\\        "),
  //     std::string("|   |  /----\\"),
  //     std::string("| /-+--+-\\  |"),
  //     std::string("| | |  | v  |"),
  //     std::string("\\-+-/  \\-+--/"),
  //     std::string("  \\------/   ")
  // }));
  // size_t time{0};
  do {
    // std::cout << "Time: " << time << std::endl;
    // std::cout << track << std::endl;
    // time++;
  } while (track.step());
  // std::cout << "Time: " << time << std::endl;
  // std::cout << track << std::endl;

  std::stringstream ss;
  ss << std::get<0>(track.crash_position()) << ","
     << std::get<1>(track.crash_position());

  return ss.str();
}

std::string part_two(const std::vector<std::string> &input) {
  Track<150, 150> track(input);
  // debug track:
  // Track<7, 7> track(std::vector<std::string>({
  //     "/>-<\\  ",
  //     "|   |  ",
  //     "| /<+-\\",
  //     "| | | v",
  //     "\\>+</ |",
  //     "  |   ^",
  //     "  \\<->/"}));
  // size_t time{0};
  do {
    // std::cout << "Time: " << time << std::endl;
    // std::cout << track << std::endl;
    // time++;
    track.step();
  } while (track.num_carts() > 1);
  // std::cout << "Time: " << time << std::endl;
  // std::cout << track << std::endl;

  std::stringstream ss;
  ss << std::get<0>(track.final_cart_position()) << ","
     << std::get<1>(track.final_cart_position());

  return ss.str();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2018/input_13.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
