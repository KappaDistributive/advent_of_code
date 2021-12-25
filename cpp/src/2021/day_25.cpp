#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<size_t, 2>;
class SeaCucumberVille {
 private:
  std::map<Point, char> m_map;
  size_t m_width;
  size_t m_height;

 public:
  explicit SeaCucumberVille(const std::vector<std::string>& input) {
    this->m_height = input.size();
    this->m_width = input[0].size();

    for (size_t y{0}; y < input.size(); ++y) {
      assertm(input[y].size() == this->m_width,
              "Encountered unexpected line-width.");
      for (size_t x{0}; x < this->m_width; ++x) {
        switch (input[y][x]) {
          case '>':
            this->m_map.insert({Point{{x, y}}, '>'});
            break;
          case 'v':
            this->m_map.insert({Point{{x, y}}, 'v'});
            break;
          case '.':
            break;
          default:
            throw std::runtime_error("Encountered unknown symbol.");
            break;
        }
      }
    }
  }

  bool step() {
    bool moved{false};
    // move east
    std::map<Point, char> new_map;
    for (auto [point, cucumber] : this->m_map) {
      if (cucumber == '>') {
        auto coordinates = point.coordinates();
        Point target{point};
        if (coordinates[0] + 1 < this->m_width) {
          target += Point{{1, 0}};
        } else {
          target = Point{{0, coordinates[1]}};
        }
        if (this->m_map.count(target) == 0) {
          new_map.insert({target, cucumber});
          moved = true;
        } else {
          new_map.insert({point, cucumber});
        }
      } else {
        new_map.insert({point, cucumber});
      }
    }
    this->m_map = new_map;

    // move south
    new_map = std::map<Point, char>();
    for (auto [point, cucumber] : this->m_map) {
      if (cucumber == 'v') {
        auto coordinates = point.coordinates();
        Point target{point};
        if (coordinates[1] + 1 < this->m_height) {
          target += Point{{0, 1}};
        } else {
          target = Point{{coordinates[0], 0}};
        }
        if (this->m_map.count(target) == 0) {
          new_map.insert({target, cucumber});
          moved = true;
        } else {
          new_map.insert({point, cucumber});
        }
      } else {
        new_map.insert({point, cucumber});
      }
    }
    this->m_map = new_map;

    return moved;
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const SeaCucumberVille& map) {
    for (size_t y{0}; y < map.m_height; ++y) {
      for (size_t x{0}; x < map.m_width; ++x) {
        Point point{{x, y}};
        if (map.m_map.count(point) > 0) {
          os << map.m_map.at(point);
        } else {
          os << '.';
        }
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  SeaCucumberVille map{input};
  size_t result{0};
  // std::cout << map << std::endl;
  do {
    ++result;
    // std::cout << map << std::endl;
  } while (map.step());
  std::cout << map << std::endl;

  return result;
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_25{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);

  return 0;
}
