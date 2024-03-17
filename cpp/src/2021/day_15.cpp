#include <limits>
#include <queue>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Candidate {
 private:
  Point m_point;
  int m_risk;

 public:
  Candidate(Point point, int risk) : m_point(point), m_risk(risk) {}

  int risk() const { return this->m_risk; }

  Point point() const { return m_point; }
};

class Compare {
 public:
  bool operator()(const Candidate& lhs, const Candidate& rhs) {
    return lhs.risk() > rhs.risk();
  }
};

class Cave {
 private:
  std::vector<int> m_risk_levels;
  size_t m_width;
  size_t m_height;
  size_t m_num_tiles;

  std::set<Point> neighbors(Point point) const {
    auto coordinates = point.coordinates();
    std::set<Point> result;

    for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
        if ((offset_x == 0 && offset_y != 0) ||
            (offset_x != 0 && offset_y == 0)) {
          if ((offset_x >= 0 || 0 < coordinates[0]) &&
              (offset_x < 0 ||
               coordinates[0] + offset_x <
                   static_cast<int>(this->m_width * this->m_num_tiles)) &&
              (offset_y >= 0 || 0 < coordinates[1]) &&
              (offset_y < 0 ||
               coordinates[1] + offset_y <
                   static_cast<int>(this->m_height * this->m_num_tiles))) {
            result.insert(point +
                          Point{std::array<int, 2>{offset_x, offset_y}});
          }
        }
      }
    }
    return result;
  }

 public:
  explicit Cave(const std::vector<std::string>& input, size_t num_tiles = 1)
      : m_height(0), m_num_tiles(num_tiles) {
    this->m_width = input[0].size();
    for (auto line : input) {
      assertm(this->m_width == line.size(),
              "Encountered lines of different lengths");
      ++this->m_height;
      for (auto level : line) {
        this->m_risk_levels.push_back(level - '0');
        assertm(
            1 <= this->m_risk_levels.back() && this->m_risk_levels.back() <= 9,
            "Encountered invalid risk level");
      }
    }

    assertm(this->m_width == this->m_height, "Cave is not square");
  }

  auto risk_level(size_t x, size_t y) const {
    auto level = this->m_risk_levels[(y % this->m_height) * this->m_width +
                                     (x % this->m_width)];
    level += (y / this->m_height) + (x / this->m_width);

    return ((level + 8) % 9) + 1;
  }

  auto risk_level(Point point) const {
    auto coordinates = point.coordinates();
    assertm(0 <= coordinates[0], "Encountered invalid coordinate");
    assertm(0 <= coordinates[1], "Encountered invalid coordinate");

    return this->risk_level(static_cast<size_t>(coordinates[0]),
                            static_cast<size_t>(coordinates[1]));
  }

  auto dijkstra() const {
    std::map<Point, int> minimal_risks;
    for (size_t y{0}; y < this->m_height * this->m_num_tiles; ++y) {
      for (size_t x{0}; x < this->m_width * this->m_num_tiles; ++x) {
        Point point{
            std::array<int, 2>{static_cast<int>(x), static_cast<int>(y)}};
        minimal_risks.insert(
            std::make_pair(point, std::numeric_limits<int>::max()));
      }
    }

    Point current_node{std::array<int, 2>{0, 0}};
    std::priority_queue<Candidate, std::vector<Candidate>, Compare> candidates;
    minimal_risks.at(Point{std::array<int, 2>{0, 0}}) = 0;
    candidates.push(Candidate{current_node, 0});
    const Point destination_node{std::array<int, 2>{
        static_cast<int>(this->m_width * this->m_num_tiles - 1),
        static_cast<int>(this->m_height * this->m_num_tiles - 1)}};

    while (current_node != destination_node) {
      current_node = candidates.top().point();
      candidates.pop();
      for (auto neighbor : this->neighbors(current_node)) {
        int local_risk;
        if (minimal_risks.at(current_node) < std::numeric_limits<int>::max()) {
          local_risk =
              minimal_risks.at(current_node) + this->risk_level(neighbor);
          if (local_risk < minimal_risks.at(neighbor)) {
            minimal_risks.at(neighbor) = local_risk;
            candidates.push(Candidate{neighbor, local_risk});
          }
        }
      }
    }

    return minimal_risks.at(destination_node);
  }
};

auto part_one(const std::vector<std::string>& input) {
  Cave cave(input);
  return cave.dijkstra();
}

auto part_two(const std::vector<std::string>& input) {
  Cave cave(input, 5);
  return cave.dijkstra();
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      std::format("../../data/2021/input_15{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;
  
  return 0;
}
