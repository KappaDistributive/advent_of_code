#include <queue>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

Point step(const Point &position, const Point &move) { return position + move; }

struct Candidate {
  Point m_pos;
  size_t m_risk;
};

struct Compare {
  bool operator()(const Candidate &lhs, const Candidate &rhs) {
    return lhs.m_risk > rhs.m_risk;
  }
};

std::set<Point> step(const std::set<Point> &positions, Point move) {
  std::set<Point> result;
  for (auto position : positions) {
    result.insert(position + move);
  }
  return result;
}

Point get_move(char move) {
  switch (move) {
  case 'N':
    return Point{{0, -1}};
    break;
  case 'E':
    return Point{{1, 0}};
    break;
  case 'S':
    return Point{{0, 1}};
    break;
  case 'W':
    return Point{{-1, 0}};
    break;
  default:
    throw std::runtime_error("");
    break;
  }
}

struct Facility {
  std::set<Point> vertices;
  std::map<Point, std::set<Point>> edges;

  Facility(const std::string &description) {
    std::set<Point> current_positions{{Point{{0, 0}}}};
    std::set<Point> starts{{Point{{0, 0}}}};
    std::set<Point> ends;
    std::stack<std::pair<std::set<Point>, std::set<Point>>> stack;
    for (auto symbol : description) {
      switch (symbol) {
      case 'N':
      case 'E':
      case 'S':
      case 'W':
        for (auto pos : current_positions) {
          auto destination = step(pos, get_move(symbol));
          this->vertices.insert(pos);
          this->vertices.insert(destination);
          if (!edges.contains(pos)) {
            edges.insert({pos, {}});
          }
          if (!edges.contains(destination)) {
            edges.insert({destination, {}});
          }
          edges.at(pos).insert(destination);
          edges.at(destination).insert(pos);
        }
        current_positions = step(current_positions, get_move(symbol));
        break;
      case '|':
        for (auto pos : current_positions) {
          ends.insert(std::move(pos));
        }
        current_positions = starts;
        break;
      case '(':
        stack.push({starts, ends});
        starts = current_positions;
        ends.clear();
        break;
      case ')':
        for (auto pos : ends) {
          current_positions.insert(pos);
        }
        starts = std::get<0>(stack.top());
        ends = std::get<1>(stack.top());
        stack.pop();
        break;
      case '^':
        break;
      case '$':
        break;
      default:
        throw std::runtime_error("");
        break;
      }
    }
  }

  size_t dijkstra(const Point source, const Point destination) {

    std::map<Point, size_t> minimal_risk;
    for (auto vertex : this->vertices) {
      minimal_risk.insert(
          {std::move(vertex), std::numeric_limits<size_t>::max()});
    }
    Point current_node{source};
    minimal_risk.at(current_node) = 0;

    std::priority_queue<Candidate, std::vector<Candidate>, Compare> candidates;
    candidates.push(Candidate{current_node, 0});

    while (current_node != destination) {
      if (candidates.empty()) {
        throw std::runtime_error("");
      }
      current_node = candidates.top().m_pos;
      candidates.pop();
      for (auto neighbor : this->edges.at(current_node)) {
        if (minimal_risk.at(current_node) <
            std::numeric_limits<size_t>::max()) {
          if (minimal_risk.at(current_node) + 1 < minimal_risk.at(neighbor)) {
            minimal_risk.at(neighbor) = minimal_risk.at(current_node) + 1;
            candidates.push(Candidate{neighbor, minimal_risk.at(neighbor)});
          }
        }
      }
    }

    return minimal_risk.at(destination);
  }

  friend std::ostream &operator<<(std::ostream &os, const Facility &facility) {
    if (facility.vertices.size() == 0)
      return os;
    Point upper_left{*facility.vertices.begin()},
        lower_right{*facility.vertices.begin()};
    for (const auto &vertex : facility.vertices) {
      upper_left[0] = std::min(upper_left[0], vertex[0]);
      upper_left[1] = std::min(upper_left[1], vertex[1]);
      lower_right[0] = std::max(lower_right[0], vertex[0]);
      lower_right[1] = std::max(lower_right[1], vertex[1]);
    }
    int width{lower_right[0] - upper_left[0] + 1};
    int height{lower_right[1] - upper_left[1] + 1};

    Point point{{0, 0}};
    for (int y{0}; y < 2 * height; ++y) {
      if (y == 0) {
        for (int x{0}; x < width; ++x) {
          if (x == 0)
            os << '#';
          os << "##";
        }
        os << '\n';
      }
      point[1] = upper_left[1] + (y / 2);
      for (int x{0}; x < width; ++x) {
        point[0] = upper_left[0] + x;
        if (y % 2 == 0) {
          if (facility.vertices.count(point)) {
            if (x == 0) {
              os << '#';
            }
            os << '.';
            if (facility.vertices.count(point) &&
                facility.edges.at(point).count(point + Point{{1, 0}})) {
              os << '|';
            } else {
              os << '#';
            }
          } else {
            os << ' ';
          }
        } else {
          if (x == 0) {
            os << '#';
          }
          if (facility.vertices.count(point) &&
              facility.edges.at(point).count(point + Point{{0, 1}})) {
            os << '-';
          } else {
            os << '#';
          }
          os << '#';
        }
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::string &input) {
  std::cout << input << std::endl;
  Facility facility(input);
  std::cout << facility << std::endl;

  size_t result{0};
  for (auto destination : facility.vertices) {
    result = std::max(result, facility.dijkstra(Point{{0, 0}}, destination));
    // std::cout << Point{{0, 0}} << " -- " << facility.dijkstra(Point{{0, 0}},
    // destination) << " --> " << destination << std::endl;
  }
  return result;
}

auto part_two(const std::string &input) {
  Facility facility(input);
  size_t result{0};
  for (auto destination : facility.vertices) {
    if (facility.dijkstra(Point{{0, 0}}, destination) >= 1000) {
      ++result;
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2018/input_20_mock.txt"};
  std::filesystem::path input_path{"../../data/2018/input_20.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines()[0];

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
