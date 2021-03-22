#include <algorithm>
#include <array>
#include <cassert>
#include <limits>
#include <optional>
#include <vector>

#include "../utils/input.hpp"

typedef std::pair<int, int> point;

std::ostream& operator<<(std::ostream& os, const point& coordinate) {
    os << "(" << coordinate.first << ", " << coordinate.second << ")";
    return os;
}

std::vector<point> prepare_input(const std::vector<std::string>& input) {
    std::vector<point> coordindates;
    for (auto line: input) {
        auto splits = utils::split_string(line, ',');
        assert(splits.size() == 2);
        coordindates.push_back({std::stoi(splits[0]), std::stoi(splits[1])});
    }
    return coordindates;
}

int distance(const point& x, const point& y) {
    return std::abs(x.first - y.first) + std::abs(x.second - y.second);
}

std::vector<point> find_closest(const point& x, const std::vector<point>& neighbors) {
    point clostest_neighbor{0, 0};
    int closest_distance{std::numeric_limits<int>::max()};

    for (auto neighbor: neighbors) {
        if (distance(x, neighbor) < closest_distance) {
            closest_distance = distance(x, neighbor);
            clostest_neighbor = neighbor;
        }
    }
    std::vector<point> closest_neighbors{};

    for (auto neighbor: neighbors) {
        if (distance(x, neighbor) == closest_distance) {
            closest_neighbors.push_back(neighbor);
        }
    }

    return closest_neighbors;
}

std::tuple<int, int, int, int> get_borders(const std::vector<point>& coordinates) {
    int min_x{std::numeric_limits<int>::max()};
    int max_x{std::numeric_limits<int>::min()};
    int min_y{std::numeric_limits<int>::max()};
    int max_y{std::numeric_limits<int>::min()};
    for (auto coordinate: coordinates) {
        min_x = std::min(min_x, coordinate.first);
        max_x = std::max(max_x, coordinate.first);
        min_y = std::min(min_y, coordinate.second);
        max_y = std::max(max_y, coordinate.second);
    }

    return {min_x, min_y, max_x, max_y} ;
}

class Map {
 private:
    void populate_map() {
        for (int y{0}; y <= max_y; y++) {
            for (int x{0}; x <= max_x; x++) {
                auto closest_neighbors = find_closest({x, y}, coordinates);
                if (closest_neighbors.size() == 1) {
                    this->operator[]({x, y}) = closest_neighbors[0];
                } else {
                    this->operator[]({x, y}) = std::nullopt;
                }
            }
        }
    }
 public:
    const std::vector<point>& coordinates;
    int min_x, min_y, max_x, max_y;
    std::vector<std::optional<point>> owners;
    Map(const std::vector<point>& coordinates) 
        : coordinates(coordinates) {
        min_x = std::get<0>(get_borders(coordinates));
        min_y = std::get<1>(get_borders(coordinates));
        max_x = std::get<2>(get_borders(coordinates));
        max_y = std::get<3>(get_borders(coordinates));

        owners.reserve((max_x + 1) * (max_y + 1));
        populate_map();
    }

    std::optional<point>& operator[](const point& coordinate) {
        assert(coordinate.first <= max_x && coordinate.second <= max_y);
        return std::ref(owners[coordinate.first * (max_y+1) + coordinate.second]);
    }

    std::vector<point> owned_land(const point& coordinate) {
        std::vector<point> land;
        if (std::find(coordinates.begin(), coordinates.end(), coordinate) == coordinates.end()) {
            return land;
        }
        for (int y{0}; y <= max_y; y++) {
            for (int x{0}; x <= max_x; x++) {
                if (this->operator[]({x,y}) == coordinate) {
                    land.push_back({x, y});
                }
            }
        }
        return land;
    }
};

size_t part_one(const std::vector<std::string>& input) {
    auto coordinates = prepare_input(input);
    Map map(coordinates);
    size_t result{0};

    for (auto coordinate: coordinates) {
        auto land = map.owned_land(coordinate);
        bool is_finite{true};
        for (auto p: land) {
            if (p.first <= map.min_x || p.second <= map.min_y || p.first >= map.max_x || p.second >= map.max_y) {
                is_finite = false;
                break;
            }
        }
        if (is_finite && land.size() > result) {
            std::cout << "New Champion: " << coordinate << " with score: " << land.size() << std::endl;
            result = land.size();
        }
    }
    return result;
}

size_t part_two(const std::vector<std::string>& input) {
    return 2;
}



int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_06.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

