#include <numeric>
#include <set>

#include "../utils/input.hpp"

class Map {
 private:
  std::set<std::pair<size_t, size_t>> asteroids;
  size_t width{0}, height{0};

 public:
  explicit Map(const std::vector<std::string>& input) {
    for (size_t y{0}; y < input.size(); y++) {
      height++;
      if (width == 0) {
        width = input[y].size();
      } else {
        assert(width == input[y].size());
      }
      for (size_t x{0}; x < input[y].size(); x++) {
        if (input[y][x] == '#') {
          asteroids.insert({x, y});
        }
      }
    }
  }

  bool
  has_asteroid(const std::pair<size_t, size_t>& location) const {
    return asteroids.count(location) > 0;
  }

  bool
  has_asteroid(const size_t& x, const size_t& y) const {
    return has_asteroid({x, y});
  }

  bool
  visible(const std::pair<size_t, size_t>& observer,
          const std::pair<size_t, size_t>& object) const {
    if (observer == object) {
      return false;
    }
    int64_t x_delta = static_cast<int64_t>(object.first) -
      static_cast<int64_t>(observer.first);
    int64_t y_delta = static_cast<int64_t>(object.second) -
      static_cast<int64_t>(observer.second);
    auto gcd = std::gcd(x_delta, y_delta);
    auto x_step = x_delta / gcd;
    auto y_step = y_delta / gcd;

    auto location = observer;
    bool is_visible = true;
    while (location != object) {
       location.first += x_step;
       location.second += y_step;
       if (location != object && has_asteroid(location)) {
         is_visible = false;
         break;
       }
    }
    return is_visible;
  }

  size_t
  num_visible_asteroids(const std::pair<size_t, size_t>& location) const {
    size_t num_asteroids{0};
    if (asteroids.count(location) > 0) {
      for (auto asteroid : asteroids) {
        num_asteroids += visible(location, asteroid);
      }
    }
    return num_asteroids;
  }

  size_t get_width() const {
    return width;
  }

  size_t get_height() const {
    return height;
  }
};


size_t part_one(const std::vector<std::string>& input) {
  auto map = Map(input);
  std::pair<size_t, size_t> ideal_location{0, 0};
  size_t ideal_score{0};
  for (size_t y{0}; y < map.get_height(); y++) {
    for (size_t x{0}; x < map.get_width(); x++) {
      std::pair<size_t, size_t> location{x, y};
      auto score = map.num_visible_asteroids(location);
      if (score > ideal_score) {
        ideal_location = location;
        ideal_score = score;
      }
    }
  }
  std::cout << "Ideal location: ("
            << ideal_location.first << ", "
            << ideal_location.second
            << ")" << std::endl;
  return ideal_score;
}

int part_two(const std::vector<std::string>& input) {
  return 9;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_10.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

