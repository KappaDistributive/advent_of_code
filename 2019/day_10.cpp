#include <math.h>
#include <numeric>
#include <optional>
#include <set>

#include "../utils/input.hpp"

class Map {
 private:
  std::set<std::pair<size_t, size_t>> asteroids;
  size_t width{0}, height{0};
  std::optional<std::pair<size_t, size_t>> laser_location;

 public:
  explicit Map(const std::vector<std::string>& input,
               const std::optional<std::pair<size_t, size_t>>& laser_location
                 = std::nullopt)
      : laser_location(laser_location) {
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

  size_t size() const {
    return asteroids.size();
  }

  std::optional<std::pair<size_t, size_t>>
  get_target(std::optional<std::pair<size_t, size_t>>
             previous_target) {
    bool avoid_inline_shots = previous_target.has_value();

    std::pair<size_t, size_t> reference = previous_target.value_or(
      std::pair<size_t, size_t>({laser_location.value().first, 0}));
    long double best_angle{std::numeric_limits<long double>::max()};
    std::optional<std::pair<size_t, size_t>> target = std::nullopt;

    for (auto asteroid : asteroids) {
      if (visible(laser_location.value(), asteroid)) {
        std::pair<long double, long double> vec_reference = {
          static_cast<long double>(reference.first) -
          static_cast<long double>(laser_location.value().first),
          static_cast<long double>(reference.second) -
          static_cast<long double>(laser_location.value().second)};
        std::pair<long double, long double> vec_asteroid = {
          static_cast<long double>(asteroid.first) -
          static_cast<long double>(laser_location.value().first),
          static_cast<long double>(asteroid.second) -
          static_cast<long double>(laser_location.value().second)};
        long double dot_product = vec_reference.first * vec_asteroid.first +
                           vec_reference.second * vec_asteroid.second;
        long double determinant = vec_reference.first * vec_asteroid.second -
                                  vec_reference.second * vec_asteroid.first;
        long double angle = std::atan2(determinant, dot_product);
        if (avoid_inline_shots && angle <= 1e-3) {
          angle += 2 * M_PI;
        } else if (angle < 0) {
          angle += M_PI;
        }
        if (angle < best_angle) {
          best_angle = angle;
          target = asteroid;
        }
      }
    }
    return target;
  }

  std::optional<std::pair<size_t, size_t>>
  vaporize(std::optional<std::pair<size_t, size_t>> laser_target) {
    laser_target = get_target(laser_target);
    if (laser_target.has_value()) {
      asteroids.erase(laser_target.value());
    }
    return laser_target;
  }
};


std::pair<std::pair<size_t, size_t>, size_t>
part_one(const std::vector<std::string>& input) {
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
  return {ideal_location, ideal_score};
}

int part_two(const std::vector<std::string>& input) {
  auto [laser_location, _] = part_one(input);
  auto map = Map(input, laser_location);
  std::vector<std::pair<size_t, size_t>> vaporized_asteroids;
  std::optional<std::pair<size_t, size_t>> laser_target = std::nullopt;
  while (map.size() > 1) {
    laser_target = map.vaporize(laser_target);
    if (laser_target.has_value()) {
    std::cout << "Vaporized: (" << laser_target.value().first << ","
              << laser_target.value().second << ")" << std::endl;
      vaporized_asteroids.push_back(laser_target.value());
    }
  }
  auto asteroid = vaporized_asteroids[199];
  return 100 * asteroid.first + asteroid.second;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_10.txt"));
  auto input = reader.get_lines();

  auto answer_one =  std::get<1>(part_one(input));
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
