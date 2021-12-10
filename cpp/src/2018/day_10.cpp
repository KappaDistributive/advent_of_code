#include <cassert>
#include <limits>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

class Star {
 private:
    int x, y;
    int velocity_x, velocity_y;

 public:
    Star(int x, int y, int velocity_x, int velocity_y)
        : x(x), y(y), velocity_x(velocity_x), velocity_y(velocity_y) {
    }

    explicit Star(const std::string& description) {
        std::regex re{"^position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>$"};  // NOLINT
        std::smatch matches;
        std::regex_match(description, matches, re);
        assert(matches.size() == 5);
        x = std::stoi(matches[1].str());
        y = std::stoi(matches[2].str());
        velocity_x = std::stoi(matches[3].str());
        velocity_y = std::stoi(matches[4].str());
    }

    std::pair<int, int> position(int time = 0) const {
        return {x + time * velocity_x, y + time * velocity_y};
    }

    friend std::ostream& operator<<(std::ostream& os, const Star& star) {
        os << "position=<" << star.x << ", " << star.y << "> "
        << "velocity=<" << star.velocity_x << ", " << star.velocity_y << ">";
        return os;
    }
};


class Starfield {
 private:
    std::vector<Star> stars;
    int initial_x_min, initial_y_min, initial_x_max, initial_y_max;

 public:
    explicit Starfield(const std::vector<std::string>& input) {
        for (auto line : input) {
            Star star(line);
            stars.push_back(Star(line));
        }

        auto border = this->border();
        initial_x_min = std::get<0>(border);
        initial_y_min = std::get<1>(border);
        initial_x_max = std::get<2>(border);
        initial_y_max = std::get<3>(border);
    }

    std::tuple<int, int, int, int> border(size_t time = 0) {
        std::vector<int> xs;
        std::vector<int> ys;
        for (auto star : stars) {
            auto [x, y] = star.position(time);
            xs.push_back(x);
            ys.push_back(y);
        }
        return {
            *std::min_element(xs.begin(), xs.end()),
            *std::min_element(ys.begin(), ys.end()),
            *std::max_element(xs.begin(), xs.end()),
            *std::max_element(ys.begin(), ys.end())
        };
    }

    void plot(int min_x, int min_y,
              int max_x, int max_y,
              size_t time = 0) const {
        for (int y={min_y}; y <=max_y; y++) {
            for (int x={min_x}; x <= max_x; x++) {
                bool lights_up{false};
                for (auto star : stars) {
                    if (std::get<0>(star.position(time)) == x &&
                        std::get<1>(star.position(time)) == y) {
                        lights_up = true;
                        break;
                    }
                }
                if (lights_up) {
                    std::cout << '#';
                } else {
                    std::cout << '.';
                }
            }
            std::cout << std::endl;
        }
    }

    void plot(size_t time = 0) {
        plot(initial_x_min, initial_y_min, initial_x_max, initial_y_max, time);
    }
};


int align_stars(const std::vector<std::string>& input, bool verbose = false) {
    Starfield starfield(input);
    std::vector<int64_t> areas;
    int64_t min_area = std::numeric_limits<int64_t>::max();
    int time_at_min_area = 0;
    int time;
    for (time = 0; ; time++) {
        auto [x_min, y_min, x_max, y_max] = starfield.border(time);
        auto area = static_cast<int64_t>(x_max - x_min + 1) *
                    static_cast<int64_t>(y_max - y_min + 1);
        if (area < min_area) {
            min_area = area;
            time_at_min_area = time;
        }
        areas.push_back(area);
        if (verbose) {
            std::cout << "Area at time " << time << ": " << area << std::endl;
        }
        if (area > 2 * min_area) {
            break;
        }
    }
    auto [x_min, y_min, x_max, y_max] = starfield.border(time_at_min_area);
    starfield.plot(x_min, y_min, x_max, y_max, time_at_min_area);

    return time_at_min_area;
}


int main() {
  utils::Reader reader(std::filesystem::path("../../data/2018/input_10.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is:" << std::endl;
  auto time = align_stars(input);
  std::cout << "The answer to part two is: " << time << std::endl;

  return 0;
}
