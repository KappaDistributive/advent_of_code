#include <array>
#include <limits>
#include <sstream>

#include "../utils/input.hpp"

class Grid {
 private:
    std::array<int, 300*300> power_level;
    const int serial_number;

    int calc_power_level(size_t x, size_t y) {
        int rack_id = x + 10;
        int power = rack_id * y;
        power += serial_number;
        power *= rack_id;
        power = (power % 1000) / 100;
        power -= 5;
        return power;
    }

 public:
    explicit Grid(const int& serial_number) 
        : serial_number(serial_number) { 
        for (size_t y{1}; y <= 300; y++) {
            for (size_t x{1}; x <= 300; x++) {
                power(x, y) = calc_power_level(x, y);
            }
        }
    }

    int& power(size_t x, size_t y) {
        assert (1 <= x && x <= 300);
        assert (1 <= y && y <= 300);
        return power_level[(y-1) * 300 + (x-1)];
    }

    int total_power(size_t x, size_t y, size_t width_x = 3, size_t width_y = 3) {
        assert (1 <= x && 1 <= width_x && x + width_x -1 <= 300);
        assert (1 <= y && 1 <= width_y && y + width_y -1 <= 300);
        int power{0};
        for (size_t offset_y{0}; offset_y < width_y; offset_y++)
        {
            for (size_t offset_x{0}; offset_x < width_x; offset_x++) {
                power += this->power(x + offset_x, y + offset_y);
            }
        }
        return power;
    }

    std::ostream& plot(std::ostream& os, size_t min_x = 1, size_t min_y = 1, size_t max_x = 300, size_t max_y = 300) {
        assert (1 <= min_x && min_x <= max_x && max_x <= 300);
        assert (1 <= min_y && min_y <= max_y && max_y <= 300);
        for (size_t y{min_y}; y <= max_y; y++) {
            for (size_t x{min_x}; x <= max_x; x++) {
                int power = this->power(x, y);
                if (power >= 0) {
                    os << " ";
                }
                os << power;
                if (x < 300) {
                    os << " ";
                }
            }
            os << std::endl;
        }
        return os;
    }

    friend std::ostream& operator<<(std::ostream& os, Grid& grid) {
        return grid.plot(os);
    }
};

std::string part_one(const int& input) {
    Grid grid(input);
    size_t coord_x{0}, coord_y{0};
    int max_power{std::numeric_limits<int>::min()};

    for (size_t y{1}; y + 2 <= 300; y++) {
        for (size_t x{1}; x + 2 <= 300; x++) {
            auto power = grid.total_power(x, y);
            if (power > max_power) {
                max_power = power;
                coord_x = x;
                coord_y = y;
            }
        }
    }
    std::stringstream ss;
    ss << coord_x << "," << coord_y;
    return ss.str();
}


size_t part_two(const int& input) {
    return 5;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_11.txt"));
  auto input = std::stoi(reader.get_lines()[0]);

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
