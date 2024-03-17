#include <regex>  // NOLINT

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 3>;
using Cuboid = utils::geometry::RasterCuboid<int, 3>;

class Grid {
 private:
  std::vector<std::pair<bool, Cuboid>> m_reboot_steps;

  void add_step(bool on, Cuboid cuboid,
                std::vector<std::pair<bool, Cuboid>> *steps) const {
    for (const auto &step : *steps) {
      auto intersection = cuboid.intersect(step.second);
      if (intersection.has_value()) {
        if (on) {
          if (step.first) {
            steps->push_back(std::make_pair(false, intersection.value()));
          } else {
            steps->push_back(std::make_pair(true, intersection.value()));
          }
        } else {
          if (step.first) {
            steps->push_back(std::make_pair(false, intersection.value()));
          } else {
            steps->push_back(std::make_pair(true, intersection.value()));
          }
        }
      }
    }

    if (on) {
      steps->push_back(std::make_pair(on, cuboid));
    }
  }

 public:
  explicit Grid(const std::vector<std::string> &input,
                std::optional<Cuboid> area = std::nullopt) {
    std::regex instruction_regex{
        "(on|off) "
        "x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?"
        "\\d+)"};
    std::smatch matches;

    for (auto line : input) {
      std::regex_match(line, matches, instruction_regex);
      assertm(matches.size() == 8,
              std::format("Failed to parse reboot step `{}`\n", line).c_str());
      bool on = matches[1] == "on";
      Cuboid cuboid{{std::make_pair(std::stoi(matches[2].str()),
                                    std::stoi(matches[3].str())),
                     std::make_pair(std::stoi(matches[4].str()),
                                    std::stoi(matches[5].str())),
                     std::make_pair(std::stoi(matches[6].str()),
                                    std::stoi(matches[7].str()))}};

      if (area.has_value()) {
        auto intersection = cuboid.intersect(area.value());
        if (intersection.has_value()) {
          this->m_reboot_steps.push_back(
              std::make_pair(on, intersection.value()));
        }
      } else {
        this->m_reboot_steps.push_back(std::make_pair(on, cuboid));
      }
    }
  }

  auto volume() const {
    std::vector<std::pair<bool, Cuboid>> refined_reboot_steps;
    for (auto [on, cuboid] : this->m_reboot_steps) {
      this->add_step(on, cuboid, &refined_reboot_steps);
    }

    int64_t result{0};
    for (auto [on, cuboid] : refined_reboot_steps) {
      auto intervals = cuboid.intervals();
      auto vol = std::accumulate(
          intervals.begin(), intervals.end(), static_cast<int64_t>(1),
          [](const auto &lhs, const auto &rhs) {
            return lhs * static_cast<int64_t>(rhs.second - rhs.first + 1);
          });
      result += on ? vol : -vol;
    }

    return result;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Grid grid{input, Cuboid{{std::make_pair(-50, 50), std::make_pair(-50, 50),
                           std::make_pair(-50, 50)}}};
  return grid.volume();
}

auto part_two(const std::vector<std::string> &input) {
  Grid grid{input};
  return grid.volume();
}

int main(int argc, char *argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      std::format("../../data/2021/input_22{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("the answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("the answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
