#include <algorithm>
#include <cassert>
#include <regex>  // NOLINT

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

#ifdef DEBUG
#define DBGVAR(os, var)                                                    \
  (os) << "DBG: " << __FILE__ << "(" << __LINE__ << ") " << #var << " = [" \
       << (var) << "]" << std::endl
#else
#define DBGVAR(os, var) \
  do {                  \
  } while (0)
#endif

static const std::regex bot_regex{
    "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(-?\\d+)"};

using Position = utils::geometry::Point<int, 3>;

struct NanoBot {
  Position position;
  size_t radius;

  explicit NanoBot(const std::string& description) {
    std::smatch matches;
    std::regex_match(description, matches, bot_regex);
    assert(matches.size() == 5);

    this->position = Position{std::vector<int>{
        std::stoi(matches[1].str()),
        std::stoi(matches[2].str()),
        std::stoi(matches[3].str()),
    }};
    this->radius = std::stoi(matches[4].str());
  }

  bool is_in_range(NanoBot other) const noexcept {
    return utils::geometry::manhatten_distance(this->position,
                                               other.position) <= this->radius;
  }
};

std::ostream& operator<<(std::ostream& os, NanoBot bot) {
  os << bot.position << ", r=" << bot.radius;
  return os;
}

std::vector<NanoBot> parse_input(const std::vector<std::string>& input) {
  std::vector<NanoBot> bots;
  for (auto line : input) {
    bots.push_back(NanoBot(line));
  }

  return bots;
}

auto part_one(const std::vector<std::string>& input) {
  auto bots = parse_input(input);
  std::sort(bots.begin(), bots.end(),
            [](NanoBot lhs, NanoBot rhs) { return lhs.radius > rhs.radius; });
  assert(bots[0].radius > bots[1].radius);

  size_t bots_in_range{0};
  for (auto bot : bots) {
    bots_in_range += bots[0].is_in_range(bot);
  }

  return bots_in_range;
}

// auto part_two(const std::vector<std::string>& input) { return 8; }

int main() {
  // utils::Reader
  // reader(std::filesystem::path("../2018/data/input_23_mock.txt"));
  utils::Reader reader(std::filesystem::path("../2018/data/input_23.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

