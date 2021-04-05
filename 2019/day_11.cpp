#include <cassert>
#include <map>
#include <set>
#include <stack>

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

std::vector<int64_t> prepare_input(const std::vector<std::string>& input) {
  std::vector<int64_t> intcodes;
  std::transform(
    input.begin(),
    input.end(),
    std::back_inserter(intcodes),
      [](std::string code) -> int64_t {
        return std::strtoll(code.c_str(), NULL, 10);
      });
  return intcodes;
}

struct Instruction {
  int64_t opcode;
  std::vector<int64_t> parameters;
};

class CPU {
 private:
  std::map<size_t, int64_t> memory;
  std::stack<int64_t> input_tape;
  size_t instruction_pointer;
  size_t relative_base;
  int64_t output;
  bool verbose;

 public:
  explicit CPU(const std::vector<int64_t>& intcodes,
               const bool& verbose = true)
      : instruction_pointer(0),
        relative_base(0),
        output(0),
        verbose(verbose) {
    for (size_t index{0}; index < intcodes.size(); index++) {
      this->get_memory(index) = static_cast<int64_t>(intcodes[index]);
    }
  }

  int64_t get_mode(const Instruction& instruction, const size_t& index) {
    return static_cast<int64_t>(instruction.opcode /
    utils::pow(
      static_cast<size_t>(10),
      static_cast<size_t>(2 + index))) % 10;
  }

  int64_t& get_parameter(Instruction instruction, const size_t& index) {
    auto mode = get_mode(instruction, index);
    switch (mode) {
    case 0:   // position mode
      return this->get_memory(instruction.parameters[index]);
      break;
    case 1:  // immediate mode
      return instruction.parameters[index];
      break;
    case 2:  // relative mode
      return this->get_memory(relative_base +
        instruction.parameters[index]);
      break;
    default:
      throw std::runtime_error(
        "Invalid parameter mode " + std::to_string(mode));
      break;
    }
  }

std::pair<bool, std::optional<int64_t>>
execute(const Instruction& instruction) {
  bool halting{false};
  std::optional<int64_t> output{std::nullopt};
  std::string input{""};
  bool update_instruction_pointer{true};

  switch (instruction.opcode % 100) {
    case 1:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) +
        get_parameter(instruction, 1);
      break;
    case 2:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) *
        get_parameter(instruction, 1);
      break;
    case 3:
      assert(instruction.parameters.size() == 1);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      assert(get_mode(instruction, 0) != 1);
      if (this->input_tape.size() == 0) {
        std::cout << "Input required:" << std::endl;
        std::cin >> input;
        this->get_parameter(instruction, 0) =
          std::strtoll(input.c_str(), NULL, 10);
      } else {
        this->get_parameter(instruction, 0) = this->input_tape.top();
        this->input_tape.pop();
      }
      break;
    case 4:
      assert(instruction.parameters.size() == 1);
      output = get_parameter(instruction, 0);
      if (verbose) {
        std::cout << "Output: " << output.value() << std::endl;
      }
      break;
    case 5:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) != 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 6:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) == 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 7:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      assert(get_mode(instruction, 2) != 1);
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) <
        get_parameter(instruction, 1));
      break;
    case 8:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) ==
        get_parameter(instruction, 1));
      break;
    case 9:
      assert(instruction.parameters.size() == 1);
      relative_base += get_parameter(instruction, 0);
      break;
    case 99:
      halting = true;
      break;
    default:
      throw std::runtime_error(
        "Encountered invalid opcode: " +
        std::to_string(instruction.opcode));
      break;
  }

  if (update_instruction_pointer) {
    this->instruction_pointer += 1 + instruction.parameters.size();
  }

  return {halting, output};
}

  std::pair<bool, std::optional<int64_t>> execute() {
    int64_t opcode = this->get_memory(this->instruction_pointer);
    Instruction instruction;
    instruction.opcode = opcode;
    std::vector<int64_t> parameters;
    switch (opcode % 100) {
      case 1:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 2:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 3:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 4:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 5:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 6:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 7:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 8:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 9:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      default:
        parameters = {};
        break;
    }
    instruction.parameters = parameters;
    return execute(instruction);
  }

  void pushInput(int64_t input) {
    this->input_tape.push(input);
  }

  std::pair<bool, std::optional<int64_t>> run(bool pause_on_output = false) {
    bool halting{false};
    std::optional<int64_t> output{std::nullopt};

    while (!halting) {
      auto update = this->execute();
      halting = std::get<0>(update);
      output = std::get<1>(update);
      if (halting || (pause_on_output && output.has_value())) {
        break;
      }
    }

    return {halting, output};
  }

  int64_t& get_memory(const size_t& index) {
    if (memory.count(index) == 0) {
      this->memory.insert({index, 0});
    }
    return this->memory.at(index);
  }

  int64_t get_output() const {
    return output;
  }
};

class Robot {
 private:
  enum class Direction {
    north,
    east,
    south,
    west
  };

  struct Point {
    int x;
    int y;

    bool operator<(const Point& rhs) const {
      return this->x < rhs.x || (this->x == rhs.x && this->y < rhs.y);
    }
  };

  Direction m_direction;
  Point m_position;
  CPU m_cpu;
  std::set<Point> m_visited;
  std::map<Point, bool> m_panel;

  std::pair<Point, Point> getFrame() const {
    Point top_left{0, 0}, bottom_right{0, 0};
    for (auto [position, _] : this->m_panel) {
      top_left.x = std::min(top_left.x, position.x);
      top_left.y = std::min(top_left.y, position.y);
      bottom_right.x = std::max(bottom_right.x, position.x);
      bottom_right.y = std::max(bottom_right.y, position.y);
    }

    top_left.x = std::min(top_left.x, this->m_position.x);
    top_left.y = std::min(top_left.y, this->m_position.y);
    bottom_right.x = std::max(bottom_right.x, this->m_position.x);
    bottom_right.y = std::max(bottom_right.y, this->m_position.y);

    return {top_left, bottom_right};
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const Direction& direction) {
    switch (direction) {
      case Direction::north: os << "^"; break;
      case Direction::east: os << ">"; break;
      case Direction::south: os << "v"; break;
      case Direction::west: os << "<"; break;
    }

    return os;
  }

 public:
  explicit Robot(const std::vector<int64_t>& intcodes,
                 bool verbose = false)
    : m_direction(Direction::north),
      m_position(Point{0, 0}),
      m_cpu(CPU(intcodes, verbose)) {
  }

  void turn(bool panel) {
    switch (this->m_direction) {
      case Direction::north:
        this->m_direction = panel ? Direction::east : Direction::west;
        break;
      case Direction::east:
        this->m_direction = panel ? Direction::south : Direction::north;
        break;
      case Direction::south:
        this->m_direction = panel ? Direction::west: Direction::east;
        break;
      case Direction::west:
      this->m_direction = panel ? Direction::north: Direction::south;
      break;
    }
  }

  void move() {
    switch (this->m_direction) {
      case Direction::north: this->m_position.y--; break;
      case Direction::east: this->m_position.x++; break;
      case Direction::south: this->m_position.y++; break;
      case Direction::west: this->m_position.x--; break;
    }
  }

  bool step() {
    bool halting{false};
    int64_t color{-1};
    int64_t turn{-1};

    m_cpu.pushInput(this->operator[](this->m_position));
    std::optional<int64_t> output;
    auto update = m_cpu.run(true);
    halting = std::get<0>(update);
    output = std::get<1>(update);
    if (!halting) {
      color = output.value();
      update = m_cpu.run(true);
      halting = std::get<0>(update);
      output = std::get<1>(update);
      if (!halting) {
        turn = output.value();
        this->m_panel.insert_or_assign(this->m_position, color);
        this->m_visited.insert(this->m_position);
        this->turn(turn);
        this->move();
      }
    }
    return halting;
  }

  bool operator[](const Point& position) const {
    return this->m_panel.count(position) > 0 &&
           this->m_panel.at(position);
  }

  void setPanel(int x, int y, bool white) {
    this->m_panel.insert_or_assign(Point{x, y}, white);
  }

  std::set<Point> getVisitedLocations() const {
    return this->m_visited;
  }

  friend std::ostream& operator<<(std::ostream& os, const Robot& robot) {
    auto [top_left, bottom_right] = robot.getFrame();

    for (int y{top_left.y}; y <= bottom_right.y; y++) {
      for (int x{top_left.x}; x <= bottom_right.x; x++) {
        if (x == robot.m_position.x && y == robot.m_position.y) {
          std::cout << robot.m_direction;
        } else {
          std::cout << (robot[{x, y}] ? '#' : '.');
        }
      }
      os << std::endl;
    }

    return os;
  }
};

/*
* - All of the panels are currently black.
* - provide 0 if the robot is over a black panel or 1 if the robot is over a white panel
*   - Then, the program will output two values:
*   - First: 0 means to paint the panel black, and 1 means to paint the panel white.
*   - Second: Second, it will output a value indicating the direction the robot should turn: 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees.
* - After the robot turns, it should always move forward exactly one panel. The robot starts facing up.
*/

size_t part_one(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  bool halting{false};
  bool verbose{false};
  Robot robot(intcodes, verbose);

  if (verbose)
    std::cout << robot << std::endl;
  while (!halting) {
    halting = robot.step();
    if (verbose)
      std::cout << robot << std::endl;
  }
  return robot.getVisitedLocations().size();
}

void part_two(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  bool halting{false};
  bool verbose{false};
  Robot robot(intcodes, verbose);
  robot.setPanel(0, 0, true);

  if (verbose)
    std::cout << robot << std::endl;
  while (!halting) {
    halting = robot.step();
    if (verbose)
      std::cout << robot << std::endl;
  }

  std::cout << robot << std::endl;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_11.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  std::cout << "The answer to part two is: " << std::endl;
  part_two(input);
  return 0;
}
