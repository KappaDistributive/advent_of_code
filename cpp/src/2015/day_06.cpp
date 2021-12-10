#include "../utils/input.hpp"

class Interpreter {
 public:
  Interpreter() = default;
  virtual ~Interpreter() = default;
  virtual void execute(const std::string& command, std::vector<int>& grid) = 0;
};

class LegacyInterpreter : public Interpreter {
 private:
  std::regex re;
  std::smatch match;

 public:
  LegacyInterpreter()
      : re("^(toggle|turn)(?:\\s(on|off))?\\s(\\d+),(\\d+) through "
           "(\\d+),(\\d+)$") {}
  ~LegacyInterpreter() override = default;

  void execute(const std::string& command, std::vector<int>& grid) override {
    std::regex_match(command, match, re);
    std::pair<int, int> start, end;
    start = std::pair<size_t, size_t>{std::stoi(match[3].str()),
                                      std::stoi(match[4].str())};
    end = std::pair<size_t, size_t>{std::stoi(match[5].str()),
                                    std::stoi(match[6].str())};

    if (match[1] == "toggle") {
      toggle(start, end, grid);
    } else if (match[1] == "turn") {
      if (match[2] == "on") {
        turn_on(start, end, grid);
      } else if (match[2] == "off") {
        turn_off(start, end, grid);
      } else {
        throw std::runtime_error("This should never happen.");
      }
    } else {
      throw std::runtime_error("This should never happen.");
    }
  }

  void toggle(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
              std::vector<int>& grid) {
    // std::cout << "toggle ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        grid[column + 1000 * row] = 1 - grid[column + 1000 * row];
      }
    }
  }

  void turn_on(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
               std::vector<int>& grid) {
    // std::cout << "turn on ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        grid[column + 1000 * row] = 1;
      }
    }
  }

  void turn_off(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
                std::vector<int>& grid) {
    // std::cout << "turn off ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        grid[column + 1000 * row] = 0;
      }
    }
  }
};

class NewInterpreter : public Interpreter {
 private:
  std::regex re;
  std::smatch match;

 public:
  NewInterpreter()
      : re("^(toggle|turn)(?:\\s(on|off))?\\s(\\d+),(\\d+) through "
           "(\\d+),(\\d+)$") {}
  ~NewInterpreter() override = default;

  void execute(const std::string& command, std::vector<int>& grid) override {
    std::regex_match(command, match, re);
    std::pair<int, int> start, end;
    start = std::pair<size_t, size_t>{std::stoi(match[3].str()),
                                      std::stoi(match[4].str())};
    end = std::pair<size_t, size_t>{std::stoi(match[5].str()),
                                    std::stoi(match[6].str())};

    if (match[1] == "toggle") {
      toggle(start, end, grid);
    } else if (match[1] == "turn") {
      if (match[2] == "on") {
        turn_on(start, end, grid);
      } else if (match[2] == "off") {
        turn_off(start, end, grid);
      } else {
        throw std::runtime_error("This should never happen.");
      }
    } else {
      throw std::runtime_error("This should never happen.");
    }
  }

  void toggle(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
              std::vector<int>& grid) {
    // std::cout << "toggle ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        grid[column + 1000 * row] += 2;
      }
    }
  }

  void turn_on(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
               std::vector<int>& grid) {
    // std::cout << "turn on ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        grid[column + 1000 * row]++;
      }
    }
  }

  void turn_off(std::pair<size_t, size_t> start, std::pair<size_t, size_t> end,
                std::vector<int>& grid) {
    // std::cout << "turn off ("
    //           << std::get<0>(start) << ", " << std::get<1>(start) << ") -- ("
    //           << std::get<0>(end) << ", " << std::get<1>(end) << ")" <<
    //           std::endl;
    for (size_t row = std::get<1>(start); row <= std::get<1>(end); row++) {
      for (size_t column = std::get<0>(start); column <= std::get<0>(end);
           column++) {
        if (grid[column + 1000 * row] > 0) grid[column + 1000 * row]--;
      }
    }
  }
};

class Lights {
 private:
  std::vector<int> grid;
  Interpreter* interpreter;

 public:
  explicit Lights(Interpreter* interpreter) : interpreter(interpreter) {
    grid.reserve(1000000);
    for (size_t index{0}; index < 1000000; index++) grid.push_back(0);
  }

  void step(const std::string& command) { interpreter->execute(command, grid); }

  int brightness() { return std::accumulate(grid.begin(), grid.end(), 0); }

  ~Lights() = default;
};

int part_one(const std::vector<std::string>& input) {
  Interpreter* interpreter = new LegacyInterpreter();
  Lights lights(interpreter);

  for (auto command : input) lights.step(command);

  delete interpreter;
  return lights.brightness();
}

int part_two(const std::vector<std::string>& input) {
  Interpreter* interpreter = new NewInterpreter();
  Lights lights(interpreter);

  for (auto command : input) lights.step(command);

  delete interpreter;
  return lights.brightness();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2015/input_06.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

