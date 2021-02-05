#include <cassert>
#include <cstdint>
#include <regex>
#include <vector>

#include "../utils/input.hpp"

enum gate_type {assignment_op, not_op, and_op, or_op, lshift_op, rshift_op};

std::ostream& operator<<(std::ostream& os, gate_type type) {
  switch (type) {
    case assignment_op: os << "Assignment"; break;
    case not_op: os << "Not"; break;
    case and_op: os << "And"; break;
    case or_op: os << "Or"; break;
    case lshift_op: os << "Lshift"; break;
    case rshift_op: os << "Rshift"; break;
  }

  return os;
}


class Wire {
private:
  std::string name;
  uint16_t value;
  bool set;

public:
  Wire() : name(""), value(0), set(false) {}

  Wire(std::string name, bool allow_conversion = true) : name(name), value(0), set(false) {
    if (allow_conversion && name.size() > 0 && std::all_of(name.begin(), name.end(), ::isdigit)) {
      this->set_value(std::stoi(name));
      this->name = "const_" + name;
    }
  }

  bool is_set() const {
    return set;
  }

  std::string get_name() const {
    return name;
  }

  uint16_t get_value () const {
    return value;
  }

  void set_value(uint16_t value) {
    set = true;
    this->value = value;
  }

  void unset_value() {
    set = false;
    this->value = 0;
  }

};

int part_one(std::string input) {
  return 1;
}

int part_two(std::string input) {
  return 2;
}

std::ostream& operator<< (std::ostream& os, const Wire& wire) {
  os << wire.get_name() << ": ";
  if (wire.is_set()) {
    os << wire.get_value();
  }
  else {
    os << "N/A";
  }
  return os;
}

class Gate {
private:
  std::vector<Wire*> inputs;
  Wire* output;
  const gate_type type;

public:
  Gate(std::vector<Wire*>& inputs, Wire* output, const gate_type type) : inputs(inputs), output(output), type(type) {
    assert(output!=nullptr);
    switch (type) {
      case assignment_op: assert(inputs.size() == 1); break;
      case not_op: assert(inputs.size() == 1); break;
      case and_op: assert(inputs.size() == 2); break;
      case or_op: assert(inputs.size() == 2); break;
      case lshift_op: assert(inputs.size() == 2); break;
      case rshift_op: assert(inputs.size() == 2); break;
    }
  }

  void evaluate() {
    assert(this->is_ready());

    switch(type) {
      case assignment_op: output->set_value(inputs[0]->get_value()); break;
      case not_op: output->set_value(~inputs[0]->get_value()); break;
      case and_op: output->set_value(inputs[0]->get_value() & inputs[1]->get_value()); break;
      case or_op: output->set_value(inputs[0]->get_value() | inputs[1]->get_value()); break;
      case lshift_op: output->set_value(inputs[0]->get_value()<<inputs[1]->get_value()); break;
      case rshift_op: output->set_value(inputs[0]->get_value()>>inputs[1]->get_value()); break;
    }
  }

  bool is_ready() const {
    for (auto input: inputs) {
      if (!input->is_set()) {
        return false;
      }
    }
    return true;
  }

  bool is_evaluated() const {
    return output->is_set();
  }

  bool is_pending() const {
    return is_ready() && !is_evaluated();
  }

  const Wire* get_output() const {
    return output;
  }

  const std::vector<Wire*> get_inputs() const {
    return inputs;
  }

  const gate_type get_type() const {
    return type;
  }

};

std::ostream& operator<< (std::ostream& os, const Gate& gate) {
  os << gate.get_type() << " (";
  for (size_t index{0}; index < gate.get_inputs().size(); index++)
    os << *gate.get_inputs()[index] << ", ";
  os << ") -> " << *gate.get_output();

  return os;
}

class Circuit {
private:
  std::vector<Wire*> wires;
  std::vector<Gate*> gates;
  std::regex re;
  std::smatch matches;

  gate_type get_type(const std::smatch& matches) const {
    if (matches[1] == "NOT")
      return not_op;
    if (matches[3] == "AND")
      return and_op;
    if (matches[3] == "OR")
      return or_op;
    if (matches[3] == "LSHIFT")
      return lshift_op;
    if (matches[3] == "RSHIFT")
      return rshift_op;

    return assignment_op;
  }

  std::vector<Wire*> create_inputs(const std::smatch& matches, const gate_type type) const {
    std::vector<Wire*> inputs;
    switch (type) {
      case assignment_op: {
        inputs.push_back(new Wire(matches[2].str()));
        break;
      }

      case not_op: {
        inputs.push_back(new Wire(matches[2].str()));
        break;
      }

      case and_op: {
        inputs.push_back(new Wire(matches[2].str()));
        inputs.push_back(new Wire(matches[4].str()));
        break;
      }

      case or_op: {
        inputs.push_back(new Wire(matches[2].str()));
        inputs.push_back(new Wire(matches[4].str()));
        break;
      }

      case lshift_op: {
        inputs.push_back(new Wire(matches[2].str()));
        inputs.push_back(new Wire(matches[4].str()));
        break;
      }

      case rshift_op: {
        inputs.push_back(new Wire(matches[2].str()));
        inputs.push_back(new Wire(matches[4].str()));
        break;
      }

      default: break;
    }
    return inputs;
  }

  Wire* create_output(const std::smatch& matches) const {
    return new Wire(matches[5].str());
  }

  Gate* create_gate(std::vector<Wire*>& inputs, Wire* output, const gate_type type) const {
    return new Gate(inputs, output, type);
  }

  Wire* get_wire(std::string name) {
    for (auto wire: wires) {
      if (wire->get_name() == name) {
        return wire;
      }
    }
    return nullptr;
  }

public:
  Circuit() = default;

  ~Circuit() {
    // must delete gates before deleting wires
    for (auto gate: gates)
      delete gate;
    for (auto wire: wires)
      delete wire;
  }
  
  explicit Circuit(const std::vector<std::string>& input) :
    re("^(?:(NOT)\\s)?([\\w\\d]+)\\s?(?:(AND|OR|LSHIFT|RSHIFT)\\s)?(?:([\\w\\d]+)\\s)?-> (\\w+)$") {

    for (auto description: input)
        create_gate(description);
  }

  void create_gate(const std::string& description) {
    std::regex_match(description, matches, re);
    gate_type type = get_type(matches);
    std::vector<Wire*> inputs = create_inputs(matches, type);
    std::vector<Wire*> gate_inputs;
    for (auto wire: inputs) {
      if (get_wire(wire->get_name()) == nullptr) {
        wires.push_back(wire);
        gate_inputs.push_back(wire);
      }
      else {
        gate_inputs.push_back(get_wire(wire->get_name()));
        delete wire;
      }
    }
    Wire* output = create_output(matches);
    if (get_wire(output->get_name()) == nullptr) {
      wires.push_back(output);
    }
    else {
      delete output;
      output = get_wire(output->get_name());
    }
    gates.push_back(create_gate(gate_inputs, output, type));
  }

  std::vector<Gate*> get_gates() const {
    return gates;
  }

  bool step() {
    for (auto gate: gates) {
      if (gate->is_pending()) {
        gate->evaluate();
        return true;
      }
    }
    return false;
  }

  std::pair<bool, int> read_wire(std::string name) {
    for (auto gate: gates) {
      if (gate->get_output()->get_name() == name) {
        if (gate->is_ready()) {
          return std::pair<bool, int>{true, gate->get_output()->get_value()};
        }
        else {
          return std::pair<bool, int>{false, 0};
        }
      }
    }
    return std::pair<bool, int>{false, 0};
  }
};
 
std::ostream& operator<<(std::ostream& os, const Circuit& circuit) {
  for(auto gate: circuit.get_gates())
    os << *gate << "\n";

  return os;
}

int part_one(std::vector<std::string> input) {
  Circuit circuit(input);
  bool running = true;
  while (running)
    running = circuit.step();

  auto result = circuit.read_wire("a");
  assert(std::get<0>(result));

  return std::get<1>(result);
}

int part_two(std::vector<std::string> input) {
  int16_t val_a = part_one(input);
  std::vector<std::string> new_input;
  new_input.push_back(std::to_string(val_a) + " -> b");
  for (auto description: input) {
    new_input.push_back(description);
  }

  Circuit circuit(new_input);
  bool running = true;
  while (running)
    running = circuit.step();

  auto result = circuit.read_wire("a");
  assert(std::get<0>(result));

  return std::get<1>(result);
}


int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_07.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
