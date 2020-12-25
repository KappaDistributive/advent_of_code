#include <array>
#include <map>

#include "../utils/input.hpp"

class Cup
{
private:
  int _value;
  std::shared_ptr<Cup> _next;
  std::shared_ptr<Cup> _prev;

public:
  Cup () = default;
  explicit Cup (int value, std::shared_ptr<Cup> next=nullptr, std::shared_ptr<Cup> prev=nullptr)
    : _value(value), _next(next), _prev(prev)
  {
  }

  int& value()
  {
    return _value;
  }

  std::shared_ptr<Cup>& next()
  {
    return _next;
  }

  std::shared_ptr<Cup>& prev()
  {
    return _prev;
  }

  friend std::ostream& operator<< (std::ostream& os, const Cup& cup)
  {
    os << cup._value;
    return os;
  }
};

class Ring
{
private:
  std::shared_ptr<Cup> current;
  bool verbose;
  const int min_value, max_value;
  std::array<std::shared_ptr<Cup>, 3> buffer;
  std::map<int, std::shared_ptr<Cup>> mapper;

  std::shared_ptr<Cup> get_destination() const
  {
    std::shared_ptr<Cup> destination;
    int value = current->value() > min_value ? current->value() - 1 : max_value;
    auto cup = current;
    bool found_destination = false;

    while (!found_destination)
    {
      cup = mapper.at(value);
      if (std::find(buffer.begin(), buffer.end(), cup) == buffer.end())
      {
        destination = cup;
        found_destination = true;
      }
      else if (value > min_value)
      {
        value--;
      }
      else
      {
        value = max_value;
      }
    }

    return destination;
  }

public:
  Ring(const std::vector<int>& input, int min_value, int max_value, bool verbose = false)
    : verbose(verbose), min_value(min_value), max_value(max_value)
  {
    current = std::make_shared<Cup>(input[0]);
    auto target = current;
    for (size_t index{1}; index < input.size(); index++)
    {
      auto new_target = std::make_shared<Cup>(input[index]);
      target->next() = new_target;
      new_target->prev() = target;
      target = new_target;
    }
    target->next() = current;
    current->prev() = target;

    auto cup = current;
    mapper.insert(std::make_pair(current->value(), current));
    while (cup->next() != current)
    {
      cup = cup->next();
      mapper.insert(std::make_pair(cup->value(), cup));
    }
  }

  void step()
  {
    auto cup = this->current;
    for (size_t index{0}; index < 3; index++)
    {
      cup = cup->next();
      buffer[index] = cup;
    }
    auto destination = get_destination();
    if (verbose)
    {
      std::cout << "cups: " << *this << std::endl;
      std::cout << "pick up: ";
      for (auto c: buffer)
      {
        std::cout << *c << " ";
      }
      std::cout << std::endl;
      std::cout << "destination: " << *destination << std::endl;
    }

    assert (buffer.size() == 3);
    auto before_buffer = buffer[0]->prev();
    auto after_buffer = buffer[2]->next();
    before_buffer->next() = after_buffer;
    after_buffer->prev() = before_buffer;
    destination->next()->prev() = buffer[2];
    buffer[2]->next() = destination->next();
    destination->next() = buffer[0];
    buffer[0]->prev() = destination;
    current = current->next();

  }

  std::shared_ptr<Cup> get_by_value(int value)
  {
    auto cup = current;
    std::shared_ptr<Cup> result;
    do
    {
      if (cup->value() == value)
      {
        result = cup;
        break;
      }
      cup = cup->next();
    }
    while (cup != current);
    return result;
  }

  friend std::ostream& operator<< (std::ostream& os, const Ring& ring)
  {
    auto cup = ring.current;
    os << "(" << *cup << ") ";
    while (cup->next() != nullptr && cup->next() != ring.current)
    {
      cup = cup->next();
      os << *cup << " ";
    }
    return os;
  }

};

std::vector<int> prepare_input(const std::string& raw_input)
{
  std::vector<int> result;
  for (auto c: raw_input)
  {
    assert (c - '0' >= 0 && c - '0' <= 9);
    result.push_back(c - '0');
  }
  return result;
}

int part_one(const std::string& input)
{
  bool verbose = false;
  auto prepared_input = prepare_input(input);
  int min_value{prepared_input[0]};
  int max_value{prepared_input[0]};
  for (auto value: prepared_input)
  {
    if (value < min_value)
    {
      min_value = value;
    }
    if (value > max_value)
    {
      max_value = value;
    }
  }
  Ring ring(prepare_input(input), min_value, max_value, verbose);
  if (verbose)
  {
    std::cout << ring << std::endl;
  }
  for (size_t round{0}; round < 100; round++)
  {
    ring.step();
    if (verbose)
    {
      std::cout << ring << std::endl;
    }
  }

  auto cup_1 = ring.get_by_value(1);
  auto cup = cup_1->next();
  std::string result;
  do
  {
    result += std::to_string(cup->value());
    cup = cup->next();
  }
  while (cup != cup_1);

  return std::stoi(result);
}

long part_two(const std::string& input)
{
  bool verbose = false;
  auto prepared_input = prepare_input(input);
  int max_value{0};
  for (auto value: prepared_input)
  {
    if (value > max_value)
    {
      max_value = value;
    }
  }

  for (int value{max_value+1}; value <= 1000000; value++)
  {
    prepared_input.push_back(value);
  }
  Ring ring(prepared_input, 1, 1000000, verbose);
  if (verbose)
  {
    std::cout << ring << std::endl;
  }
  for (size_t round{0}; round < 10000000; round++)
  {
    ring.step();
    if (verbose)
    {
      std::cout << ring << std::endl;
    }
  }

  auto cup = ring.get_by_value(1)->next();
  long result{static_cast<long>(cup->value())};
  result *= static_cast<long>(cup->next()->value());
  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_23.txt"));
  const std::string input = reader.get_lines()[0];
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
