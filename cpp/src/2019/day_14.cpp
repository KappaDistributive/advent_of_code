#include <map>
#include <regex>  // NOLINT
#include <set>

#include "../utils/input.hpp"

struct Resource {
  std::string name;
  size_t quantity;

  bool operator<(const Resource& other) const {
    return this->quantity < other.quantity||
           (this->quantity == other.quantity && this->name < other.name);
  }

  bool operator==(const Resource& other) const {
    return this->name == other.name &&
           this->quantity == other.quantity;
  }

  bool operator!=(const Resource& other) const {
    return !(*this == other);
  }

  friend std::ostream& operator<<(std::ostream& os, const Resource& res) {
    os << res.quantity << " " << res.name;

    return os;
  }
};

std::map<Resource, std::set<Resource>>
prepare_input(const std::vector<std::string>& input) {
  std::regex re{"(\\d+) (\\w+)"};
  std::smatch matches;
  std::map<Resource, std::set<Resource>> reactions;
  for (auto line : input) {
    utils::replace_all_substrings(&line, " =>", ",");
    utils::replace_all_substrings(&line, ", ", ",");
    auto splits = utils::split_string(line, ',');
    auto product = splits.back();
    std::set<std::string> educts;
    for (size_t index{0}; index + 1 < splits.size(); index++) {
      educts.insert(splits[index]);
    }
    std::regex_match(product, matches, re);
    Resource product_res{matches[2].str(), std::stoul(matches[1].str())};
    std::set<Resource> educts_res;
    for (auto educt : educts)  {
      std::regex_match(educt, matches, re);
      educts_res.insert(
        Resource{matches[2].str(), std::stoul(matches[1].str())});
    }
    reactions.insert_or_assign(product_res, educts_res);
  }

  return reactions;
}

std::ostream& operator<<(std::ostream& os,
                    const std::pair<Resource, std::set<Resource>>& reaction) {
  for (auto it = reaction.second.begin(); it != reaction.second.end(); it++) {
    os << *it;
    if (std::next(it) != reaction.second.end()) {
      os << ", ";
    }
  }
  os << " => " << reaction.first;

  return os;
}

bool done(const std::map<std::string, size_t>& reagents) {
  for (auto [name, quantity] : reagents) {
    if (name != "ORE" && quantity > 0) {
      return false;
    }
  }

  return true;
}

std::pair<Resource, std::set<Resource>>
find(const std::string& name,
     const std::map<Resource, std::set<Resource>>& reactions) {

  for (auto [product, educts] : reactions) {
    if (product.name == name) {
      return {product, educts};
    }
  }

  throw std::out_of_range("There is no reaction with name: " + name);
}

size_t
createFuel(const std::map<Resource, std::set<Resource>>& reactions,
           size_t fuel_amount) {
  std::map<std::string, size_t> reagents;
  std::map<std::string, size_t> stash;
  reagents.insert_or_assign("FUEL", fuel_amount);

  while (!done(reagents)) {
    std::map<std::string, size_t> new_reagents;

    for (auto [name, quantity] : reagents) {
      if (name != "ORE") {
        auto [product, educts] = find(name, reactions);
        stash.emplace(name, 0);
        if (quantity <= stash.at(name)) {  // sufficient product is in stash
          stash.at(name) -= quantity;
        } else {  // need to create more product
          quantity -= stash.at(name);
          size_t multiplier = quantity / product.quantity +
            (quantity % product.quantity != 0);
          stash.at(name) = multiplier * product.quantity - quantity;
          for (auto educt : educts) {
            new_reagents.emplace(educt.name, 0);
            new_reagents.at(educt.name) += multiplier * educt.quantity;
          }
        }
      } else {
        new_reagents.emplace(name, 0);
        new_reagents.at(name) += quantity;
      }
    }

    reagents = new_reagents;
  }

  return reagents.at("ORE");
}

auto part_one(const std::vector<std::string>& input) {
  auto reactions = prepare_input(input);

  return createFuel(reactions, 1);
}

auto part_two(const std::vector<std::string>& input) {
  bool verbose{true};
  const size_t ore_stock{1000000000000};
  auto reactions = prepare_input(input);
  size_t fuel_amount{1};

  while (createFuel(reactions, fuel_amount) <= ore_stock) {
    if (verbose) {
      std::cout << std::format("Answer in [{}, ∞)", fuel_amount) << std::endl;
    }
    fuel_amount *= 2;
  }
  size_t upper_limit{fuel_amount};
  size_t lower_limit{fuel_amount/2};
  fuel_amount = (upper_limit + lower_limit) / 2;
  while (upper_limit > lower_limit + 1) {
    if (createFuel(reactions, fuel_amount) > ore_stock) {
      upper_limit = fuel_amount;
    } else {
      lower_limit = fuel_amount;
    }
    fuel_amount = (upper_limit + lower_limit) / 2;
    if (verbose) {
      std::cout << "Answer in [" << lower_limit
                << ", " << upper_limit << ")" << std::endl;
    }
  }
  return lower_limit;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2019/input_14.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

