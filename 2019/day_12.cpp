#include <regex> // NOLINT

#include "../utils/input.hpp"

struct Moon {
  int pos_x;
  int pos_y;
  int pos_z;
  int vel_x;
  int vel_y;
  int vel_z;
};

std::vector<Moon> prepare_input(const std::vector<std::string>& input) {
  std::regex re{"^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>"};
  std::smatch matches;
  std::vector<Moon> moons;

  for (auto line : input) {
    std::regex_match(line, matches, re);
    moons.push_back(Moon{
      std::stoi(matches[1].str()),
      std::stoi(matches[2].str()),
      std::stoi(matches[3].str()),
      0,
      0,
      0});
  }

  return moons;
}

void apply_gravity(std::vector<Moon>* moons) {
  std::vector<std::tuple<int, int, int>> updates;

  for (size_t i{0}; i < moons->size(); i++) {
    for (size_t j{0}; j < moons->size(); j++) {
      if (j == 0) {
        updates.push_back({0, 0, 0});
      }
      if (i == j) {
        continue;
      }
      if (moons->operator[](i).pos_x > moons->operator[](j).pos_x) {
        std::get<0>(updates[i])--;
      } else if (moons->operator[](i).pos_x < moons->operator[](j).pos_x) {
        std::get<0>(updates[i])++;
      }
      if (moons->operator[](i).pos_y > moons->operator[](j).pos_y) {
        std::get<1>(updates[i])--;
      } else if (moons->operator[](i).pos_y < moons->operator[](j).pos_y) {
        std::get<1>(updates[i])++;
      }
      if (moons->operator[](i).pos_z > moons->operator[](j).pos_z) {
        std::get<2>(updates[i])--;
      } else if (moons->operator[](i).pos_z < moons->operator[](j).pos_z) {
        std::get<2>(updates[i])++;
      }
    }
  }

  for (size_t i{0}; i < moons->size(); i++) {
    moons->operator[](i).vel_x += std::get<0>(updates[i]);
    moons->operator[](i).vel_y += std::get<1>(updates[i]);
    moons->operator[](i).vel_z += std::get<2>(updates[i]);
  }
}

void apply_velocity(std::vector<Moon>* moons) {
  for (size_t i{0}; i < moons->size(); i++) {
    moons->operator[](i).pos_x += moons->operator[](i).vel_x;
    moons->operator[](i).pos_y += moons->operator[](i).vel_y;
    moons->operator[](i).pos_z += moons->operator[](i).vel_z;
  }
}

void step(std::vector<Moon>* moons) {
  apply_gravity(moons);
  apply_velocity(moons);
}

std::ostream& operator<<(std::ostream& os, const Moon& moon) {
  os << "pos="
     << "<x=" << moon.pos_x
     << ", y=" << moon.pos_y
     << ", z=" << moon.pos_z
     << ", vel="
     << "<x=" << moon.vel_x
     << ", y=" << moon.vel_y
     << ", z=" << moon.vel_z
     << ">";

  return os;
}

void print(const std::vector<Moon>& moons, size_t time_step) {
  std::cout << "After " << time_step << " steps:" << std::endl;
  for (auto moon : moons) {
    std::cout << moon << std::endl;
  }
  std::cout << std::endl;
}

size_t getPotentialEnergy(const Moon& moon) {
  return std::abs(moon.pos_x) +
         std::abs(moon.pos_y) +
         std::abs(moon.pos_z);
}

size_t getKineticEnergy(const Moon& moon) {
  return std::abs(moon.vel_x) +
         std::abs(moon.vel_y) +
         std::abs(moon.vel_z);
}

size_t getEnergy(const Moon& moon) {
  return getPotentialEnergy(moon) * getKineticEnergy(moon);
}

size_t getEnergy(const std::vector<Moon>& moons) {
  size_t total_energy{0};
  for (auto moon : moons) {
    total_energy += getEnergy(moon);
  }

  return total_energy;
}

int part_one(const std::vector<std::string>& input) {
  bool verbose{false};
  size_t time_step{0};
  auto moons = prepare_input(input);
  if (verbose)
    print(moons, time_step);

  for (time_step = 1; time_step <= 1000; time_step++) {
    step(&moons);

    if (verbose)
      print(moons, time_step);
  }

  auto energy = getEnergy(moons);
  return getEnergy(moons);
}

int part_two(const std::vector<std::string>& input) {
  return -97;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_12.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
