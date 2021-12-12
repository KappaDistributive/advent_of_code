#include <regex>  // NOLINT

#include "../utils/input.hpp"

static const std::regex particle_regex{
    "p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, "
    "a=<(-?\\d+),(-?\\d+),(-?\\d+)>"};

struct Coordinate {
  int64_t x;
  int64_t y;
  int64_t z;

  Coordinate& operator+=(const Coordinate& other) {
    this->x += other.x;
    this->y += other.y;
    this->z += other.z;

    return *this;
  }

  bool operator==(const Coordinate& other) {
    return (this->x == other.x && this->y == other.y && this->z == other.z);
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const Coordinate& coordinate) {
    os << '<' << coordinate.x << ',' << coordinate.y << ',' << coordinate.z
       << '>';
    return os;
  }
};

auto manhatten_distance(Coordinate& lhs, Coordinate& rhs) {
  return static_cast<size_t>(std::abs(lhs.x - rhs.x)) +
         static_cast<size_t>(std::abs(lhs.y - rhs.y)) +
         static_cast<size_t>(std::abs(lhs.z - rhs.z));
}

auto manhatten_distance(Coordinate& lhs) {
  Coordinate origin{0, 0, 0};
  return manhatten_distance(lhs, origin);
}

struct Particle {
  Coordinate position;
  Coordinate velocity;
  Coordinate acceleration;

  explicit Particle(const std::string& description) {
    std::smatch matches;
    std::regex_match(description, matches, particle_regex);
    assertm(matches.size() == 10, "Failed to parse description.");

    position.x = std::stoi(matches[1].str());
    position.y = std::stoi(matches[2].str());
    position.z = std::stoi(matches[3].str());

    velocity.x = std::stoi(matches[4].str());
    velocity.y = std::stoi(matches[5].str());
    velocity.z = std::stoi(matches[6].str());

    acceleration.x = std::stoi(matches[7].str());
    acceleration.y = std::stoi(matches[8].str());
    acceleration.z = std::stoi(matches[9].str());
  }

  void update() {
    this->velocity += this->acceleration;
    this->position += this->velocity;
  }

  friend std::ostream& operator<<(std::ostream& os, const Particle& particle) {
    // p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>
    os << "p=" << particle.position << ", v=" << particle.velocity
       << ", a=" << particle.acceleration;
    return os;
  }
};

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<Particle> particles;
  for (auto line : input) {
    particles.push_back(Particle(line));
  }

  return particles;
}

auto part_one(const std::vector<std::string>& input) {
  auto particles = prepare_input(input);
  for (size_t step{0}; step < 1000; ++step) {
    for (size_t index{0}; index < particles.size(); ++index) {
      particles[index].update();
    }
  }

  size_t min_distance{manhatten_distance(particles[0].position)};
  size_t answer{0};
  for (size_t index{1}; index < particles.size(); ++index) {
    auto distance = manhatten_distance(particles[index].position);
    if (distance < min_distance) {
      min_distance = distance;
      answer = index;
    }
  }

  return answer;
}

auto part_two(const std::vector<std::string>& input) {
  auto particles = prepare_input(input);
  for (size_t step{0}; step < 1000; ++step) {
    for (size_t index{0}; index < particles.size(); ++index) {
      particles[index].update();
    }
    std::set<size_t> collissions;
    for (size_t lhs{0}; lhs < particles.size(); ++lhs) {
      for (size_t rhs{lhs + 1}; rhs < particles.size(); ++rhs) {
        if (particles[lhs].position.operator==(particles[rhs].position)) {
          collissions.insert(lhs);
          collissions.insert(rhs);
        }
      }
    }

    std::vector<Particle> remaining_particles;
    for (size_t index{0}; index < particles.size(); ++index) {
      if (collissions.count(index) == 0) {
        remaining_particles.push_back(particles[index]);
      }
    }
    particles = remaining_particles;
    // std::cout << particles.size() << std::endl;
  }

  return particles.size();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_20.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
}

