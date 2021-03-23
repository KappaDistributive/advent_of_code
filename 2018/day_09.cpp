#include <regex>

#include "../utils/input.hpp"

#define RESET       "\033[0m"
#define BOLDWHITE   "\033[1m\033[37m"


class Game {
 private:
    const size_t num_players, last_marble;
    size_t current_player{0};
    size_t current_marble{0};
    size_t marble_index{0};
    std::vector<size_t> marbles = {0};
    std::vector<size_t> scores;

 public:
    Game(size_t num_players, size_t last_marble) 
        : num_players(num_players), last_marble(last_marble) {
            for (size_t player_index{0}; player_index < num_players; player_index++) {
                scores.push_back(0);
            }
        }

    size_t& score(size_t player) {
        return scores[player-1];
    }

    bool play() {
        current_player++;
        if (current_player > num_players) {
            current_player -= num_players;
        }
        current_marble++;

        if (current_marble > last_marble) {
            return false;
        }

        if (current_marble % 23 == 0) {
            score(current_player) += current_marble;
            marble_index = (marble_index + 7 * marbles.size() - 7) % marbles.size();
            score(current_player) += marbles[marble_index];
            std::vector<size_t> new_marbles = std::vector<size_t>(marbles.begin(), marbles.begin() + marble_index);
            for (size_t index{marble_index + 1}; index < marbles.size(); index++) {
                new_marbles.push_back(marbles[index]);
            }
            marbles = new_marbles;
        } else {
            size_t insert_index = ((marble_index + 1) % (marbles.size())) + 1;
            std::vector<size_t> new_marbles;
            for (size_t index{0}; index < insert_index; index++) {
                new_marbles.push_back(marbles[index]);
            }
            new_marbles.push_back(current_marble);
            for (size_t index{insert_index}; index < marbles.size(); index++) {
                new_marbles.push_back(marbles[index]);
            }
            marbles = new_marbles;
            marble_index = insert_index;
        }

        return true;
    }

    size_t highest_score() const {
        return *std::max_element(scores.begin(), scores.end());
    }

    friend std::ostream& operator<<(std::ostream& os, const Game& game) {
        os << "[";
        if (game.current_player > 0) {
            os << game.current_player;
        } else {
            os << "-";
        }
        os << "] ";
        for (size_t index{0}; index < game.marbles.size(); index++) {
            if (index == game.marble_index) {
                os << BOLDWHITE << "(" << game.marbles[index] << ")" << RESET;
            } else {
                os << game.marbles[index];
            }
            if (index + 1 < game.marbles.size()) {
                os << " ";
            }
        }

        return os;
    }
};

int part_one(size_t num_players, size_t last_marble, bool verbose = false) {
    Game game(num_players, last_marble);
    if (verbose) {
        std::cout << game << std::endl;
    }

    while (game.play()) {
        if (verbose) {
            std::cout << game << std::endl;
        }
    }

    return game.highest_score();
}


int part_two(size_t num_players, size_t last_marble) {
    return 98;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_09.txt"));
  auto input = reader.get_lines()[0];
  std::regex re{"^(\\d+) players; last marble is worth (\\d+) points$"};
  std::smatch matches;
  std::regex_match(input, matches, re);
  assert (matches.size() == 3);
  size_t num_players = std::stol(matches[1].str());
  size_t last_marble = std::stol(matches[2].str());

  auto answer_one =  part_one(num_players, last_marble);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(num_players, last_marble);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
