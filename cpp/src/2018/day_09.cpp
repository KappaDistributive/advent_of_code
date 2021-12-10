#include <cassert>
#include <list>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

#define RESET       "\033[0m"
#define BOLDWHITE   "\033[1m\033[37m"


class Game {
 private:
    const size_t num_players, last_marble;
    size_t current_player{0};
    size_t current_marble{0};
    std::list<size_t> marbles = {0};
    std::list<size_t>::iterator marble_it = marbles.begin();
    std::vector<size_t> scores;

 public:
    Game(size_t num_players, size_t last_marble)
        : num_players(num_players), last_marble(last_marble) {
            for (size_t player_index{0};
                 player_index < num_players;
                 player_index++) {
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
            for (size_t index{0}; index < 7; index++) {
                if (marble_it == marbles.begin()) {
                    marble_it = marbles.end();
                }
                marble_it--;
            }
            score(current_player) += *marble_it;
            marble_it = marbles.erase(marble_it);
        } else {
            if (marbles.size() == 1) {
                marbles.insert(marbles.end(), current_marble);
                marble_it = std::next(marbles.begin());
            } else if (marbles.size() == 2) {
                marbles.insert(marble_it, current_marble);
                marble_it = std::next(marbles.begin());
            } else if (std::next(marble_it) == marbles.end()) {
                marbles.insert(std::next(marbles.begin()), current_marble);
                marble_it = std::next(marbles.begin());
            } else if (std::next(std::next(marble_it)) == marbles.end()) {
                marbles.insert(marbles.end(), current_marble);
                marble_it = std::prev(marbles.end());
            } else {
                marble_it++;
                marble_it++;
                marbles.insert(marble_it, current_marble);
                marble_it--;
            }
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
        for (auto it=game.marbles.begin(); it != game.marbles.end(); it++) {
            if (it == game.marble_it) {
                os << BOLDWHITE << "(" << *it << ")" << RESET;
            } else {
                os << *it;
            }
            if (std::next(it) != game.marbles.end()) {
                os << " ";
            }
        }

        return os;
    }
};


size_t part_one(size_t num_players, size_t last_marble, bool verbose = false) {
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


size_t part_two(size_t num_players, size_t last_marble, bool verbose = false) {
    return part_one(num_players, last_marble * 100, verbose);
}


int main() {
  utils::Reader reader(std::filesystem::path("../../data/2018/input_09.txt"));
  auto input = reader.get_lines()[0];
  std::regex re{"^(\\d+) players; last marble is worth (\\d+) points$"};
  std::smatch matches;
  std::regex_match(input, matches, re);
  assert(matches.size() == 3);
  size_t num_players = std::stol(matches[1].str());
  size_t last_marble = std::stol(matches[2].str());

  auto answer_one =  part_one(num_players, last_marble);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(num_players, last_marble);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
