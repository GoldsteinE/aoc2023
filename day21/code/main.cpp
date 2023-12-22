#include <cstdint>
#include <cstring>
#include <vector>
#include <string>
#include <utility>
#include <iostream>
#include <boost/container/flat_set.hpp>

using boost::container::flat_set;

auto rem(auto x, auto y) {
    return (x % y + y) % y;
}

struct point {
    ssize_t x, y;
    auto friend operator<=>(const point&, const point&) = default;
    point operator+(const point& other) const {
        return {x + other.x, y + other.y};
    }
    void operator+=(const point& other) {
        x += other.x;
        y += other.y;
    }
};

struct map {
    ssize_t width;
    ssize_t height;
    std::vector<uint8_t> cells;

    const auto& operator[](point p) const {
        return cells[p.y * width + p.x];
    }

    point find_start() const {
        for (ssize_t x = 0; x < width; x++) {
            for (ssize_t y = 0; y < height; y++) {
                if ((*this)[{x, y}] == 'S') return {x, y};
            }
        }
        throw "tough luck";
    }

    point effective_location(point p) {
        return {rem(p.x, width), rem(p.y, height)};
    }

    auto in_bounds(point p) {
        return p.x >= 0 && p.y >= 0 && p.x < width && p.y < height;
    }

    auto propagate(const flat_set<point>& to_check, flat_set<point>& result, bool is_part1) {
        result.clear();
        point offsets[4] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
        for (auto p : to_check) {
            for (auto d : offsets) {
                auto np = p + d;
                if (is_part1 && !in_bounds(np)) continue;
                if ((*this)[effective_location(np)] == '#') continue;
                result.insert(np);
            }
        }
    }
};

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cerr << "Usage: day21 <part|demo> <1|2>" << std::endl;
        return 1;
    }

    std::vector<uint8_t> cells;
    ssize_t width = 0;
    ssize_t height = 0;
    for (std::string line; std::getline(std::cin, line); height++) {
        width = line.size();
        cells.insert(cells.end(), line.begin(), line.end());
    }
    map m{width, height, std::move(cells)};

    auto is_demo = strncmp(argv[1], "demo", 4) == 0;
    auto is_part1 = strncmp(argv[2], "1", 1) == 0;
    auto stop_on =
        is_part1
        ? (is_demo ? 6 : 64)
        : (is_demo ? 500 : (width / 2 + width * 2));

    auto start = m.find_start();
    flat_set<point> curr = {start};
    flat_set<point> scratch;
    for (auto i = 0; i < stop_on; i++) {
        m.propagate(curr, scratch, is_part1);
        std::swap(curr, scratch);
    }

    if (is_part1 || is_demo) {
        std::cout << curr.size() << std::endl;
    } else {
        ssize_t r[5][5] = {};
        for (auto p : curr) {
            p += point{m.width * 2, m.height * 2};
            auto rx = p.x / m.width;
            auto ry = p.y / m.height;
            r[ry][rx]++;
        }

        auto steps = 26501365 / width;
        auto angles = r[0][2] + r[4][2] + r[2][0] + r[2][4];
        auto small_slopes = steps * (r[0][1] + r[0][3] + r[4][1] + r[4][3]);
        auto big_slopes = (steps - 1) * (r[1][1] + r[1][3] + r[3][1] + r[3][3]);
        auto like_center = r[2][2] * (steps - 1) * (steps - 1);
        auto also_filled_but_not_like_center = r[1][2] * steps * steps;

        auto result = angles + small_slopes + big_slopes + like_center + also_filled_but_not_like_center;
        std::cout << result << std::endl;
    }
}

// vim: ts=4 sw=4 et
