from __future__ import annotations

import sys
from dataclasses import dataclass

from z3.z3 import Solver, Real
from z3.z3types import Z3Exception

@dataclass(frozen=True)
class Point:
    x: float
    y: float
    z: float

    def __add__(self, other: Point) -> Point:
        return Point(self.x + other.x, self.y + other.y, self.z + other.z)

    def __mul__(self, coeff: float) -> Point:
        return Point(self.x * coeff, self.y * coeff, self.z * coeff)


@dataclass(frozen=True)
class Stone:
    start: Point
    velocity: Point


def parse_stone(line: str) -> Stone:
    raw_position, raw_velocity = line.split(' @ ')
    position = Point(*map(float, raw_position.split(', ')))
    velocity = Point(*map(float, raw_velocity.split(', ')))
    return Stone(position, velocity)


def ray_intersect(s1: Stone, s2: Stone, min_bound: int, max_bound: int) -> bool:
    dx = s2.start.x - s1.start.x
    dy = s2.start.y - s1.start.y
    det = s2.velocity.x * s1.velocity.y - s2.velocity.y * s1.velocity.x
    if det == 0:
        return False
    t1 = (dy * s2.velocity.x - dx * s2.velocity.y) / det
    t2 = (dy * s1.velocity.x - dx * s1.velocity.y) / det
    if t1 < 0 or t2 < 0:
        return False
    intersection = s1.start + s1.velocity * t1
    return min_bound <= intersection.x <= max_bound and min_bound <= intersection.y <= max_bound


def part1(stones: tuple[Stone, ...], min_bound: int, max_bound: int) -> int:
    count = 0
    for idx, s1 in enumerate(stones):
        for jdx, s2 in enumerate(stones):
            if idx >= jdx:
                continue
            if ray_intersect(s1, s2, min_bound, max_bound):
                count += 1
    return count


def part2(stones: tuple[Stone, ...]) -> int:
    s = Solver()
    rx, ry, rz, rdx, rdy, rdz = map(Real, 'rx ry rz rdx rdy rdz'.split())
    for idx, stone in enumerate(stones):
        t = Real(f't{idx}')
        s.add(rx + t * rdx == stone.start.x + t * stone.velocity.x)
        s.add(ry + t * rdy == stone.start.y + t * stone.velocity.y)
        s.add(rz + t * rdz == stone.start.z + t * stone.velocity.z)
    s.check()
    m = s.model()
    return m[rx].as_long() + m[ry].as_long() + m[rz].as_long()


def main() -> None:
    stones = tuple(parse_stone(line.strip()) for line in sys.stdin)
    match sys.argv:
        case [_, 'demo', '1']: r = part1(stones, 7, 27)
        case [_, 'part', '1']: r = part1(stones, 200000000000000, 400000000000000)
        case [_, _,      '2']: r = part2(stones)
        case [program, *_]:
            print(f'Usage: {program} <demo|part> <1|2>', file=sys.stderr)
            sys.exit(1)
    print(r)

if __name__ == '__main__':
    main()
