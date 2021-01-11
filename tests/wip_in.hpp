#pragma once

struct Test {
  enum class Enum {
    first = 1 << 0,
    second = 1 << 1,
    third = 1 << 2
    // secondDifferentName = second
  };

  void acceptsEnum(Enum en) {}
};

void acceptsEnum(Test::Enum en) {}
