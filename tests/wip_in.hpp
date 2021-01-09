#pragma once

#include <initializer_list>

template <class T>
struct HashIterator
{
  typedef T* pointer;
  typedef T& reference;
  typedef T valueType;

  reference operator()(const T){}
  reference operator++();

  bool operator!=(const T& other) {}
  bool operator==(const T& other) {}
};

template <class T>
class HashImpl {
  typedef HashIterator<T> iterator;

public:
  iterator begin() {}
  iterator end() {}
};

template <class T>
class Set {
  HashImpl<T> table;

public:
  Set(std::initializer_list<T> ilist) {}
};
