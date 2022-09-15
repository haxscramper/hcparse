#define WIP 0

template <typename T1>
struct test {};

template <typename Tp, Tp v>
struct integral_constant {
#if WIP
    static const Tp                  value = v;
    typedef Tp                       value_type;
    typedef integral_constant<Tp, v> type;
#endif
    typedef test<test<Tp>> test_t;
};

#if WIP

template <bool B, class T = void>
struct enable_if {};

template <class T>
struct enable_if<true, T> {
    typedef T type;
};

typedef integral_constant<bool, true>  true_type;
typedef integral_constant<bool, false> false_type;

// clang-format off
template <typename Tp> struct remove_const { typedef Tp type; };
template <typename Tp> struct remove_const<Tp const> { typedef Tp type; };
template <typename Tp> struct remove_volatile { typedef Tp type; };
template <typename Tp> struct remove_volatile<Tp volatile> { typedef Tp type; };
// clang-format on

template <typename Tp>
struct remove_cv {
    typedef typename remove_const<typename remove_volatile<Tp>::type>::type
        type;
};

template <typename>
struct is_integral_helper : public false_type {};

template <>
struct is_integral_helper<int> : public integral_constant<bool, true> {};


template <typename Tp>
struct is_integral
    : public integral_constant<
          bool,
          (is_integral_helper<typename remove_cv<Tp>::type>::value)> {};


template <
    class T,
    typename enable_if<is_integral<T>::value, bool>::type* = nullptr>
int sfinae_overload(T t) {
}


template <class T>
float sfinae_overload(T t) {
}


// template <
//   class T,
//   typename enable_if<
//     is_floating_point<T>::value, bool
//   >::type* = nullptr
// >
// sfinaeOverload() {
//   std::cout << "Calling floating point type function\n";
// }

#endif
