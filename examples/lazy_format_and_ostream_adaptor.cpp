
#include <concepts>
#include <format>
#include <ostream>
#include <streambuf>
#include <type_traits>
#include <print>
#include <iostream>

#include <dtl/format_extend.hpp>

// We start with some simple example-types.

struct can_only_stream {
    int foo{};
};

inline std::ostream& operator<<(std::ostream& in, can_only_stream const& v) {
    return in << "'can_only_stream' with foo '" << v.foo << '\'';
}

struct can_only_format {
    int bar{};
};


namespace std {
    template <>
    struct formatter<::can_only_format, char> {
        
        template<typename ParseContext>
        constexpr ParseContext::iterator parse(ParseContext& ctx) {
            return ctx.begin();
        }
        template<class FmtContext>
        constexpr FmtContext::iterator format(::can_only_format const& s, FmtContext& ctx) const {
            return format_to(ctx.out(), "can only format with value '{}'", s.bar);
        }
    };
}


int main(int, char**) {
    auto only_stream = can_only_stream(1);
    std::print("First we print some stuff: {} and {}\n", dtl::stream_formatter(only_stream), dtl::stream_formatter(can_only_stream(2)));
    
    auto only_fmt = can_only_format(3);
    std::cout << "And now " << dtl::lazy_format("Some lazy formatted strings to cout, '{}' and '{}'\n", can_only_format(3), can_only_format(4));
    return 0;
}
