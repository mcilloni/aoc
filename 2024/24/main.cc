// AoC 2024 Day 24

/*
 * This is one of "those problems" where it's not really that feasible to find a generic solution for
 * all possible inputs, and it's better to focus on solving it just for your own instead
 * part1 is super trivial, basically Day 2 level of difficulty
 * part2 is horrendously complex, because it basically involves reverse engineering the output
 * through observation it's possible to notice that the input describes a sequence of full adder circuits, i.e.
 * circuits that adds two numbers with a carry-in and a carry-out
 * in particular every xN and yN value are summed with 5 different boolean operations:
 * 1. out1N = xN ^ yN
 * 2. cout1N = xN & yN
 * 3. cout2N = cinN & out1N (note: cirN is coutN-1)
 * 4. zN = out1N ^ cinN
 * 5. coutN = cout1N | cout2N
 * with x0 + y0 being a half adder due to no carry-in
 *
 * This program basically attempts to verify the correctness of the wires by probing what the output should look like
 * and quitting as soon as it finds a discrepancy. This allows the user to manually go through the input file, check
 * what the cout1, cout2, out1, out, and cin values should be, and then verify which ones must be swapped.
 * (thus this program is no solution at all, and you've to trust me that I really inputted the correct values in my 
 * AoC account)
 */

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <charconv>
#include <concepts>
#include <cstddef>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <deque>
#include <expected>
#include <format>
#include <limits>
#include <memory>
#include <print>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

namespace {
    constexpr std::size_t max_outsize_bits{std::numeric_limits<unsigned long long>::digits};

    using bits = std::bitset<max_outsize_bits>;

    class id {    
    public:
        using array = std::array<char, 3ZU>;

        id(const char key, std::uint8_t n) : _value {
            key, static_cast<char>('0' + n / 10), static_cast<char>('0' + n % 10)
        } {
            assert((key == 'x' || key == 'y' || key == 'z') && n < 100);
        };

        constexpr id(const std::array<char, 3ZU> value = {}) : _value { value } {}

        constexpr id(const ::id &other) noexcept = default;

        constexpr char operator[](const std::size_t i) const noexcept {
            return this->_value[i];
        }

        constexpr std::size_t size() const noexcept {
            return *(end(this->_value) - 1) ? std::size(this->_value) : std::strlen(data(this->_value));
        }

        constexpr operator std::string_view() const noexcept {
            return std::string_view{ data(this->_value), this->size() };
        }

        friend constexpr bool operator==(const ::id &lhs, const ::id &rhs) noexcept = default;

        friend constexpr auto operator<=>(const ::id lhs, const auto rhs) noexcept {
            return std::string_view{lhs} <=> std::string_view{rhs};
        }

        constexpr auto to_array() const noexcept {
            return this->_value;
        }

    private:
        std::array<char, 3ZU> _value;
    };
}

template<>
struct std::hash<::id> {
    auto operator()(const ::id &value) const noexcept {
        return std::hash<std::string_view>{}(value);
    }
};

template<>
struct std::formatter<::id> : std::formatter<std::string_view> {
    auto format(const ::id value, auto &ctx) const {
        return this->std::formatter<std::string_view>::format(value, ctx);
    }
};

namespace {
    enum class op_kind : std::uint8_t {
        band,
        bor,
        exor,
    };

    std::optional<::op_kind> from_string(const std::string_view s) {
        using enum ::op_kind;

        if (s == "AND") {
            return band;
        } else if (s == "OR") {
            return bor;
        } else if (s == "XOR") {
            return exor;
        }

        return std::nullopt;
    }

    class op {
        ::op_kind _kind;
        ::id _lhs, _rhs;

    public:
        constexpr op(const ::op_kind kind, const ::id lhs, const ::id rhs)
            : _kind { kind }, _lhs { std::min(lhs, rhs) }, _rhs { std::max(lhs, rhs) } {}

        template <std::size_t I>
        constexpr auto get() const {
            if constexpr (I == 0) {
                return this->_kind;
            } else if constexpr (I == 1) {
                return this->_lhs;
            } else {
                return this->_rhs;
            }
        }

        constexpr auto kind() const noexcept {
            return this->_kind;
        }

        constexpr auto lhs() const noexcept {
            return this->_lhs;
        }

        constexpr auto rhs() const noexcept {
            return this->_rhs;
        }

        friend constexpr bool operator==(const ::op &lhs, const ::op &rhs) noexcept = default;
    };

    using value = std::variant<bool, ::op>;
}

template<>
struct std::tuple_element<0, ::op> {
    using type = ::op_kind;
};

template<>
struct std::tuple_element<1, ::op> {
    using type = ::id;
};

template<>
struct std::tuple_element<2, ::op> {
    using type = ::id;
};

template<>
struct std::tuple_size<::op> : public integral_constant<std::size_t, 3> {};

template <>
struct std::hash<::op> {
    auto operator()(const ::op &value) const noexcept {
        const auto [kind, lhs, rhs] { value };

        const auto lhs_arr { lhs.to_array() };
        const auto rhs_arr { rhs.to_array() };

        std::array<char, size(lhs_arr) + size(rhs_arr) + 1> combined{};

        auto it{begin(combined)};

        *it++ = static_cast<char>(kind) + '0';

        it = std::copy(begin(lhs_arr), end(lhs_arr), it);
        it = std::copy(begin(rhs_arr), end(rhs_arr), it);

        return std::hash<std::string_view>{}(std::string_view{data(combined), size(combined)});
    }
};

template<>
struct std::formatter<::op_kind> : std::formatter<std::string_view> {
    template<typename FormatContext>
    auto format(const ::op_kind &value, FormatContext &ctx) const {
        using enum ::op_kind;

        std::string_view result{};
        switch (value) {
        case band:
            result = "AND";
            break;
        
        case bor:
            result = "OR";
            break;

        case exor:
            result = "XOR";
            break;
        }

        return this->std::formatter<std::string_view>::format(result, ctx);
    }
};

template<>
struct std::formatter<::op> : std::formatter<std::string_view> {
    template<typename FormatContext>
    auto format(const ::op &value, FormatContext &ctx) const {
        const auto [kind, lhs, rhs] { value };
        return std::format_to(ctx.out(), "{} {} {}", lhs, kind, rhs);
    }
};

template<>
struct std::formatter<::value> : std::formatter<std::string_view> {
    template<typename FormatContext>
    auto format(const ::value &value, FormatContext &ctx) const {
        return std::visit([this, &ctx](const auto &v) {
            return std::format_to(ctx.out(), "{}", v);
        }, value);
    }
};

namespace {

    using wireset = std::unordered_map<::id, ::value>;
    using inputset = std::unordered_map<::op, ::id>;

    using error = std::string;

    enum class token_kind : std::uint8_t {
        arrow,
        boolean,
        colon,
        eof,
        id,
        newline,
    };

    template <::token_kind Kind>
    struct data_for {
        using type = std::monostate;
    };

    template <>
    struct data_for<::token_kind::id> {
        using type = ::id;
    };

    template <>
    struct data_for<::token_kind::boolean> {
        using type = bool;
    };

    template<::token_kind Kind>
    using data_for_t = typename data_for<Kind>::type;

    struct token {
        ::token_kind kind;
        std::variant<std::monostate, ::id, bool> data{};

        constexpr bool is(::token_kind k) const noexcept {
            return this->kind == k;
        }

        template <typename T>
        constexpr auto get() const {
            return std::get<T>(this->data);
        }

        template <typename T>
        constexpr auto *try_get() const noexcept {
            return std::get_if<T>(&this->data);
        }
    };

    std::expected<std::optional<char>, ::error> nextch(std::FILE *const file) {
        const int ch = std::fgetc(file);
        if (ch == EOF) {
            if (std::feof(file)) {
                return std::nullopt;
            } else {
                return std::unexpected{std::format("while reading: {}", std::strerror(errno))};
            }
        }

        return static_cast<char>(ch);
    }

    bool is_id_char(const char ch) noexcept {
        return std::isalnum(ch);
    }

    std::expected<::id, ::error> finish_id(std::FILE *const file, const char start) {
        ::id::array id_chars { start };
        for (std::size_t i = 1; i < size(id_chars); ++i) {
            auto ch_res { ::nextch(file) };
            if (!ch_res) {
                return std::unexpected{std::move(ch_res).error()};
            }

            if (!*ch_res || !::is_id_char(**ch_res)) {
                ungetc(**ch_res, file);
                break;
            }

            id_chars[i] = **ch_res;
        }

        return ::id{ id_chars };
    }

    std::expected<void, ::error> skip_spaces(std::FILE *const file) {
        for (;;) {
            auto ch_res { ::nextch(file) };
            if (!ch_res) {
                return std::unexpected{std::move(ch_res).error()};
            }

            if (!*ch_res) {
                break;
            }
            
            if (!std::isblank(**ch_res)) {
                std::ungetc(**ch_res, file);
                break;
            }
        }

        return {};
    }

    std::expected<void, ::error> slurp_newlines(std::FILE *const file) {
        for (;;) {
            auto ch_res { ::nextch(file) };
            if (!ch_res) {
                return std::unexpected{std::move(ch_res).error()};
            }

            if (!*ch_res) {
                break;
            }
            
            if (!std::isspace(**ch_res)) {
                std::ungetc(**ch_res, file);
                break;
            }
        }

        return {};
    }

    std::expected<::token, ::error> next(std::FILE *const file) {
        using enum ::token_kind;

        return ::skip_spaces(file)
            .and_then([file] {
                return ::nextch(file);
            })
            .and_then([file](const std::optional<char> some_ch) -> std::expected<::token, ::error> {
                if (!some_ch) {
                    return ::token { .kind = eof };
                }

                switch (const char ch { *some_ch }; ch) {
                case '\n':
                    return ::slurp_newlines(file).transform([] { return ::token { .kind = newline }; });
                case '-':
                    {
                        auto ch_res { ::nextch(file) };
                        if (!ch_res) {
                            return std::unexpected{std::move(ch_res).error()};
                        }

                        if (!*ch_res || **ch_res != '>') {
                            return std::unexpected{"expected '>'"};
                        }

                        return ::token { .kind = arrow };
                    }

                case '0':
                case '1':
                    return ::token { .kind = boolean, .data = ch == '1' };

                case ':':
                    return ::token { .kind = colon };

                default:
                    if (::is_id_char(ch)) {
                        return ::finish_id(file, ch).transform([](const ::id id) {
                            return ::token { .kind = ::token_kind::id, .data = id };
                        });
                    } else {
                        return std::unexpected{std::format("unexpected character: `{}` (0x{:02X})", ch, ch)};
                    }
                }
            });
    }

    std::expected<std::optional<char>, ::error> peekch(std::FILE *const file) {
        const auto ch_res { ::nextch(file) };
        if (ch_res && *ch_res) {
            std::ungetc(**ch_res, file);
        }
        
        return ch_res;
    }

    template <::token_kind ...Tokens>
    auto expect(std::FILE *const file) {
        using FirstType = std::tuple_element_t<0, std::tuple<::data_for_t<Tokens>...>>;
        using RetType = std::conditional_t<sizeof...(Tokens) == 1, FirstType, ::token>;
        return ::next(file).and_then([](const ::token tok) -> std::expected<RetType, ::error> {
            if ((tok.is(Tokens) || ...)) {
                if constexpr (sizeof...(Tokens) == 1) {
                    return tok.get<FirstType>();
                } else {
                    return tok;
                }
            } else {
                return std::unexpected{"unexpected token"};
            }
        });
    }

    std::expected<typename ::wireset::value_type, ::error> parse_conn(std::FILE *const file, const ::id lhs, const ::id op) {
        return ::expect<::token_kind::id>(file)
                    .and_then([lhs, op](const ::id rhs_id)-> std::expected<::op, ::error> {
                        using namespace std::literals;
                        using enum ::op_kind;

                        ::op_kind op_kind{};
                        if (op == "AND"sv) {
                            op_kind = band;
                        } else if (op == "OR"sv) {
                            op_kind = bor;
                        } else if (op == "XOR"sv) {
                            op_kind = exor;
                        } else {
                            return std::unexpected{std::format("expected AND, OR, or XOR, got `{}`", op)};
                        }

                        return ::op(op_kind, lhs, rhs_id);
                    })
                .and_then([file](const ::op op) {
                    return ::expect<::token_kind::arrow>(file)
                        .and_then([file, op](auto) {
                            return ::expect<::token_kind::id>(file)
                                .transform([op](const ::id id) {
                                    return ::wireset::value_type{id, op};
                                });
                        });
                });
    }

    std::expected<typename ::wireset::value_type, ::error> parse_def(std::FILE *const file, const ::id id) {        
        auto value_res { ::expect<::token_kind::id, ::token_kind::boolean>(file) };
        if (!value_res) {
            return std::unexpected{std::move(value_res).error()};
        }
        
        auto *const value { value_res->try_get<bool>() };
        if (!value) {
            return std::unexpected{"expected boolean"};
        }

        return ::wireset::value_type{id, *value};
    } 

    std::optional<std::uint8_t> get_numid(const ::id id) {
        const std::array num { id[1], id[2] };

        std::uint8_t wireno{};

        const auto *const beg { data(num) };
        const auto *const nd { data(num) + size(num) };

        if (const auto [ptr, ec] { std::from_chars(beg, nd, wireno, 10) }; ptr != nd || ec != std::errc{}) {
            return std::nullopt;
        }

        return wireno;
    }
    
    std::expected<::wireset, ::error> parse_defs(std::FILE *const file) {
        ::wireset result{};

        for (;;) {
            auto linestart_res { ::expect<::token_kind::id, ::token_kind::newline, ::token_kind::eof>(file) };
            if (!linestart_res) {
                return std::unexpected{std::move(linestart_res).error()};
            }

            if (linestart_res->is(::token_kind::newline)) {
                continue;
            } else if (linestart_res->is(::token_kind::eof)) {
                break;
            }

            const auto lhs { linestart_res->get<::id>() };

            auto next_res { ::expect<::token_kind::colon, ::token_kind::id>(file) };
            if (!next_res) {
                return std::unexpected{std::move(next_res).error()};
            }

            auto def_res {
                next_res->is(::token_kind::colon)
                    ? ::parse_def(file, lhs)
                    : ::parse_conn(file, lhs, next_res->get<::id>())
            };

            if (!def_res) {
                return std::unexpected{std::move(def_res).error()};
            }

            auto [id, value] { std::move(*def_res) };

            result[id] = value;

            auto newline_res { ::expect<::token_kind::newline, ::token_kind::eof>(file) };
            if (!newline_res) {
                return std::unexpected{std::move(newline_res).error()};
            }

            if (newline_res->is(::token_kind::eof)) {
                break;
            }
        }

        return result;
    }

    std::expected<::wireset, ::error> parse_from(std::FILE *const file) {
        return ::parse_defs(file);
    }

    std::expected<::wireset, ::error> parse(const char *const fpath) {
        auto *const file = std::fopen(fpath, "r");
        if (!file) {
            return std::unexpected{std::format("while opening {}: {}", fpath, std::strerror(errno))};
        }

        auto result { ::parse_from(file) };

        std::fclose(file);

        return result;
    }

    constexpr ::bits badbits(const ::bits got, const ::bits expected) noexcept {
        return got ^ expected;
    }

    constexpr bool do_op(const ::op_kind kind, const bool lhs, const bool rhs) {
        switch (kind) {
        case ::op_kind::band:
            return lhs & rhs;
        case ::op_kind::bor:
            return lhs | rhs;
        case ::op_kind::exor:
            return lhs ^ rhs;
        }

        std::terminate();
    }

    bool eval_wire(::wireset &wires, const ::id id) {
        const auto wire { wires.at(id) };

        return std::visit([&wires, id]<typename T>(const T v) {
            if constexpr (std::same_as<T, ::op>) {
                const auto [kind, lhs, rhs] { v };

                const auto lval { ::eval_wire(wires, lhs) };
                const auto rval { ::eval_wire(wires, rhs) };

                const auto result { ::do_op(kind, lval, rval) };

                wires[id] = result;

                return result;
            } else {
                return v;
            }   
        }, wire);
    }

    unsigned long long eval(::wireset &wires, const char key = 'z') {
        ::bits values{};

        for (const auto [wire, val] : wires) {
            if (wire[0] == key) {
                const auto some_numid { ::get_numid(wire) };
                if (!some_numid) {
                    assert(false);
                    continue;
                }

                values.set(*some_numid, ::eval_wire(wires, wire));
            }
        }

        return values.to_ullong();
    }

    unsigned long long extract(const ::wireset &solved_wires, const char key = 'z') {
        std::bitset<::max_outsize_bits> values{};

        for (const auto [wire, val] : solved_wires) {
            if (wire[0] == key) {
                const auto some_numid { ::get_numid(wire) };
                if (!some_numid) {
                    assert(false);
                    continue;
                }

                values.set(*some_numid, std::visit([&solved_wires]<typename T>(const T v) -> bool {
                    if constexpr (std::same_as<T, ::op>) {
                        throw "contract failure: the wireset should be solved";
                    } else {
                        return v;
                    }   
                }, solved_wires.at(wire)));
            }
        }

        return values.to_ullong();
    }    

    struct adder_out {
        ::id out;
        ::id cout;
    };

    ::adder_out check_adder(const ::inputset &inputs, const ::id lhs, const ::id rhs, const std::optional<::id> cin) {
        // adder: out = (lhs XOR rhs) XOR cin, cout = (lhs AND rhs) OR (cin AND (lhs XOR rhs))
        
        // out1 = lhs XOR rhs
        // cout1 = lhs AND rhs
        // cout2 = cin AND out1
        // out = out1 XOR cin
        // cout = cout1 OR cout2
        const auto get_if {
            [&inputs](const ::op &op) -> const std::optional<::id> {
                const auto it { inputs.find(op) };
                if (it == end(inputs)) {
                    return std::nullopt;
                }
                 
                return it->second;
            }
        };

        const auto expect_z {
            [](const ::id &id, const std::string_view msg) {
                if (id[0] != 'z' || !std::isdigit(id[1]) || !std::isdigit(id[2])) {
                    std::println("expected zXX, got {} at {}", id, msg);
                    std::exit(7);
                }
            }
        };

        const auto expect_temp {
            [](const ::id &id, const std::string_view msg) {
                for (const char c : id.to_array()) {
                    if (!std::isalpha(c)) {
                        std::println("expected temporary id, got {} at {}", id, msg);
                        std::exit(8);
                    }
                }
            }
        };

        const ::op expected1(::op_kind::exor, lhs, rhs);

        const auto out1 { get_if(expected1) };
        if (!out1) {
            std::println("missing out1: {}", expected1);
            std::exit(1);
        }

        std::println("{} -> {}", expected1, *out1);

        const ::op expected2(::op_kind::band, lhs, rhs);

        const auto cout1 { get_if(expected2) };
        if (!cout1) {
            std::println("missing cout1: {}", expected2);
            std::exit(2);
        }

        std::println("{} -> {}", expected2, *cout1);

        expect_temp(*cout1, "cout1");

        if (!cin) {
            expect_z(*out1, "out1, no cin");

            return ::adder_out{ .out = *out1, .cout = *cout1 };
        }

        expect_temp(*out1, "out1, with cin");

        const ::op expected3(::op_kind::band, *cin, *out1);
        
        const auto cout2 { get_if(expected3) };
        if (!cout2) {
            std::println("missing cout2: {}", expected3);
            std::exit(3);
        }

        std::println("{} -> {}", expected3, *cout2);

        expect_temp(*cout2, "cout2");

        const ::op expected4(::op_kind::exor, *out1, *cin);

        const auto out { get_if(expected4) };
        if (!out) {
            std::println("missing out: {}", expected4);
            std::exit(4);
        }

        std::println("{} -> {}", expected4, *out);

        expect_z(*out, "out");

        const ::op expected5(::op_kind::bor, *cout1, *cout2);

        const auto cout { get_if(expected5) };
        if (!cout) {
            std::println("missing cout: {}", expected5);
            std::exit(5);
        }

        std::println("{} -> {}", expected5, *cout);

        expect_temp(*cout, "cout");

        return ::adder_out{ .out = *out, .cout = *cout };
    }

    ::inputset flip(const ::wireset &wires) {
        ::inputset result{};

        for (const auto &[id, value] : wires) {
            if (const auto *const op { std::get_if<::op>(&value) }) {
                result[*op] = id;
            }
        }

        return result;
    }

    bool check_badbits(const ::wireset &wires, const ::wireset &wirevals) {
        const ::bits x { ::extract(wirevals, 'x') };
        const ::bits y { ::extract(wirevals, 'y') };
        const ::bits z { ::extract(wirevals, 'z') };
        const ::bits expected { x.to_ullong() + y.to_ullong() };

        const auto badbits { ::badbits(z, expected) };

        std::println("bad bits: {:b}", badbits.to_ullong());

        for (const auto n : std::views::iota(0zu, ::max_outsize_bits)) {
            if (badbits.test(n)) {
                const ::id wireid {'z', static_cast<std::uint8_t>(n)};

                std::println("output bit {} is wrong, expected {}, got {}", wireid, expected.test(n), z.test(n));
            }
        }

        return badbits.any();
    }

    void check_adder_integ(const ::wireset &wires, const ::inputset &inputs) {
        std::optional<::id> cin {};

        for (const auto n : std::views::iota(0)) {
            if (n > std::numeric_limits<std::uint8_t>::max()) {
                throw "contract failure: n is too large";
            }
            
            const auto inum { static_cast<std::uint8_t>(n) };
            const ::id expected{'z', inum};

            if (!wires.contains(expected)) {
                break;
            }
            
            const ::id lhs {'x', static_cast<std::uint8_t>(inum)};
            const ::id rhs {'y', static_cast<std::uint8_t>(inum)};

            const auto [out, cout] { ::check_adder(inputs, lhs, rhs, cin) };

            if (out != expected) {
                std::println("bad output, expected {} got {}", expected, out);
                std::exit(6);
            }

            cin = cout;
        }
    }
        
}

int main(const int argc, const char *const argv[]) {
    if (argc != 2) {
        std::println(stderr, "error: wrong number of arguments\nusage: {} INPUT", argv[0]);

        return 2;
    }

    auto result { ::parse(argv[1]) };
    if (!result) {
        std::println(stderr, "error: {}", result.error());

        return 1;
    }

    const auto &wires { *result };
    auto wcopy { wires };
    auto inputs { ::flip(wires) };

    // for (const auto &[id, value] : wires) {
    //     std::println("{}: {}", id, value);
    // }

    for (const auto [op, id] : inputs) {
        std::println("{} -> {}", op, id);
    }

    const auto output { ::eval(wcopy) };
    std::println("part 1: {}", output);

    const auto has_badbits { ::check_badbits(wires, wcopy) };

    if (has_badbits) {
        ::check_adder_integ(wires, inputs);
    } else {
        std::println("input is sound");
    }

    return 0;
}
