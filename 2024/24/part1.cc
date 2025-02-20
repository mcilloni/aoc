// AoC 2024 Day 24

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
#include <expected>
#include <format>
#include <limits>
#include <memory>
#include <print>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <variant>

namespace {
    constexpr std::size_t max_outsize_bits{std::numeric_limits<unsigned long long>::digits};

    class id {    
    public:
        using array = std::array<char, 3ZU>;

        constexpr id(const std::array<char, 3ZU> value = {}) : _value { value } {}

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

    private:
        std::array<char, 3ZU> _value;
    };
}

template<>
struct std::hash<::id> {
    constexpr auto operator()(const ::id &value) const noexcept {
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

    struct op {
        ::op_kind kind;
        ::id lhs, rhs;
    };

    using value = std::variant<::op, bool>;
}

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
        return std::format_to(ctx.out(), "{} {} {}", value.lhs, value.kind, value.rhs);
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

    constexpr bool is_id_char(const char ch) noexcept {
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

                        return ::op{ .kind = op_kind, .lhs = lhs, .rhs = rhs_id };
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
        if (const auto [ptr, ec] { std::from_chars(begin(num), end(num), wireno, 10) }; ptr != end(num) || ec != std::errc{}) {
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

    constexpr bool do_op(const ::op_kind kind, const bool lhs, const bool rhs) {
        switch (kind) {
        case ::op_kind::band:
            return lhs & rhs;
        case ::op_kind::bor:
            return lhs | rhs;
        case ::op_kind::exor:
            return lhs ^ rhs;
        }
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

    unsigned long long eval(::wireset &wires) {
        std::bitset<::max_outsize_bits> values{};

        for (const auto [wire, val] : wires) {
            if (wire[0] == 'z') { // this is an output
                const auto some_numid { ::get_numid(wire) };
                if (!some_numid) {
                    assert(false);
                    continue;
                }

                values[*some_numid] = ::eval_wire(wires, wire);
            }
        }

        return values.to_ullong();
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

    for (const auto &[id, value] : wires) {
        std::println("{}: {}", id, value);
    }

    const auto output { ::eval(wcopy) };
    std::println("part 1: {}", output);

    return 0;
}
