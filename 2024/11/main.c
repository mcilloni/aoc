// AoC 2024 Day 11

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// this is an implementation of a hash table that I wrote for a different project at work
// this will come in handy for memoizing the results of the visit_count function
#include "hashtable.h"

// I have basically assumed that uintmax_t is at least 64 bits, which is true on all platforms I know of except some
// weird stuff like 8-bit microcontrollers and DOS
_Static_assert(sizeof(uintmax_t) >= sizeof(uint64_t), "uintmax_t is too small");

#define STARTING_CAP 7U
#define UINTMAX_MAX_DIGITS (sizeof(uintmax_t) / sizeof(uint64_t) * 20U)
#define REASONABLY_LARGE_LABEL (UINTMAX_MAX_DIGITS * 2U + 2U)

// values > 0 are from errno
enum error {
    OK = 0,
    END_OF_FILE = -1,
    UNEXPECTED = -2,
};

struct array {
    size_t len, cap;
    uintmax_t value[];
};

static uintmax_t *array_begin(struct array *const stones) {
    return stones ? stones->value : NULL;
}

static size_t array_cap(const struct array *const stones) {
    return stones ? stones->cap : 0U;
}

static uintmax_t *array_end(struct array *const stones) {
    return stones ? stones->value + stones->len : NULL;
}

static bool array_grow(struct array **const stones) {
    assert(stones);

    const size_t old_cap = array_cap(*stones);
    const size_t new_cap = old_cap ? old_cap * 3U / 2U : STARTING_CAP;

    struct array *const new_stones = realloc(*stones, sizeof **stones + new_cap * sizeof *(*stones)->value);
    if (!new_stones) {
        return false;
    }
    
    new_stones->cap = new_cap;

    if (!old_cap) {
        // initialize the array
        new_stones->len = 0U;
    }

    *stones = new_stones;

    return true;
}

static size_t array_len(const struct array *const stones) {
    return stones ? stones->len : 0U;
}

static bool array_append(struct array **const stones, const uintmax_t value) {
    assert(stones);

    if (array_cap(*stones) <= array_len(*stones)) {
        if (!array_grow(stones)) {
            return false;
        }
    }

    (*stones)->value[(*stones)->len++] = value;

    return true;
}

static bool format_label(char *const dest, const size_t ndest, const uintmax_t value, const size_t depth) {
    const size_t written = snprintf(dest, ndest, "%" PRIuMAX ":%zu", value, depth);
    return written < ndest;
} 

static bool split_if_even_digits(uintmax_t value, uintmax_t *const high, uintmax_t *const low) {
    assert(high && low);

    if (!value) {
        return false;
    }

    char digits[UINTMAX_MAX_DIGITS] = {0};
    char *cur = digits;

    while (value) {
        *cur++ = value % 10U;
        value /= 10U;
    }

    const size_t digits_count = cur - digits;
    if (digits_count % 2U) {
        return false;
    }

    *high = *low = 0LLU;

    uintmax_t *target = high;
    const char *const mid = digits + digits_count / 2;

    while (cur >= digits) {
        *target = *target * 10U + *cur;

        if (cur == mid) {
            target = low;
        }

        --cur;
    }

    return true;
}

static void blink(struct array **const array) {
    assert(array);

    struct array *new_array = NULL;

    for (const uintmax_t *it = array_begin(*array), *const end = array_end(*array); it < end; ++it) {
        uintmax_t cur = *it, high = 0U, low = 0U;

        bool success = false;

        if (!cur) {
            success = array_append(&new_array, 1U);
        } else if (split_if_even_digits(cur, &high, &low)) {
            success = array_append(&new_array, high) && array_append(&new_array, low);
        } else {
            success = array_append(&new_array, cur * 2024LLU);
        }

        assert(success);
    }

    free(*array);
    *array = new_array;
}

static void dump_stones(struct array *const array) {
    for (const uintmax_t *it = array_begin(array), *const end = array_end(array); it < end; ++it) {
        printf("%" PRIuMAX " ", *it);
    }

    putchar('\n');
}

static const char *errstr(const int error) {
    switch (error) {
    case OK:
        return "OK";

    case END_OF_FILE:
        return "end of file";

    case UNEXPECTED:
        return "unexpected character";

    default:
        // it's 2025 and still there's no safe standard way to get a string from an error code (strerror_s doesn't exist
        // in practice)
        return strerror(error);
    }
}

static int nextch(FILE *const file, int *const out) {
    assert(file && out);

    errno = 0;

    const int ch = fgetc(file);
    if (ch == EOF) {
        return errno ? errno : END_OF_FILE;
    }

    *out = ch;

    return OK;
}

static int skip_whitespace(FILE *const file) {
    assert(file);

    for (;;) {
        errno = 0;

        int ch = 0;
        
        int error = nextch(file, &ch);
        if (error) {
            // return OK if we reached the end of the file, because this function reads 0 or more whitespace characters
            return error == END_OF_FILE ? OK : error;
        }

        if (!isspace(ch)) {
            error = ungetc(ch, file);
            if (error == EOF) {
                return errno;
            }

            return OK;
        }
    }
}

static int read_unsigned(FILE *const file, uintmax_t *const out) {
    assert(file && out);

    int error = skip_whitespace(file);
    if (error) {
        return error;
    }
    
    // read first digit
    int ch = 0;

    error = nextch(file, &ch);
    if (error) { // always fail if we can't read at least one character
        return error;
    }

    if (!isdigit(ch)) {
        return UNEXPECTED;
    }

    uintmax_t value = ch - '0';
    for (;;) {
        error = nextch(file, &ch);
        switch (error) {
        case OK:
            break;

        case END_OF_FILE:
            goto quit;
        
        default:
            return error;
        }

        if (!isdigit(ch)) {
            error = ungetc(ch, file);
            if (error == EOF) {
                return errno;
            }

            break;
        }

        value = value * 10 + (ch - '0');        
    }

quit:
    *out = value;

    return OK;
}

static int parse_file(const char *const filename, struct array **dest) {
    assert(filename && dest);

    FILE *const file = fopen(filename, "r");
    if (!file) {
        return errno;
    }

    struct array *list = NULL;
    for (;;) {
        uintmax_t value = 0U;
        int error = read_unsigned(file, &value);
        switch (error) {
        case OK:
            break;
        
        case END_OF_FILE:
            goto quit;

        default:
            free(list);

            return error;
        }

        error = array_append(&list, value) ? OK : ENOMEM;
    }

quit:
    *dest = list;
    return fclose(file);
}

struct visit_state {
    char buffer[REASONABLY_LARGE_LABEL];
    struct hashtable *visited;
};

static uintmax_t visit_count(const uintmax_t n, const size_t depth, struct visit_state *const state) {
    if (!depth) {
        return 1U; // there's just one stone, us
    }

    bool ok = format_label(state->buffer, sizeof state->buffer, n, depth);
    assert(ok);

    const uintmax_t *const existing = hashtable_get(state->visited, state->buffer);
    if (existing) {
        return *existing; // memoized
    }

    uintmax_t result = 0U, high = 0U, low = 0U;

    if (!n) {
        result = visit_count(1U, depth - 1U, state);
    } else if (split_if_even_digits(n, &high, &low)) {
        result = visit_count(high, depth - 1U, state) + visit_count(low, depth - 1U, state);
    } else {
        result = visit_count(n * 2024LLU, depth - 1U, state);
    }

    ok = format_label(state->buffer, sizeof state->buffer, n, depth);
    assert(ok);
    
    const enum hash_set_result res = hashtable_set(&state->visited, state->buffer, result, NULL);
    assert(res != HASH_SET_FAILED);

    return result;
}

static uintmax_t visit_stones(struct array *const stones, const size_t depth) {
    uintmax_t count = 0U;

    struct visit_state state = {
        .visited = hashtable_new(),
    };

    assert(state.visited);

    for (const uintmax_t *it = array_begin(stones), *const end = array_end(stones); it < end; ++it) {
        count += visit_count(*it, depth, &state);
    }

    hashtable_delete(state.visited);

    return count;
}

int main(const int argc, const char *const argv[]) {
    if (argc != 2) {
        fprintf(stderr,  "error: wrong number of arguments\nusage: %s INPUT\n", argv[0]);

        return 2;
    }

    struct array *list = NULL;
    const int error = parse_file(argv[1], &list);
    if (error) {
        fprintf(stderr, "error: failed to parse file: %s\n", errstr(error));

        return 1;
    }

    printf("part1: %" PRIuMAX "\n", visit_stones(list, 25U));
    printf("part2: %" PRIuMAX "\n", visit_stones(list, 75U));

    return EXIT_SUCCESS;
}
