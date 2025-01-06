/*
 * Copyright (c) 2024-2025 Zuru Tech HK Limited, All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// hashtable taken from https://github.com/zurutech/dicey

#if !defined(XCRGTMFDFE_HASHTABLE_H)
#define XCRGTMFDFE_HASHTABLE_H

#include <stdbool.h>
#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

struct hashtable;

struct hashtable_iter {
    const struct hashtable *_table;
    const void *_current;
};

struct hashtable_entry {
    const char *key;
    uintmax_t value;
};

struct hashtable *hashtable_new(void);

void hashtable_delete(struct hashtable *table);

struct hashtable_iter hashtable_iter_start(const struct hashtable *table);
bool hashtable_iter_next(struct hashtable_iter *iter, const char **key, uintmax_t *value);

bool hashtable_contains(const struct hashtable *table, const char *key);
const uintmax_t *hashtable_get(const struct hashtable *table, const char *key);

const uintmax_t *hashtable_get_entry(
    const struct hashtable *table,
    const char *key,
    struct hashtable_entry *entry
);

bool hashtable_remove(struct hashtable *table, const char *key);

enum hash_set_result {
    HASH_SET_FAILED = 0,
    HASH_SET_ADDED,
    HASH_SET_UPDATED,
};

enum hash_set_result hashtable_set(
    struct hashtable **table,
    const char *key,
    uintmax_t value,
    uintmax_t *old_value
);

uint32_t hashtable_size(const struct hashtable *table);

#if defined(__cplusplus)
}
#endif

#endif // XCRGTMFDFE_HASHTABLE_H
