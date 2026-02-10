#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdatomic.h>

/* --- Thread-Safe Ring Buffer --- */
typedef struct {
    void **buffer;
    size_t capacity;
    atomic_size_t head;
    atomic_size_t tail;
    pthread_mutex_t mutex;
    pthread_cond_t not_empty;
    pthread_cond_t not_full;
} RingBuffer;

RingBuffer *ring_create(size_t capacity) {
    RingBuffer *rb = calloc(1, sizeof(RingBuffer));
    rb->buffer = calloc(capacity, sizeof(void *));
    rb->capacity = capacity;
    atomic_init(&rb->head, 0);
    atomic_init(&rb->tail, 0);
    pthread_mutex_init(&rb->mutex, NULL);
    pthread_cond_init(&rb->not_empty, NULL);
    pthread_cond_init(&rb->not_full, NULL);
    return rb;
}

int ring_push(RingBuffer *rb, void *item) {
    pthread_mutex_lock(&rb->mutex);
    size_t next = (atomic_load(&rb->head) + 1) % rb->capacity;
    while (next == atomic_load(&rb->tail))
        pthread_cond_wait(&rb->not_full, &rb->mutex);
    rb->buffer[atomic_load(&rb->head)] = item;
    atomic_store(&rb->head, next);
    pthread_cond_signal(&rb->not_empty);
    pthread_mutex_unlock(&rb->mutex);
    return 0;
}

void *ring_pop(RingBuffer *rb) {
    pthread_mutex_lock(&rb->mutex);
    while (atomic_load(&rb->tail) == atomic_load(&rb->head))
        pthread_cond_wait(&rb->not_empty, &rb->mutex);
    void *item = rb->buffer[atomic_load(&rb->tail)];
    atomic_store(&rb->tail, (atomic_load(&rb->tail) + 1) % rb->capacity);
    pthread_cond_signal(&rb->not_full);
    pthread_mutex_unlock(&rb->mutex);
    return item;
}

/* --- Memory Pool Allocator --- */
typedef struct Block { struct Block *next; } Block;

typedef struct {
    Block *free_list;
    size_t block_size;
    size_t pool_size;
    void *pool;
} MemPool;

MemPool *pool_create(size_t block_size, size_t count) {
    MemPool *mp = malloc(sizeof(MemPool));
    mp->block_size = block_size < sizeof(Block) ? sizeof(Block) : block_size;
    mp->pool_size = mp->block_size * count;
    mp->pool = malloc(mp->pool_size);
    mp->free_list = NULL;
    char *p = (char *)mp->pool;
    for (size_t i = 0; i < count; i++) {
        Block *b = (Block *)(p + i * mp->block_size);
        b->next = mp->free_list;
        mp->free_list = b;
    }
    return mp;
}

void *pool_alloc(MemPool *mp) {
    if (!mp->free_list) return NULL;
    Block *b = mp->free_list;
    mp->free_list = b->next;
    return b;
}

void pool_free(MemPool *mp, void *ptr) {
    Block *b = (Block *)ptr;
    b->next = mp->free_lis
