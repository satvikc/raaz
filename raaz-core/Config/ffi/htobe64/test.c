#include <stdint.h>
#include <endian.h>

uint64_t be64(uint64_t arg)
{
    return htobe64 (arg);
}
