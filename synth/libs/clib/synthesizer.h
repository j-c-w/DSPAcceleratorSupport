#define POWER_OF_TWO(x) ((x & (x - 1)) == 0)
#define GREATER_THAN(x, y) x > y
#define GREATER_THAN_OR_EQUAL(x, y) x >= y
#define LESS_THAN(x, y) x < y
#define LESS_THAN_OR_EQUAL(x, y) x <= y
#define PRIM_EQUAL(x, y) x == y
/* TODO --- would like to make this better.  */
#define FLOAT_EQUAL(x, y) ((x < y + x / 1000.0) && (x > y - x / 1000.0))
