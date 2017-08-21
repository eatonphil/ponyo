#include <time.h>

void ponyo_strftime(const time_t t, const char* format, size_t size, char* result) {
  struct tm *resultp;
  resultp = gmtime_r(&t, resultp);
  strftime(result, size, format, resultp);
}
