#include <time.h>

void ponyo_strftime(const long t, const char* format, int size, char* result) {
  struct tm *resultp;
  resultp = gmtime_r(&t, resultp);
  strftime(result, size, format, resultp);
}
