#include "logger.hpp"

bool silent = false;
bool useColor = true;

int main() {
    LOG_FRIENDLY_ERROR("Hello, World!");
    return 0;
}
