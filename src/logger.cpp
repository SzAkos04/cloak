#include "logger.hpp"

#include <chrono>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

Logger &Logger::get() {
    static Logger instance;
    return instance;
}

std::string Logger::timestamp() const {
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);
    std::tm tm{};
#ifdef _WIN32
    localtime_s(&tm, &t);
#else
    localtime_r(&t, &tm);
#endif
    std::ostringstream oss;
    oss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
    return oss.str();
}

std::string Logger::getPrefix(LogLevel level) const {
    switch (level) {
    case LogLevel::Fatal:
        return "[fatal]";
    case LogLevel::Error:
    case LogLevel::FriendlyError:
        return "[error]";
    case LogLevel::Warning:
        return "[warning]";
    case LogLevel::Info:
        return "[info]";
    case LogLevel::Success:
        return "[success]";
    case LogLevel::Debug:
        return "[debug]";
    }
    return "[log]";
}

std::string Logger::getColor(LogLevel level) const {
    if (!useColor) {
        return "";
    }

    switch (level) {
    case LogLevel::Fatal:
    case LogLevel::Error:
    case LogLevel::FriendlyError:
        return "\033[1;31m"; // bold red
    case LogLevel::Warning:
        return "\033[1;33m"; // bold yellow
    case LogLevel::Info:
        return "\033[1m"; // bold white
    case LogLevel::Success:
        return "\033[1;32m"; // bold green
    case LogLevel::Debug:
        return "\033[1;36m"; // bold cyan
    }
    return "";
}

void Logger::log(LogLevel level, const std::string &msg, const char *file,
                 int line) {
    if (silent && level != LogLevel::Error && level != LogLevel::Fatal) {
        return;
    }

    std::ostringstream out;
    if (level == LogLevel::FriendlyError) {
        out << getColor(level) << getPrefix(level) << "\033[0m"
            << ": " << msg << std::endl;
    } else {
        out << timestamp() << " " << getColor(level) << getPrefix(level)
            << "\033[0m";

        if (file && line > 0) {
            out << " [" << file << ":" << line << "]";
        }

        out << ": " << msg << std::endl;
    }

    // Output to console
    if (level == LogLevel::Fatal || level == LogLevel::Error ||
        level == LogLevel::FriendlyError) {
        std::cerr << out.str();
    } else {
        std::cout << out.str();
    }

    if (level == LogLevel::Fatal) {
        std::exit(1);
    }
}
