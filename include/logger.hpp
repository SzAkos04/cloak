#pragma once

#include <string>

extern bool useColor;
extern bool silent;

enum class LogLevel {
    Fatal,
    Error,
    Warning,
    Info,
    Success,
    Debug,
    FriendlyError,
};

class Logger {
  public:
    static Logger &get();

    void log(LogLevel level, const std::string &msg,
             const char *filename = nullptr, int line = 0);

  private:
    Logger() = default;
    ~Logger() = default;

    std::string getPrefix(LogLevel level) const;
    std::string getColor(LogLevel level) const;
    std::string timestamp() const;
};

#define LOG_FATAL(msg)                                                         \
    Logger::get().log(LogLevel::Fatal, (msg), __FILE__, __LINE__)
#define LOG_ERROR(msg)                                                         \
    Logger::get().log(LogLevel::Error, (msg), __FILE__, __LINE__)
#define LOG_WARN(msg)                                                          \
    Logger::get().log(LogLevel::Warning, (msg), __FILE__, __LINE__)
#define LOG_INFO(msg)                                                          \
    Logger::get().log(LogLevel::Info, (msg), __FILE__, __LINE__)
#define LOG_SUCCESS(msg)                                                       \
    Logger::get().log(LogLevel::Success, (msg), __FILE__, __LINE__)
#ifdef DEBUG
#define LOG_DEBUG(msg)                                                         \
    Logger::get().log(LogLevel::Debug, (msg), __FILE__, __LINE__)
#else
#define LOG_DEBUG(msg)                                                         \
    do {                                                                       \
    } while (0)
#endif
#define LOG_FRIENDLY_ERROR(msg)                                                \
    Logger::get().log(LogLevel::FriendlyError, (msg))
