#include <Rcpp.h>
#include <fmt/core.h>

//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------

class Log
{
private:
  int level_;

  Log (): level_(0) { }

  void set_level (int level) {
    level_ = level;
  }

  static Log & get_instance () {
    static Log log;
    return log;
  }

public:

  Log & operator = (const Log &) = delete;

  Log (const Log &) = delete;

  static void set_threshold (int level) {
    get_instance().set_level(level);
  }

  template <typename... Args>
  static void for_level(int message_level, const std::string &message, Args &&... args)
  {
    if (get_instance().level_ >= message_level)
      Rcpp::Rcout<<fmt::format(message, std::forward<Args>(args)...)<<std::endl;
  }

};


/*class Log {

private:
  std::ostream& _out_stream;
  int _level;
  int _message_level;
  //static Log * _instance;

private:
  Log (int level = 0, std::ostream& stream = Rcpp::Rcout): _out_stream(stream), _level(level) { }

  void set_level (int level) {
    _level = level;
  }

  void set_message_level (int message_level) {
    _message_level = message_level;
  }

  static Log & get_instance () {
    static Log log;
    return log;
  }

public:

  Log & operator = (const Log &) = delete;

  Log (const Log &) = delete;

  static void set_threshold (int level) {
    get_instance().set_level(level);
  }

  //Implicit conversion to std::ostream
  // operator std::ostream() {
  //   return _out_stream;
  // }

  static Log & for_level (int message_level) {
    get_instance().set_message_level(message_level);
    return(get_instance());
  }

  template<typename T>
  Log & operator << (const T& data) {
    if (_level >= _message_level)
      _out_stream << data;
    return(*this);
  }

  Log & operator << ( std::ostream& (*f)(std::ostream&) ) {
    if (_level >= _message_level)
      f(_out_stream);
    return(*this);
  }
};
*/



/*** R
*/
