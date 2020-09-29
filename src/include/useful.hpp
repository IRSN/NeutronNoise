#ifndef __NEUTRON_MULTIPLICITY_COUNTING_USEFUL__
#define __NEUTRON_MULTIPLICITY_COUNTING_USEFUL__

#include <Rcpp.h>
#include <RcppThread.h>
#include <vector>
#include <fmt/core.h>

// [[Rcpp::plugins(cpp17)]]

template <typename... Args>
void stop(const std::string &message, Args &&... args)
{
  Rcpp::stop(fmt::format(message, std::forward<Args>(args)...));
}


template <typename... Args>
void stop_if(bool test, const std::string &message, Args &&... args)
{
  if(test)
    Rcpp::stop(fmt::format(message, std::forward<Args>(args)...));
}

template <typename... Args>
void warning(const std::string &message, Args &&... args)
{
  Rcpp::warning(fmt::format(message, std::forward<Args>(args)...));
}


template <typename... Args>
void warning_if(bool test, const std::string &message, Args &&... args)
{
  if(test)
    Rcpp::warning(fmt::format(message, std::forward<Args>(args)...));
}

/*
//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------
template <typename X>
inline bool is_strictly_increasing ( X & x) {
  auto i = x.begin() + 1;
  auto j = x.end();
  while (i != j && *i > *(i-1))
    ++i;
  return (!(i != j)) ;
}
*/

//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------
template <typename X>
inline bool is_sorted(const X & x) {
  auto i = x.cbegin() + 1;
  auto j = x.cend();
  while (i != j && *i >= *(i-1))
    ++i;
  return (!(i != j)) ;
}



/*

//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------
inline bool is_of_class (const Rcpp::RObject & x, std::string s) {

  if (x.hasAttribute ("class") == false)
    return false;

  Rcpp::CharacterVector a = Rcpp::CharacterVector::create (s.c_str());
  Rcpp::CharacterVector b = Rcpp::wrap (x.attr ("class"));
  return Rcpp::is_true(Rcpp::any (Rcpp::in (a, b)));
}
*/

//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------
inline void check_user_interrupt(int step)
{
  static int i = 0;
  if (++ i % step == 0)
    Rcpp::checkUserInterrupt();
}

/*
//-------------------------------------------------------------------------------------------------
//
//-------------------------------------------------------------------------------------------------
inline void check_user_interrupt_thread(int step)
{
  static int i_check_user_interrupt_thread = 0;

  if(++i_check_user_interrupt_thread % step == 0)
    RcppThread::checkUserInterrupt();
}

*/


#endif
