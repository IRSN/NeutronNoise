
#ifndef INTEGER_HIST
#define INTEGER_HIST

#include <vector>

class IntegerHist
{
  std::vector<unsigned long long int> data;
  unsigned long long int i = 0;
  
public:
  
  void operator () ()
  {
    i ++;
  }
  
  void commit()
  {
    if(data.size() <= i)
      data.resize(i + 1, 0);
    data[i]++;
    i = 0;
  }
  
  const auto & get_data()
  {
    return data;
  }
};


#endif
