
#ifndef INTEGER_HIST
#define INTEGER_HIST

#include <vector>

class IntegerHist
{
  std::vector<unsigned long long int> data;
  unsigned long long int i = 0;
  unsigned int nb_commit = 0;
  
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
    nb_commit ++;
  }
  
  const auto & get_data()
  {
    return data;
  }
  
  const unsigned int get_nb_commit()
  {
    return nb_commit;
  }
};


#endif
