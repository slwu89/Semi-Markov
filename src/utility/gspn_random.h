#ifndef _GSPN_RANDOM_H_
#define _GSPN_RANDOM_H_ 1

#include <random>
#include <mutex>
#include "boost/random/uniform_01.hpp"
#include "boost/random/uniform_int_distribution.hpp"

/*! These routines wrap uniform generators for our code.
 *  Options include std::random or boost::random.
 *  We may want to use a lagged Fibonacci for parallel work.
 */


namespace afidd
{
namespace smv
{


template<typename BoostRNG>
struct BoostGenerator
{
  typedef BoostRNG type;
  BoostRNG rng;
  BoostGenerator(BoostRNG rng) : rng(rng) {}
};



template<typename StdRNG>
struct StandardGenerator
{
  typedef StdRNG type;
  StdRNG rng;
  StandardGenerator(StdRNG rng) : rng(rng) {}
};



namespace detail
{

template<typename IndexType, typename RNG>
struct RngToUniform
{
  RNG& _rng;
  RngToUniform(RNG& rng) : _rng(rng) {}
  IndexType operator()(IndexType cnt)
  {
    return _rng()%cnt;
  }
};


template<typename IndexType, typename RNG>
struct RngToUniform<IndexType,BoostGenerator<RNG>>
{
  RNG& _rng;
  RngToUniform(BoostGenerator<RNG>& rng) : _rng(rng.rng) {}
  IndexType operator()(IndexType cnt)
  {
    boost::random::uniform_int_distribution<IndexType> gen_idx(0, cnt-1);
    return gen_idx(_rng);
  }
};



template<typename IndexType, typename RNG>
struct RngToUniform<IndexType,StandardGenerator<RNG>>
{
  RNG& _rng;
  RngToUniform(StandardGenerator<RNG>& rng) : _rng(rng.rng) {}
  IndexType operator()(IndexType cnt)
  {
    std::uniform_int_distribution<IndexType> gen_idx(0, cnt-1);
    return gen_idx(_rng);
  }
};

} // end namespace detail
	



template<typename RandGen>
class ParallelCacheGeneratorSource
{
  RandGen _gen;
  std::mutex _one_reader;

 public:
  typedef unsigned long result_type;
  void write(std::vector<result_type>& cache)
  {
    std::lock_guard<std::mutex> guard{_one_reader};
    for (auto& val : cache)
    {
      val=_gen();
    }
  }
};




template<typename RandGen>
class ParallelCacheGenerator
{
  RandGen& _gen;
  using result_type=typename RandGen::result_type;
  std::vector<result_type> _cache;
  typename std::vector<result_type>::const_iterator _cur;

 public:
 ParallelCacheGenerator(RandGen& gen, size_t capacity=1000)
   : _gen(gen), _cache(capacity), _cur(_cache.begin()) {}
  result_type operator()() {
    if (_cur==_cache.end())
      {
	_gen.write(_cache);
	_cur=_cache.begin();
      }
    auto res=*_cur;
    ++_cur;
    return res;
  }
};



template<typename RNG>
double uniform(RNG& rng)
{
  std::uniform_real_distribution<double> dist(0.0, 1.0);
  return dist(rng.rng);
}


template<typename RNG>
double uniform(BoostGenerator<RNG>& rng)
{
  return boost::random::uniform_01<double>()(rng.rng);
}


template<typename RNG>
size_t uniform_index(RNG& rng, size_t cnt)
{
  std::uniform_int_distribution<size_t> gen_idx(0, cnt-1);
  return gen_idx(rng.rng);
}


template<typename RNG>
size_t uniform_index(BoostGenerator<RNG>& rng, size_t cnt)
{
  boost::random::uniform_int_distribution<size_t> gen_idx(0, cnt-1);
  return gen_idx(rng.rng);
}


template<typename RandomAccessIterator, typename RNG>
void random_shuffle(RandomAccessIterator first, RandomAccessIterator last,
    RNG& rng)
{
  using Index=
    typename std::iterator_traits<RandomAccessIterator>::difference_type;
  detail::RngToUniform<Index,RNG> shuffle_gen(rng);
  std::random_shuffle(first, last, shuffle_gen);
}

} // end namespace smv
} // end namespace afidd


#endif // _GSPN_RANDOM_H_
