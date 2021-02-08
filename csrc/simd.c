#include <stdio.h>
#include <immintrin.h>
#include <x86intrin.h>
#include <strings.h>

int _elm_cmp_vec(char elem, char *src){
  __m256i _list = _mm256_loadu_si256((__m256i*)src);
  __m256i _lookup = _mm256_set1_epi8(elem);
  return _mm256_movemask_epi8(_mm256_cmpeq_epi8(_list, _lookup));
}

int _load_movemask(char *src) {
  return _mm256_movemask_epi8(_mm256_loadu_si256((__m256i*)src));
}


int _elm_add_movemask(char elem, char *src){
  __m256i _list = _mm256_loadu_si256((__m256i*)src);
  __m256i _lookup = _mm256_set1_epi8(elem);
  return _mm256_movemask_epi8(_mm256_xor_si256(_list, _lookup));
}
