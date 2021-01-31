#include <stdio.h>
#include <immintrin.h>
#include <x86intrin.h>



int _elm_cmp_vec(char elem, char src[32]){

  /* src1 = (char *)malloc(sizeof(char) * 32); */
  /* for (int i=0; i<32; i++) { */
  /*   	src1[i] = i; */
  /* } */

  /* printf("%d\n", *src1); */
  __m256i _list = _mm256_loadu_si256((__m256i*)src);
  // free(src1);
  __m256i _lookup = _mm256_set1_epi8(elem);
  __m256i _dst = _mm256_cmpeq_epi8(_list, _lookup);

  /* char *dst; */
  /* dst = (char *)malloc(sizeof(char) * 32); */
  /* _mm256_storeu_si256((__m256i*)dst, _dst); */

  /* for (int i=0; i<32; i++) { */
  /*   printf("%x  ", dst[i]); */
  /* } */

  int dst = _mm256_movemask_epi8(_dst);

  //printBits(sizeof(int), &dst);

  printf("debug\n");
  return dst;
}
