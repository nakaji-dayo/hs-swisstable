#include <stdio.h>
#include <immintrin.h>
#include <x86intrin.h>
#include <strings.h>

void printBits(size_t const size, void const * const ptr);

int _elm_cmp_vec(char elem, char *src){

  /* printf("src: "); */
  /* for (int i=0; i<32; i++) { */
  /*     printf("%d ", src[i]); */
  /* } */
  /* puts(""); */

  __m256i _list = _mm256_loadu_si256((__m256i*)src);
  __m256i _lookup = _mm256_set1_epi8(elem);
  __m256i _dst = _mm256_cmpeq_epi8(_list, _lookup);

  int dst = _mm256_movemask_epi8(_dst);

  /* printf("dist: "); */
  /* printBits(sizeof(int), &dst); */

  return dst;
}


void printBits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i = size-1; i >= 0; i--) {
        for (j = 7; j >= 0; j--) {
            byte = (b[i] >> j) & 1;
            printf("%u", byte);
        }
    }
    puts("");
}
