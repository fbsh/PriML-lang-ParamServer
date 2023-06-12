#include "../headers/hofs.h"

__device__
int identity_int(int x){
  return x;
}
__device__ tabulate_fun_int identity_int_dev = identity_int;

extern "C"
void* gen_identity_int(){
  tabulate_fun_int local;
  cudaMemcpyFromSymbol(&local, identity_int_dev, sizeof(tabulate_fun_int));
  return (void*)local;
}

__device__ 
int double_int(int x){
  return 2*x;
}
__device__ tabulate_fun_int double_int_dev = double_int;
extern "C"
void* gen_double_int(){
  tabulate_fun_int local;
  cudaMemcpyFromSymbol(&local, double_int_dev, sizeof(tabulate_fun_int));
  return (void*)local;
}

__device__
int paren_gen(int i){
  if(i % 2 == 0){
    return 1;
  }
  else{
    return -1;
  }
}
__device__ tabulate_fun_int paren_gen_int_dev = paren_gen;
extern "C"
void* gen_paren_gen(){
  tabulate_fun_int local;
  cudaMemcpyFromSymbol(&local, paren_gen_int_dev, sizeof(tabulate_fun_int));
  return (void*)local;
}
