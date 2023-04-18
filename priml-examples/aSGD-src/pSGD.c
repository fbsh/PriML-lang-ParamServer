/*

Stochastic Gradient Descent, ADAM, AMSGrad
You may change among these optimizers using the 'optimizator' parameter, where:
- SGD - optimizer = 1
- ADAM - optimizer = 2
- AMSGrad - optimizer = 3

*/

#define MAX(X, Y) (((X) >= (Y)) ? (X) : (Y))
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include"./definiciones.h"
// Import CUDA and cuBLAS libraries
# include <cuda_runtime.h>
# include "cublas_v2.h"
// Data input
#define X_matrix "X_ent.txt" //size MxN
#define X_v_matrix "X_valida.txt" //size MxN
#define b_vector "b_bh.txt" //size Nx1
#define y_vector "y_train.txt" //size Mx1
#define y_v_vector "y_val.txt" //size Mx1

// Function for descent
double sgd(int M, int N, int M_v, int batch_size, int iter, double lr, double tolerancia_gradiente,FILE *f, int optimizador, double beta_adam_1, double beta_adam_2, double epsilon) {
  // Declare required objects for CUDA
  cudaError_t cudastat ;
  // cudaMalloc status
  cublasStatus_t stat ;
  // CUBLAS functions status
  cublasHandle_t handle ;
  // Data structures
  arreglo_2d_T X, X_v, batch;
  arreglo_1d_T y, y_v, b, g, y_b, mt, vt, tmp1, tmp2, y_static, y_v_static, rmse, rmse_v, vt_hat, tiempo;
  // Data pointers for GPU
  double *d_batch, *d_X, *d_X_v, *d_y, *d_y_v, *d_b, *d_g, *d_y_b, *d_mt, *d_vt, *d_tmp1, *d_tmp2;

	int incx=1;
  double ALPHA, BETA;

  double beta_inv_1 = 1-beta_adam_1;
  double beta_inv_2 = 1-beta_adam_2;

// Reserve required space
  X=malloc(sizeof(*X));
  X_v=malloc(sizeof(*X_v));
  y=malloc(sizeof(*y));
  y_v=malloc(sizeof(*y_v));
  b=malloc(sizeof(*b));
  g=malloc(sizeof(*g));
  batch=malloc(sizeof(*batch));
  y_b=malloc(sizeof(*y_b));
  rmse=malloc(sizeof(*rmse));
  rmse_v=malloc(sizeof(*rmse_v));
  mt=malloc(sizeof(*mt));
  vt=malloc(sizeof(*vt));
  tmp1=malloc(sizeof(*tmp1));
  tmp2=malloc(sizeof(*tmp2));
  vt_hat=malloc(sizeof(*vt_hat));
  tiempo=malloc(sizeof(*tiempo));

  y_static=malloc(sizeof(*y_static));
  y_v_static=malloc(sizeof(*y_v_static));
// Assign data dimensions
	rows(X)=M;
	columns(X)=N;
  rows(X_v)=M_v;
	columns(X_v)=N;
  rows_vector(b)=N;
	rows_vector(y)=M;
  rows_vector(y_v)=M_v;
  rows_vector(g)=N;
  rows(batch)=batch_size;
  columns(batch)=N;
  rows_vector(y_b)=batch_size;
  rows_vector(rmse)=iter;
  rows_vector(rmse_v)=iter;
  rows_vector(mt)=N;
  rows_vector(vt)=N;
  rows_vector(tmp1)=N;
  rows_vector(tmp2)=N;
  rows_vector(vt_hat)=N;
  rows_vector(tiempo)=iter;
	rows_vector(y_static)=M;
  rows_vector(y_v_static)=M_v;

  // Declare CUDA environment
  stat = cublasCreate (& handle );

// Reserve memory in host
  values(X)=malloc(rows(X)*columns(X)*sizeof(double));
	fill_matrix(X,X_matrix);

  // Reserve memory in GPU and assign values with cuBLAS functions
  cudastat = cudaMalloc (( void **)&d_X , M * N * sizeof (*d_X)); // device
  stat = cublasSetMatrix (M ,N , sizeof (double ) ,X->arr ,M , d_X , M );

  values(X_v)=malloc(rows(X_v)*columns(X_v)*sizeof(double));
  fill_matrix(X_v,X_v_matrix);

  cudastat = cudaMalloc (( void **)&d_X_v , M_v * N * sizeof (*d_X_v)); // device
  stat = cublasSetMatrix (M_v ,N , sizeof (double ) ,X_v->arr ,M_v , d_X_v , M_v ); 
	
  values_vector(b)=malloc(N*sizeof(double));
	fill_vector(b,b_vector);

  cudastat = cudaMalloc (( void **)&d_b , N * sizeof (*d_b)); // device
  stat = cublasSetVector (N , sizeof (double ) ,b->arr ,1 , d_b ,1);   

  values_vector(y)=malloc(M*sizeof(double));
  fill_vector(y,y_vector);

  cudastat = cudaMalloc (( void **)&d_y , M * sizeof (*d_y)); // device
  stat = cublasSetVector (M , sizeof (double ) ,y->arr ,1 , d_y ,1);

  values_vector(y_v)=malloc(M_v*sizeof(double));
  fill_vector(y_v,y_v_vector);

  cudastat = cudaMalloc (( void **)&d_y_v , M_v * sizeof (*d_y_v)); // device
  stat = cublasSetVector (M_v , sizeof (double ) ,y_v->arr ,1 , d_y_v ,1); 

	values_vector(g)=malloc(N*sizeof(double));

  cudastat = cudaMalloc (( void **)&d_g , N * sizeof (*d_g )); // device
  stat = cublasSetVector (N , sizeof (double ) ,g->arr ,1 , d_g ,1); 

  values(batch)=malloc(rows(batch)*columns(batch)*sizeof(double));
  values_vector(y_b)=malloc(batch_size*sizeof(double));

  fill_batch(batch, X, y_b, y);
  cudastat = cudaMalloc (( void **)&d_batch , batch_size * N * sizeof (*d_batch)); // device
  cudastat = cudaMalloc (( void **)&d_y_b , batch_size * sizeof(*d_y_b)); // device

  values_vector(rmse)=malloc(iter*sizeof(double));
  values_vector(rmse_v)=malloc(iter*sizeof(double));

  values_vector(mt)=malloc(N*sizeof(double));
  fill_zeros(mt,N);

  cudastat = cudaMalloc (( void **)&d_mt , N * sizeof (*d_mt )); // device
  stat = cublasSetVector (N , sizeof (double ) ,mt->arr ,1 , d_mt ,1); 

  values_vector(vt)=malloc(N*sizeof(double));
  fill_zeros(vt,N);

  cudastat = cudaMalloc (( void **)&d_vt , N * sizeof (*d_vt )); // device
  stat = cublasSetVector (N , sizeof (double ) ,vt->arr ,1 , d_vt ,1);
  
  values_vector(tmp1)=malloc(N*sizeof(double));
  fill_zeros(tmp1,N);

  cudastat = cudaMalloc (( void **)&d_tmp1 , N * sizeof (*d_tmp1 )); // device
  stat = cublasSetVector (N , sizeof (double ) ,tmp1->arr ,1 , d_tmp1 ,1);

  values_vector(tmp2)=malloc(N*sizeof(double));
  fill_zeros(tmp2,N);

  cudastat = cudaMalloc (( void **)&d_tmp2 , N * sizeof (*d_tmp2 )); // device
  stat = cublasSetVector (N , sizeof (double ) ,tmp2->arr ,1 , d_tmp2 ,1);

  values_vector(y_static)=malloc(M*sizeof(double));
  values_vector(y_v_static)=malloc(M_v*sizeof(double));
  fill_vector(y_static,y_vector);
  fill_vector(y_v_static,y_v_vector);

  values_vector(vt_hat)=malloc(N*sizeof(double));
  fill_zeros(vt_hat,N);

  values_vector(tiempo)=malloc(iter*sizeof(double));
  

  srand(1882);
  double acum = 0;
  int it = 1;
  double grad_norm = 1e9;
  double neg_lr = -1.0*lr;

  clock_t begin = clock();

  // Check if batch size is as big as training data, to avoid unnecesary steps afterwards
  if(batch_size == M){
  stat = cublasSetMatrix (batch_size ,N , sizeof (double ) ,X->arr ,batch_size , d_batch , batch_size ); // cp a - > d_a
  stat = cublasSetVector (batch_size , sizeof (double ) ,y->arr ,1 , d_y_b ,1); // cp x - > d_x
  }
// Start descent iterations
// While will be broken if number of max iterations is reached, gradient's norm is smaller than limit or if MSE if greater than 10,000 (basically, it has exploded)

while(it <= iter && grad_norm > tolerancia_gradiente && acum/M <10000){ 

// Restart values
  stat = cublasSetVector (M , sizeof (double ) ,y_static->arr ,1 , d_y ,1); // cp x - > d_x
  stat = cublasSetVector (M_v , sizeof (double ) ,y_v_static->arr ,1 , d_y_v ,1); // cp x - > d_x
  if(batch_size == M){
  stat = cublasSetVector (M , sizeof (double ) ,y_static->arr ,1 , d_y_b ,1); // cp x - > d_x
  }
  
  fill_zeros(tmp1,N);
  fill_zeros(tmp2,N);

  stat = cublasSetVector (N , sizeof (double ) ,tmp1->arr ,1 , d_tmp1 ,1);
  stat = cublasSetVector (N , sizeof (double ) ,tmp2->arr ,1 , d_tmp2 ,1);

  //Restart batch only if size is smaller than train data
if(batch_size != M){
    fill_batch(batch, X, y_b, y);
    stat = cublasSetMatrix (batch_size ,N , sizeof (double ) ,batch->arr ,batch_size , d_batch , batch_size ); // cp a - > d_a
    stat = cublasSetVector (batch_size , sizeof (double ) ,y_b->arr ,1 , d_y_b ,1); // cp x - > d_x
}
// Calculate error: e = - X %*% b + y
// Error now saved in e vector
  ALPHA = -1.0;
  BETA = 1.0;
  //batch
  stat=cublasDgemv(handle,CUBLAS_OP_N,batch_size,N,&ALPHA,d_batch,batch_size,d_b,1,&BETA,d_y_b,1);
  //train
  stat=cublasDgemv(handle,CUBLAS_OP_N,M,N,&ALPHA,d_X,M,d_b,1,&BETA,d_y,1);
  // test
  stat=cublasDgemv(handle,CUBLAS_OP_N,M_v,N,&ALPHA,d_X_v,M_v,d_b,1,&BETA,d_y_v,1);
// Calculate train and test MSE
  // Copy error vectors form GPU to CPU
  stat = cublasGetVector (M , sizeof (double ) , d_y ,1 ,y->arr ,1);
  stat = cublasGetVector (M_v , sizeof (double ) , d_y_v ,1 ,y_v->arr ,1);
  acum = 0;
  for(int i = 0; i < M; i++){
    acum += pow(values_vector(y)[i],2);
  }
  double acum_v = 0;
  for(int i = 0; i < M_v; i++){
    acum_v += pow(values_vector(y_v)[i],2);
  }
  entrada_vector(rmse,it) = acum/M;
  entrada_vector(rmse_v,it) = acum_v/M_v;
  printf("Iteration %d/%d RSS train: %lf -- RSS val: %lf \n", it, iter, acum/M, acum_v/M_v);

// Calculate gradient : g = -X^t %*% e
  ALPHA = -1.0;
  BETA = 0.0;
  stat=cublasDgemv(handle,CUBLAS_OP_T,batch_size,N,&ALPHA,d_batch,batch_size,d_y_b,1,&BETA,d_g,1);
  double inv_batch = 1.0/batch_size;
  stat=cublasDscal(handle, N, &inv_batch, d_g, 1);
  // now vector g has gradient value
  //Calculate gradient norm:
  stat=cublasDnrm2(handle,N,d_g,1,&grad_norm);
  printf("Norma del gradiente: %f\n", grad_norm);

  // If optimizer is SGD
  if(optimizador == 1){
    //Update coefficients: b = b - lr * g
    neg_lr = -1.0*lr;
    stat=cublasDaxpy(handle,N,&neg_lr,d_g,1,d_b,1);
    // If optimizer is ADAM or AMSGrad
   }else if(optimizador == 2 | optimizador == 3){
    // Calculate mt = B1*mt-1 + (1-B1)*gt
            // Calculate first part B1*mt-1
        stat = cublasDaxpy(handle, N, &beta_adam_1, d_mt, 1, d_tmp1, 1);
            // Calculate second part (1-Bt)*gt
        stat = cublasDaxpy(handle, N, &beta_inv_1, d_g, 1, d_tmp1, 1); 
        // mt is in tmp 1
        stat = cublasDcopy(handle, N, d_tmp1, 1, d_mt, 1);
        // Calculate vt = B2*vt-1 + (1-B2)*gt^2
            // First part (B2*vt-1) 
        stat = cublasDaxpy(handle, N, &beta_adam_2,d_vt, 1, d_tmp2, 1);
            // Save gt^2 in auxiliary vector
        stat = cublasGetVector (N , sizeof (double ) , d_g ,1 ,g->arr ,1);
        for(int i =0; i<N; i++){
            entrada_vector(tmp1,i) = entrada_vector(g,i)*entrada_vector(g,i);
        }
        stat = cublasSetVector (N , sizeof (double ) ,tmp1->arr ,1 , d_tmp1 ,1);
        // Calculate second part
        stat = cublasDaxpy(handle, N, &beta_inv_2,d_tmp1, 1, d_tmp2, 1);

        stat = cublasDcopy(handle, N, d_tmp2, 1, d_vt, 1);
        // Update coefficients bt+1 = bt - lr*mt/(sqrt(vt_hat)+epsilon)
        // Consider adjustments for  vt_hat = vt/(1-B1^t) y mt_hat = mt/(1-B2^t)
        stat = cublasGetVector (N , sizeof (double ) , d_mt ,1 ,mt->arr ,1);
        stat = cublasGetVector (N , sizeof (double ) , d_vt ,1 ,vt->arr ,1);
          // If optimizer is AMSgrad
         if(optimizador ==2){
          for(int i =0; i< N; i++){
            double mt_gorro = entrada_vector(mt,i)/(1.0-pow(beta_adam_1,it));
            double vt_gorro = entrada_vector(vt,i)/(1.0-pow(beta_adam_2,it));
            entrada_vector(b,i) = entrada_vector(b,i) - lr*mt_gorro/(sqrt(vt_gorro)+epsilon);
           }  
        }
        else{
          for(int i=0; i<N; i++){
            entrada_vector(vt_hat,i) = MAX(entrada_vector(vt,i), entrada_vector(vt_hat,i));
          }
          for(int i =0; i< N; i++){
            entrada_vector(b,i) = entrada_vector(b,i) - lr*entrada_vector(mt,i)/(sqrt(entrada_vector(vt_hat,i))+epsilon);
          }
        }
        stat = cublasSetVector (N , sizeof (double ) ,b->arr ,1 , d_b ,1);
      }


  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  entrada_vector(tiempo, it) = time_spent;
  printf("Tiempo de ejecuciÃ³n de la iteraciÃ³n: %f\n", time_spent);
  printf("------------\n");
  fprintf(f, "%d,%d, %f, %f, %f ,%f, %f\n", it, iter, acum/M, acum_v/M_v,grad_norm, lr, time_spent);
  it++;
  // Add decay to iterations
  lr = lr * 0.999;
  
}
printf("----- Fin con learning rate de %lf -----\n", lr);

// Free memory up
  cudaFree(d_X);
  cudaFree(d_X_v);
  cudaFree(d_b);
  cudaFree(d_y);
  cudaFree(d_y_v);
  cudaFree(d_g);
  cudaFree(d_batch);
  cudaFree(d_y_b);
  cudaFree(d_mt);
  cudaFree(d_vt);
  cudaFree(d_tmp1); 
  cudaFree(d_tmp2);
  cublasDestroy ( handle );
  
  free(values(X));
	free(X);
	free(values_vector(b));
	free(b);
  free(values_vector(y));
  free(y);
  free(values_vector(g));
  free(g);
  free(values(X_v));
  free(X_v);
  free(values_vector(y_v));
  free(y_v);
  free(values(batch));
  free(batch);
  free(values_vector(y_b));
  free(y_b);
  free(values_vector(tmp1));
  free(tmp1);
  free(values_vector(tmp2));
  free(tmp2);
  free(values_vector(mt));
  free(mt);
  free(values_vector(vt));
  free(vt);
  free(values_vector(vt_hat));
  free(vt_hat);
  free(values_vector(tiempo));
  free(tiempo);
  free(values_vector(rmse));
  free(rmse);
  free(values_vector(rmse_v));
  free(rmse_v);

  free(values_vector(y_static));
  free(y_static);
  free(values_vector(y_v_static));
  free(y_v_static);
  
	return acum/M;

}


int main(int argc, char const *argv[]) {

  //Obtain data dimensions
  int M=atoi(argv[1]);
  int N=atoi(argv[2]);
  int M_v=atoi(argv[3]);
  // Read batch size
  int batch_size=atoi(argv[4]);
  // Read max iterations
  int iter=atoi(argv[5]);
  

  // Suggested values for grid search, to execute grid, uncomment the following two lines
  //double lrs[10] = {0.000000001, 0.00000001, 0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1.0};
  //double loss[10] = {0};
  // Assign a specific learning rate. If grid is used, comment following two lines
  double lrs[1] = {0.003};
  double loss[1] = {0};

  double tolerancia_gradiente = 1e-12;
  int optimizador=atoi(argv[6]);
  double beta_adam_1=atof(argv[7]);
  double beta_adam_2=atof(argv[8]);
  double epsilon=atof(argv[9]);
  // Guardar csv
  FILE *f = fopen("error_sgd_sec.csv", "w");
  FILE *f2 = fopen("error_sgd_sec_summary.csv", "w");
    
  fprintf(f2, "learning_rate, loss\n");
  fprintf(f, "iteracion, tot_iter, rss_train, rss_val, grad_norm, lr, tiempo\n");
  for(int i = 0; i<sizeof(lrs)/sizeof(lrs[0]); i++){
    loss[i] = sgd(M, N, M_v, batch_size, iter, lrs[i],tolerancia_gradiente,f, optimizador, beta_adam_1, beta_adam_2, epsilon);
  }

  for(int i = 0; i<sizeof(lrs)/sizeof(lrs[0]); i++){
    fprintf(f2, "%lf, %lf \n", lrs[i], loss[i]);
  }
  fclose(f);
  fclose(f2);
  return 0;
}