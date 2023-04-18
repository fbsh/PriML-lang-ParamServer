#include<stdio.h>
#include<stdlib.h>
#include <string.h>
#include"definitions.h"
static int i,j;
void fill_matrix(array_2d_T p, char *s){
	int m = rows(p);
	int n = columns(p);
	FILE * pFile;
  	pFile = fopen (s,"r");
  	for(i=0;i<m;i++)
		for(j=0;j<n;j++)
			fscanf(pFile,"%lf", &value(p,i,j));
	fclose(pFile);
}
void fill_vector(array_1d_T p, char *s){
	int m = rows_vector(p);
	FILE * pFile;
  	pFile = fopen (s,"r");
  	for(i=0;i<m;i++)
			fscanf(pFile,"%lf", &value_vector(p,i));
	fclose(pFile);
}

void fill_batch(array_2d_T b, array_2d_T X, array_1d_T y_b, array_1d_T y){
	int m_b = rows(b);
	int m = rows(X);
	int n = columns(b);
	int r;
	for(i=0; i<m_b; i++){
		r = rand() % m;
		value_vector(y_b,i)=value_vector(y, r);
		for(j=0; j<n; j++){
			value(b,i,j) = value(X,r,j);
		}
	}
}

void fill_zeros(array_1d_T a, int cols){
	for(i=0; i<cols; i++){
		value_vector(a,i)=0;
		}
}


void print_matrix(array_2d_T p){
	int m = rows(p);
	int n = columns(p);
		for(i=0;i<m;i++){
			for(j=0;j<n;j++){
				if(j<n-1)
				printf("matrix[%d][%d]=%.5f\t",i,j,value(p,i,j));
				else
				printf("matrix[%d][%d]=%.5f\n",i,j,value(p,i,j));
			}
		}
}
void print_vector_integer(array_1d_T p){
	int m = rows_vector(p);
		for(i=0;i<m;i++)
				printf("vector[%d]=%d\n",i,value_vector_integer(p,i));
}
void print_vector(array_1d_T p){
	int m = rows_vector(p);
			for(i=0;i<m;i++)
				printf("vector[%d]=%.5f\n",i,value_vector(p,i));
}