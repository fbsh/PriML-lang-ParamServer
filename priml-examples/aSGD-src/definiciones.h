//2darray:
typedef struct{
int m, n;
#define rows(array) ((array)->m)
#define columns(array) ((array)->n)
double *arr;
#define values(array) ((array)->arr)
#define value(array,i,j) ((array)->arr[j*rows(array)+i]) //column major
}array_2d;
typedef array_2d *array_2d_T;

//array1d
typedef struct{
int n;
#define rows_vector(array) ((array)->n)
double *arr;
int *arr_int;
#define values_vector(array) ((array)->arr)
#define value_vector(array,i) ((array)->arr[i])
#define values_vector_integer(array) ((array)->arr_int)
#define value_vector_integer(array,i) ((array)->arr_int[i])
}array_1d;
typedef array_1d *array_1d_T;

//functions
void print_vector(array_1d_T);
void print_matrix(array_2d_T);
void fill_matrix(array_2d_T, char *);
void fill_vector(array_1d_T, char *);
void print_vector_integer(array_1d_T);
void fill_batch(array_2d_T, array_2d_T, array_1d_T, array_1d_T);
void fill_zeros(array_1d_T,int);