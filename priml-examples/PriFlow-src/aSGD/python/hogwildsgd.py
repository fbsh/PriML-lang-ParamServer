import numpy as np
from sklearn.preprocessing import StandardScaler
import sklearn
from sklearn.linear_model import SGDRegressor
from joblib import Parallel, delayed

from shared import SharedWeights, mse_gradient_step, mse
from generators import DataGenerator
from autograd import grad


class HogWildRegressor(SGDRegressor):

    losses = {
        'squared_loss' : mse_gradient_step  
        # 'squared_loss' : mse
        
    }

    def __init__(self, 
                 n_jobs = 4, 
                 n_epochs = 5,
                 batch_size = 32, 
                 chunk_size = 32,
                 lr = .01,
                 generator=None,
                 **kwargs):
        self.shared_weights = None # initialize as None here

        super(HogWildRegressor, self).__init__(**kwargs)

        if self.loss not in self.losses:
            raise Exception(f"Loss '{self.loss}' not supported")


        self.batch_size = batch_size
        self.lr = lr
        self.gradient = self.losses.get(self.loss)
        # self.gradient = grad(self.losses.get(self.loss))
        self.lr = lr
        self.n_jobs = n_jobs
        self.n_epochs = n_epochs
        self.chunk_size = chunk_size

        if not generator:
            self.generator = DataGenerator(shuffle= self.shuffle,
                                           chunk_size = self.chunk_size,
                                           verbose = self.verbose)

    def _fit(self, X, y, alpha, C, loss=None, learning_rate=None, coef_init=None, intercept_init=None, sample_weight=None):
        n_samples, n_features = X.shape

        if self.shared_weights is None:
            self.shared_weights = SharedWeights(n_features) # initialize here with the right size

        Parallel(n_jobs= self.n_jobs, verbose=self.verbose)\
            (delayed(self.train_epoch)(i, X, y) for i in range(self.n_epochs))

        # self.coef_ = self.shared_weights.w.reshape((10,1)).T
        self.coef_ = self.shared_weights.w.reshape((n_features, 1)).T

        self.fitted = True
        self.intercept_ = 0.
        self.t_ = 0.

        return self

    def train_epoch(self, epoch_index, X, y):
        self._train_epoch(X,y)

    def _train_epoch(self, X, y):
        batch_size = self.batch_size
        for k in range(int(X.shape[0]/float(batch_size))):
            Xx = X[k*batch_size : (k+1)*batch_size,:]
            yy = y[k*batch_size : (k+1)*batch_size]
            self.gradient(Xx, yy, self.lr, self.shared_weights.w)
            
            # Xx = X[k*batch_size : (k+1)*batch_size,:]
            # yy = y[k*batch_size : (k+1)*batch_size]
            # grad_values = self.gradient(self.shared_weights.w, Xx, yy)
            # self.shared_weights.w -= self.lr * grad_values


    def get_SGDRegressor(self):
        sr = SGDRegressor(fit_intercept = False)
        sr.coef_ = self.coef_
        sr.intercept_ = 0.
        self.t_ = 0
        return sr




