import numpy as np

class Mira(object):
    def __init__(self, learning_rate=1, input_data=None, output_data=None,
                 feature_size=None):
        self.lr = learning_rate

        if feature_size is not None:
            self.dims = feature_size
        elif input_data is not None:
            self.dims = input_data.shape[1]
        else:
            raise "Mira must be able to initialize weight vector."

        self.w = np.zeros(self.dims)

        if input_data is not None and output_data is not None:
            self.train(input_data, output_data)
        
    # Batch train
    def train(self, xs, ys):
        num_features = xs.shape[0]
        if not num_features == ys.shape[0]:
            raise "All data must be labeled."

        error = ys - self.predict(xs)
        self.w += self.lr * np.dot(error,xs)

    def predict(self, xs):
        output = np.dot(xs, self.w)
        return np.vectorize(self.f)(output)


    # Indicator of if perceptron corectly classified output
    def f(self,x):
        return 1 if x >= 0 else -1

    
    def predict_single(self, x):
        output = np.inner(x, self.w)
        return self.f(output)

    # Train online
    def train_online(self, x, y):
        if self.predict_single(x) * y <= 0:
            self.w += self.lr * y * x


    def naive_average_train(self, xs, ys):
        c = 0
        wp = np.zeros(self.dims)

        while(self.test(xs,ys) != 1): 
            c += 1
            for x,y in zip(xs,ys):
                if self.predict_single(x)*y <= 0:
                    self.w += self.lr * y * x
                wp += self.w
        self.w = wp / c

    def average_train(self, xs, ys):
        c = 0
        wa = np.zeros(self.dims)
        
        while(self.test(xs,ys) != 1):
            c += 1
            for x, y in zip(xs,ys):
                if self.predict_single(x)*y <= 0:
                    self.w += self.lr * y * x
                    wa += c * self.lr * y * x
        self.w -= wa / c
        
    # Returns the classification percentage on the output data
    def test(self, input_data, output_data):
        if len(output_data) == 0:
            return 1

        return np.sum(self.predict(input_data) == output_data) / len(output_data)
        
            
    def safe_norm(self):
        norm = np.linalg.norm(self.w)
        if norm == 0:
            return
        self.w /= norm

    def reset(self):
        self.w = np.zeros(self.dims)
