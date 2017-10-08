from algs.perceptron import Perceptron

import numpy as np

xs = np.array([[1,-1,-1], [1,1,1], [-1,1,-1], [-1,-1,1]])
ys = np.array([-1, 1, -1, 1])

def main():
    perceptron = Perceptron(feature_size=3)

    print ("Before training:")
    print(perceptron.test(xs, ys))
    perceptron.train(xs,ys)

    print("After (batch) training:")
    print(perceptron.test(xs, ys))

    perceptron.reset()

    for j in range(1):
        for i in range(4):
            perceptron.train_online(xs[i], ys[i])

    print ("After single training: ")
    print(perceptron.test(xs,ys))

    print ("Average")
    perceptron.reset()
    perceptron.average_train(xs, ys)
    print(perceptron.test(xs,ys))
    print ("Naive average (with maximum iterations)")
    perceptron.reset()
    perceptron.naive_average_train(xs, ys, maxIter=10)
    print(perceptron.test(xs,ys))

    print ("MIRA")
    mira = Perceptron(feature_size=3, mira_aggro=0.0)
    for j in range(10):
        for i in range(4):
            mira.train_mira(xs[i], ys[i])

    print(mira.test(xs,ys))


    print ("MIRA Average")
    mira.reset()
    mira.train_mira_average(xs,ys)
    print(mira.test(xs,ys))
    
if __name__ == "__main__":
    main()
