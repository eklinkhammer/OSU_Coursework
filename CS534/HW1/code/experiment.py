from perceptron import Perceptron
from data_reader import get_binary_features, get_numbered_features
from data_reader import get_numbered_binary_features, get_binned_features
from data_reader import get_num_ed_features

import numpy as np

xs = np.array([[1,-1,-1], [1,1,1], [-1,1,-1], [-1,-1,1]])
ys = np.array([-1, 1, -1, 1])

def main():
    
    data_set = "../income-data/income.train.txt"
    X, Y, features = get_binary_features(data_set)
    print X.shape

    data_set = "../income-data/income.dev.txt"
    X_dev, Y_dev, features = get_binary_features(data_set, features)
    print X_dev.shape

    # data_set = "../income-data/income.train.txt"
    # X, Y, features = get_numbered_features(data_set)
    # print X.shape

    # data_set = "../income-data/income.dev.txt"
    # X_dev, Y_dev, features = get_numbered_features(data_set, features)
    # print X_dev.shape

    # data_set = "../income-data/income.train.txt"
    # X, Y, features = get_numbered_binary_features(data_set)
    # print X.shape

    # data_set = "../income-data/income.dev.txt"
    # X_dev, Y_dev, features = get_numbered_binary_features(data_set, features)
    # print X_dev.shape

    # data_set = "../income-data/income.train.txt"
    # X, Y, features = get_binned_features(data_set)
    # print X.shape

    # data_set = "../income-data/income.dev.txt"
    # X_dev, Y_dev, features = get_binned_features(data_set, features)
    # print X_dev.shape

    # data_set = "../income-data/income.train.txt"
    # X, Y, features = get_num_ed_features(data_set)
    # print X.shape

    # data_set = "../income-data/income.dev.txt"
    # X_dev, Y_dev, features = get_num_ed_features(data_set, features)
    # print X_dev.shape


    perceptron = Perceptron(feature_size=len(X[0,:]))

    # print ("Before training:")
    # print(perceptron.test(xs, ys))
    # perceptron.train(xs,ys)

    # print("After (batch) training:")
    # print(perceptron.test(xs, ys))

    # perceptron.reset()

    # for j in range(1):
    #     for i in range(4):
    #         perceptron.train_online(xs[i], ys[i])

    # print ("After single training: ")
    # print(perceptron.test(xs,ys))

    # print ("Average")
    # perceptron.reset()
    # perceptron.average_train(xs, ys)
    # print(perceptron.test(xs,ys))
    # print ("Naive average (with maximum iterations)")
    # perceptron.reset()
    # perceptron.naive_average_train(xs, ys, maxIter=10)
    # print(perceptron.test(xs,ys))

    agg_thre = [0.0,0.1,0.5,0.9]

    for item in agg_thre:
        mira = Perceptron(feature_size=len(X[0,:]), mira_aggro=item)

        print "Agg Threshold: ", item
        print ("MIRA")

        epochs = 5
        count = 0
        max_score = 0
        max_score_epoch = 0
        mira.reset()
        for j in range(epochs):
            for i in range(len(X[:,0])):
                mira.train_mira(X[i,:], Y[i])
                count += 1
                if count % 1000 == 0:
                    score = mira.test(X_dev,Y_dev)
                    # print "Score: ", score
                    # print "Epoch: ", (1.0*j) + 1.0 + ((1.0*i) / len(X[:,0]))
                    if max_score < score:
                        max_score = score
                        max_score_epoch = (1.0*j) + ((1.0*i) / len(X[:,0]))


        print "Max score: ", max_score
        print "At epoch: ", max_score_epoch
        # print(mira.test(xs,ys))


        print ("MIRA Average")
        mira.reset()
        mira.train_mira_average(X,Y,X_dev,Y_dev,maxIter=5)
    # print(mira.test(xs,ys))
    
if __name__ == "__main__":
    main()
