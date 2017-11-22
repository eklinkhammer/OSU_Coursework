#!/usr/bin/env python

from tagger import mle, test, decode, readfile
import sys
from collections import defaultdict

startsym, stopsym = "<s>", "</s>"

# Dictionary maps every word to the set of all possible parts of speech
# Model maps every POS-word tuple to log prob

def train(T,D,model, trainfile, devfile, lr):
    updates = 0
    best_train = test(trainfile, D, model)
    best_dev = test(trainfile, D, model)
    my_model = defaultdict(float)
    #models.append(copy_dict(model))
    c = 1
    for t in xrange(T):
        for x_i, y_i in readfile(trainfile):
            # temp_model = copy_dict(model)
            # for k in my_model:
            #     temp_model[k] -= my_model[key] / c
            z = decode(x_i, D, model)
            if z != y_i:
                updates += 1
                phi_z = phi(x_i, z)
                phi_y = phi(x_i, y_i)

                for key in phi_y:
                    model[key] += lr * phi_y[key]
                    my_model[key] += lr * c * phi_y[key]

                for key in phi_z:
                    model[key] -= lr * phi_z[key]
                    my_model[key] -= lr * c * phi_z[key]

                # if de < best_dev:
                #     te = test(trainfile, D, temp_model)
                #     print "Better error rate"
                #     print "epoch", t, "updates ", updates, "feature", sum([1 for k in my_model]), "train_err {0:.2%}".format(te), "test_err {0:.2%}".format(de)
                #     best_dev = de
            c += 1
            
        te = test(trainfile, D, model)
        de = test(devfile, D, model)
        print "epoch", t, "updates ", updates, "feature", sum([1 for k in my_model]), "train_err {0:.2%}".format(te), "test_err {0:.2%}".format(de)

    for key in my_model:
        model[key] -= my_model[key]/c
    return model

def copy_dict(d):
    new_d = defaultdict(float)
    for k in d:
        new_d[k] = d[k]

    return new_d

def average_dicts(ds):
    l = len(ds) if len(ds) != 0 else 1
    
    sum_dic = defaultdict(float)
    for d in ds:
        for k in d:
            sum_dic[k] += d[k]
    for k in sum_dic:
        sum_dic[k] /= l

    return sum_dic
        
def phi(x,y):
    p = defaultdict(lambda: defaultdict(int))

    for i in xrange(len(y)):
        tag_1 = startsym if i == 0 else y[i-1]
        tag_2 = stopsym if i == len(y) else y[i]
        p[tag_1, tag_2] = 1
    
    for x_i, y_i in zip(x,y):
        p[y_i, x_i] = 1

    for i in xrange(len(y)):
        tag_1 = startsym if i == 0 or i == 1 else y[i-2]
        tag_2 = startsym if i == 1 else y[i-1]
        tag_3 = y[i]
        p[tag_1, tag_2, tag_3] = 1

    return p

if __name__ == "__main__":
    trainfile, devfile, testfile = sys.argv[1:4]

    dictionary, model = mle(trainfile)

    print "train_err {0:.2%}".format(test(trainfile, dictionary, model))
    print "dev_err {0:.2%}".format(test(devfile, dictionary, model))
    
    model = train(15, dictionary, model, trainfile, devfile, 0.5)
    print "train_err {0:.2%}".format(test(trainfile, dictionary, model))
    print "dev_err {0:.2%}".format(test(devfile, dictionary, model))

    output = open('test.lower.unk.best', 'w')
    for line in open(testfile):
        words = line.split() 
        tags = decode(words, dictionary, model)
        for word, tag in zip(words, tags):
            output.write(word + '/' + tag + ' ')
        output.write("\n")

    output = open('dev.lower.unk.best', 'w')
    for line in open(devfile):
        wordtags = map(lambda x: x.rsplit("/", 1), line.split())
        words = [w for w,t in wordtags]

        tags = decode(words, dictionary, model)
        for word, tag in zip(words, tags):
            output.write(word + '/' + tag + ' ')
        output.write("\n")
