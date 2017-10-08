#!/usr/bin/python

import csv
import numpy as np


def get_binary_features(data_set, featureList=None):

	print "Loading Data"
	if featureList is None:
		featureList = get_binary_feature_vector(data_set)

	X, Y = get_binary_vector(data_set, featureList)

	print "Data Loaded"

	return X, Y, featureList

def get_binary_feature_vector(data_set):

	feature_list = []

	with open(data_set, 'rb') as csvfile:

		reader = csv.reader(csvfile, delimiter=',')

		for num_row, row in enumerate(reader):
			for i, item in enumerate(row):				
				if i != len(row)-1:
					if num_row == 0:
						feature_list.append([])
						feature_list[i].append(item)
					elif item not in feature_list[i]:
						feature_list[i].append(item)

		full_list = []
		for j, item in enumerate(feature_list):
			full_list += feature_list[j]

	return full_list

def get_binary_vector(data_set, feature_list):

	
	with open(data_set, 'rb') as csvfile:
		reader = csv.reader(csvfile, delimiter=',')

		for num_row, row in enumerate(reader):
			
			if num_row % 1000 == 0:
				print num_row

			if num_row == 0:
				X = np.zeros( (1,len(feature_list)+1) )
				Y = np.zeros( (1,1) )
				for item in row:
					if item == ' <=50K':
						Y[0] = -1
					elif item == ' >50K':
						Y[0] = 1
					else:
						X[0,feature_list.index(item)] = 1
						X[0,-1] = 1
			else:
				x = np.zeros( (1,len(feature_list)+1) )
				y = np.zeros( (1,1) )
				for item in row:
					if item == ' <=50K':
						y[0] = -1
					elif item == ' >50K':
						y[0] = 1
					else:
						x[0,feature_list.index(item)] = 1
						x[0,-1] = 1

				X = np.vstack([X,x])
				Y = np.vstack([Y,y])

	return X, Y



if __name__ == "__main__":

	X, Y = get_binary_features('../income-data/income.train.txt')

	print X[0]
	print Y[0]
