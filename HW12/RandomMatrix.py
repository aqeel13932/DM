__author__ = 'aqeel'
import math
import numpy as np
# function to Calculate the distance which can be replaced with any Type of Distance
# ex. Google API to Calculate the distance between two GPS Coordinations
def distance(p1, p2):
    return np.linalg.norm(p1-p2)


class RandomMatrix(object):
    """Random matrix."""

    def __init__(self,points):
        self.lst = points
        """Initialize random matrix."""
        self.matrix = {}
        for from_node in xrange(len(self.lst)):
            self.matrix[from_node] = {}
            for to_node in xrange(len(self.lst)):
                if from_node == to_node:
                    self.matrix[from_node][to_node] = 0
                else:
                    self.matrix[from_node][to_node] = distance(self.lst[from_node], self.lst[to_node])

    def Distance(self, from_node, to_node):
        return self.matrix[from_node][to_node]
