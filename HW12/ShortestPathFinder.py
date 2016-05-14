__author__ = 'Modified by :aqeel'
# Copyright 2010-2014 Google
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Traveling Salesman Sample.
   This is a sample using the routing library python wrapper to solve a
   Traveling Salesman Problem.
   The description of the problem can be found here:
   http://en.wikipedia.org/wiki/Travelling_salesman_problem.
   The optimization engine uses local search to improve solutions, first
   solutions being generated using a cheapest addition heuristic.
   Optionally one can randomly forbid a set of random connections between nodes
   (forbidden arcs).
"""
import random
import gflags
from ortools.constraint_solver import pywrapcp
import RandomMatrix


class ShortestPathFinder:
    def __init__(self, pointslist):
        reload(gflags)
        self.FLAGS = gflags.FLAGS
        self.points = pointslist

        gflags.DEFINE_integer('tsp_size', len(pointslist),
                              'Size of Traveling Salesman Problem instance.')
        gflags.DEFINE_boolean('tsp_use_random_matrix', True,
                              'Use random cost matrix.')
        gflags.DEFINE_integer('tsp_random_forbidden_connections', 0,
                              'Number of random forbidden connections.')
        gflags.DEFINE_integer('tsp_random_seed', 0, 'Random seed.')
        gflags.DEFINE_boolean('light_propagation', False, 'Use light propagation')

    def FindApproximateShortestWay(self):
        # Create routing model
        if self.FLAGS.tsp_size > 0:
            # Set a global parameter.
            param = pywrapcp.RoutingParameters()
            param.use_light_propagation = self.FLAGS.light_propagation
            pywrapcp.RoutingModel.SetGlobalParameters(param)

            # TSP of size FLAGS.tsp_size
            # Second argument = 1 to build a single tour (it's a TSP).
            # Nodes are indexed from 0 to FLAGS_tsp_size - 1, by default the start of
            # the route is node 0.
            routing = pywrapcp.RoutingModel(self.FLAGS.tsp_size, 1)

            parameters = pywrapcp.RoutingSearchParameters()
            # Setting first solution heuristic (cheapest addition).
            parameters.first_solution = 'PathCheapestArc'
            # Disabling Large Neighborhood Search, comment out to activate it.
            parameters.no_lns = True
            parameters.no_tsp = False

            # Setting the cost function.
            # Put a callback to the distance accessor here. The callback takes two
            # arguments (the from and to node inidices) and returns the distance between
            # these nodes.
            matrix = RandomMatrix.RandomMatrix(self.points)
            matrix_callback = matrix.Distance
            if self.FLAGS.tsp_use_random_matrix:
                routing.SetArcCostEvaluatorOfAllVehicles(matrix_callback)
            else:
                routing.SetArcCostEvaluatorOfAllVehicles(RandomMatrix.distance)
            # Forbid node connections (randomly).
            rand = random.Random()
            rand.seed(self.FLAGS.tsp_random_seed)
            forbidden_connections = 0
            while forbidden_connections < self.FLAGS.tsp_random_forbidden_connections:
                from_node = rand.randrange(self.FLAGS.tsp_size - 1)
                to_node = rand.randrange(self.FLAGS.tsp_size - 1) + 1
                if routing.NextVar(from_node).Contains(to_node):
                    print 'Forbidding connection ' + str(from_node) + ' -> ' + str(to_node)
                    routing.NextVar(from_node).RemoveValue(to_node)
                    forbidden_connections += 1

            # Solve, returns a solution if any.
            assignment = routing.SolveWithParameters(parameters, None)
            if assignment:
                # Solution cost.
                cost =  assignment.ObjectiveValue()
                # Inspect solution.
                # Only one route here; otherwise iterate from 0 to routing.vehicles() - 1
                route_number = 0
                node = routing.Start(route_number)
                result =[]
                while not routing.IsEnd(node):
                    result.append(self.points[int(node)])
                    node = assignment.Value(routing.NextVar(node))
                result.append(self.points[0])
                return cost,result
            else:
                print 'No solution found.'
        else:
            print 'Specify an instance greater than 0.'
