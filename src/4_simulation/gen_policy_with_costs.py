from ortools.linear_solver import pywraplp
import numpy as np

# optimally assign each of n individuals (fractionally) to one of k treatments
# to maximize the specified reward subject to a budget constraint, with optional 
# preferences for budget parity. In particular, we maximize:
#
#   E[Y(T)] - penalty * sum_g | E[c(X,T) | G=g] - E[c(X,T)] |
# 
# subject to E[c(X,T)] <= b
#
# where
#   T is (random) assignment of individuals to arms
#   Y(T) is the reward accrued by assigning an individual to treatment T
#   G is the group to which each individual belongs
#   c(x,t) is the cost for assigning individual x to treatment t
#   b is the overall budget per person
#
# inputs:
#   reward: n x k matrix of rewards under each treatment arm for each individual
#   cost: n x k matrix of costs for each treatment for each person
#   budget: parameter specifying average budget per person
#   group_id: optional n-dimentional list specifying group membership
#   penalty: optional preferences for budget parity
#   solver_name: by default, the GLOP solver is used, but others (e.g., 'GUROBI')
#      can be specified if appropriately configured on the system
#
# output:
#   n x k matrix of optimal (fractional) assignments
#
# requirements: google ortools, which can be installed with pip:
#
#   python3 -m pip install --upgrade --user ortools

def optimize(reward, cost, budget, group_id=[], penalty=0, solver_name='GLOP'):
   
    # specify the solver
    solver = pywraplp.Solver.CreateSolver(solver_name)

    num_people, num_arms = reward.shape
    groups = list(set(group_id))
    num_groups = len(groups)
    
    # define the assignment variables
    assignment = np.array([[solver.NumVar(0, 1, 'x-%d-%d' % (i,k)) 
                            for k in range(num_arms)]
                           for i in range(num_people)])
        
    # ensure everyone is assigned to a treatment
    for i in range(num_people):
        solver.Add(sum(assignment[i,:]) == 1)
    
    # ensure budget constraint is satisfied
    solver.Add(np.sum(np.multiply(assignment,cost)) / num_people <= budget)
           
    # if specified, encode budget parity preferences.
    # we use L^1 penalty since it plays nice with LPs.
            
    # define proxy variables to encode the parity preferences
    # see: http://lpsolve.sourceforge.net/5.1/absolute.htm    
    if penalty > 0:
        proxy = np.array([solver.NumVar(0, solver.infinity(), 'w-%d' % g) 
                            for g in range(num_groups)])
 
        # add constraints to encode parity preferences
        for g, group in enumerate(groups):
            members = [i for i, gid in enumerate(group_id) if gid == group]
            num_members = len(members)
        
            solver.Add(
                np.sum(np.multiply(assignment[members,:],cost[members,:])) / num_members
                        - np.sum(np.multiply(assignment,cost)) / num_people
                    <= proxy[g]
                )
            
            solver.Add(
                np.sum(np.multiply(assignment[members,:],cost[members,:])) / num_members
                        - np.sum(np.multiply(assignment,cost)) / num_people
                    >= -proxy[g]
                )
   
    # define the objective function
    if penalty == 0:
        solver.Maximize(np.sum(np.multiply(assignment,reward)) / num_people)
        
    else:
        solver.Maximize(np.sum(np.multiply(assignment,reward)) / num_people
                        - penalty * np.sum(proxy))

    # invoke the solver
    status = solver.Solve()
    
    # return the results
    sol = None
    if status == pywraplp.Solver.OPTIMAL:
        sol = np.array([[assignment[i,k].solution_value()
                         for k in range(num_arms)]
                        for i in range(num_people)])
    
    return(sol)
