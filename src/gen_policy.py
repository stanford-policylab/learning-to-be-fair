from ortools.linear_solver import pywraplp
import numpy as np

# optimally assign each of n individuals (fractionally) to one of k treatments
# to maximize the specified reward, with optional budget constraints and
# preferences for treatment parity. In particular, we maximize:
#
#   E[Y(T)] - penalty / num_groups * sum_g ||D(T|G = g) - D(T)||_1
# 
# subject to Pr(T=t) <= B_t
#
# where
#   T is (random) assignment of individuals to arms
#   Y(T) is the reward accrued by assigning an individual to treatment T
#   G is the group to which each individual belongs
#   D(*|*) denotes the conditional distribution, and || * ||_1 is the L^1 norm
#   B_t is the budget for arm t
#
# inputs:
#   reward: n x k matrix of rewards under each treatment arm for each individual
#   budget: optional k-dimentional list specifying capacity of each treatment arm
#   group_id: optional n-dimentional list specifying group membership
#   penalty: optional preferences for treatment parity
#   solver_name: by default, the GLOP solver is used, but others (e.g., 'GUROBI')
#      can be specified if appropriately configured on the system
#
# output:
#   n x k matrix of optimal (fractional) assignments
#
# requirements: google ortools, which can be installed with pip:
#
#   python3 -m pip install --upgrade --user ortools

def optimize(reward, budget=[], group_id=[], penalty=0, solver_name='GLOP'):
   
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
    
    # if specified, ensure budget constraints are satisfied
    if len(budget) > 0:
        for k in range(num_arms):
            solver.Add(np.mean(assignment[:,k]) <= budget[k])
           
    # if specified, encode treatment and outcome parity preferences.
    # we use L^1 penalty since it plays nice with LPs.
            
    # define proxy variables to encode the parity preferences
    # see: http://lpsolve.sourceforge.net/5.1/absolute.htm    
    if penalty > 0:
        proxy_t = np.array([[solver.NumVar(0, 1, 't-%d-%d' % (g,k)) 
                             for k in range(num_arms)]
                            for g in range(num_groups)])
 
    # add constraints to encode parity preferences
    for g, group in enumerate(groups):
        members = [i for i, gid in enumerate(group_id) if gid == group]
        
        # treatment parity
        if penalty > 0:
            for k in range(num_arms):
                solver.Add(
                    np.mean(assignment[members,k]) - np.mean(assignment[:,k]) 
                    <= proxy_t[g,k]
                )
                solver.Add(
                    np.mean(assignment[members,k]) - np.mean(assignment[:,k]) 
                    >= -proxy_t[g,k]
                )
                    
    # define the objective function
    if penalty == 0:
        solver.Maximize(1/num_people * np.sum(np.multiply(assignment,reward)))
        
    else:
        solver.Maximize(1/num_people * np.sum(np.multiply(assignment,reward))
                        - penalty/num_groups * np.sum(proxy_t))

    # invoke the solver
    status = solver.Solve()
    
    # return the results
    sol = None
    if status == pywraplp.Solver.OPTIMAL:
        sol = np.array([[assignment[i,k].solution_value()
                         for k in range(num_arms)]
                        for i in range(num_people)])
    
    return(sol)
