import random
import numpy as np


# Exercise 4 simulation
def GetDisk():
    return random.randint(0, 1000)


n = 0
totaltries = 0
lst = []
for i in range(0, 100):
    while (n < 4):
        if (GetDisk() <= 25):
            n += 1
        totaltries += 1
    lst.append(totaltries)
    totaltries = 0
    n = 0
#print the average for tested disks to get 4 defected
print np.mean(lst)


#Simulation for PickSuccess Student
def PickSuccessStudent():
    #Student is Prepared(0.8 probability)
    if (random.randint(0,10)<=8):
        #Student Succeed(0.7 probability)
        if (random.randint(0,10)<=7):
            return True
        #Student faild (0.3 Probability)
        else:
            return False
    #Student is not prepared (0.2 probability)
    else:
        #Student Succeed (0.4 probability)
        if(random.randint(0,10)<=4):
            return True
        #Student failed (0.6 probability)
        else:
            return False
succeed=[]
for j in range (0,100):
    tmp =0
    for i in range (0,100):
        if PickSuccessStudent():
            tmp+=1
    succeed.append(tmp)
print float(np.mean(succeed))/float(100)

