
# coding: utf-8

# In[96]:

import itertools
import sys


# In[97]:

main =[]
main.append(sorted(['B','C', 'A' ,'F', 'H']))
main.append(sorted(['F', 'E', 'C', 'H']))
main.append(sorted(['E' ,'D', 'B']))
main.append(sorted(['A', 'C' ,'H', 'F']))
main.append(sorted(['E', 'F' ,'A']))
main.append(sorted(['D', 'H' ,'B']))
main.append(sorted(['E', 'C' ,'F', 'B', 'D']))
main.append(sorted(['A', 'H' ,'C', 'E']))
main.append(sorted(['G', 'A', 'E']))
main.append(sorted(['B', 'H', 'E']))


# In[98]:

class Core:
    def __init__(self,name=None,occurence=0):
        self.name = name
        self.occurence =occurence
        self.sons={}
    def AddItem(self,items):
        if len(items)<=0:
            return
        if items[0] in self.sons.keys():
            self.sons[items[0]].occurence+=1
            self.sons[items[0]].AddItem(items[1:])
        else:
            self.sons[items[0]] = Core(name=items[0],occurence=1)
            self.sons[items[0]].AddItem(items[1:])
    def printeverything(self,level=0):
        #print 
        thespace = '----'*level
        print(thespace+str(self.name)+':'+str(self.occurence))
        for itm in self.sons.keys():
            self.sons[itm].printeverything(level=level+1)
        #sys.stdout.write('\n')
    #def printeverything2(self,level=0):
    #    print '\t' * level + repr(self.value)
    #    for child in self.children:
    #        child.other_name(level+1)


# In[99]:

nullobject = Core()
for item in main:
    nullobject.AddItem(item)


# In[100]:

nullobject.printeverything()

