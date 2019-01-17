import math
#find largest value of binomal coefficent

m = 1
ovflow= 0.9999*(10**15)
over=False
x=0
while(not over):
    #have to increment by one b/c ! cannot handle decimals
   m=m+1
   if(math.factorial(m) > ovflow):
       over=True
       
print "overflowed at ", m, "so largest value is ", m-1
   
