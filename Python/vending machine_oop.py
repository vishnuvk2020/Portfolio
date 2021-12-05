# create a boss machine,refill it,collect cash

class Myboss_machine:

    def __init__(self,capacity):
##        self.no_cans=cans
        self.capacity=capacity
        self.count=0
        self.leftover_cans=self.capacity-self.count
##        self.refill=0
      #  self.price=5
        print("your boss machine is created")
##    leftover_cans= 'global'
##    count='global'
    def getmyboss(self,cans):
##        count=0
        if self.capacity>cans:
            print("have your",cans,"boss cans")
            self.leftover_cans=self.capacity-cans
            self.count=self.count+cans
##            print("left over cans=",leftover_cans)
##            print("no of cans given=",count)
        else:
            print("Sorry,no cans left")
        
        return (self.count)
    
    
    def refill(self,cans):
        if (cans<=self.leftover_cans):
            self.capacity=self.capacity+cans
            print("number of cans refilled=",cans)
            print("total number of cans left in the machine=",self.capacity)
        else:
            print("sorry max capacity reached ")
        return(self.capacity)

##    def get_cashbalance(self):
##        cash_balance=5*count
##        return(cashbalance)

machine1=Myboss_machine(500)
machine1.getmyboss(2)
machine1.refill(100)
##machine1.getmyboss(1000)
##machine1.get_cashbalance()
        
        
        
        
        
        
        
        
            
            
    
