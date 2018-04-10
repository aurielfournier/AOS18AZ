#Function writing, why learn it?
# There's a couple of reasons learning functions is helpful. One you can write your own common functions. two you can better understand how to troubleshoot your code and other peoples functions. Three the principles of function writing teach us more about good coding tips to learn including reproduciability.  
# What is a function and why is it necessary in R
# A function lets you package code into one unit that doesnt need to be loaded multiple times, or rewritten.
sem<-function(x,na.rm=T){sd(x,na.rm=na.rm)/sqrt(length(x))}

data<-sample.int(100,10,replace=T)

c(hi=mean(data) + sem(data),lo= mean(data) - sem(data),mean=mean(data))

#And we can create a list of all these functions, so we never have to write them again, just load them. And we'll show you how to do this today.

# A function is the functional unit in R. R is built around vectorization and function writing.
# Once you understand this you'll get better at understanding why R does things the way it does them. And be able to fix functions or use them for your own troubleshooting
# A function is anything that has two parenthesis next to it. 
# so

mean
plot
ggplot

# But a function is simply a way to package up code and run it in a confined space. For example lets look at sd

sd

# you see that? That's actually the underlying code in the function.

sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x), 
         na.rm = na.rm))

# which we can write out simpler, ignoring the special case stuff

sqrt(var(x),na.rm=T)

# so if we give x a value.
x<-c(1:100)

sqrt(var(x))

#its the same thing as the sd function
sd(x)

# but why do this?
# 1. It's much cleaner. Typing sd() and not having anyone worry about the underlying stuff can be good for readability.
# 2. It's reproduciable, and no one can mess with the function. This code will work everytime the way its supposed to, there's no accidently messing up the code. You can hand this to someone and it will always work. And then hark back to #1
# 3. Much easier to give finished product to collaborator or someone with lower R understanding. Including package building
# 4. It's actually more memory friendly. Lets actually look at that. Here's a function.
# say we wanted to find the range of a vector. A post the maxinum, minimum, and range. How might we do that.
min.num<-min(x)
max.num<-max(x)
range<-max.num-min.num

c(minimum=min.num,maximum=max.num,range=range)

#Now in standard coding it created 3 values just to get to the end. Lets do this same thing in a function

range.function<-function(x){
min.num<-min(x)
max.num<-max(x)
range<-max.num-min.num

c(minimum=min.num,maximum=max.num,range=range)
}
x<-c(1:100)
range.function

range.function(x)

#And it doesnt matter what X is (within reason) this will work

x<-sample.int(100,100,replace=T)
x
range.function(x)

# This can be very important when you have a really long code. R does create these inbetweens but then deletes them. It never gets stored long term in memory, you never know its happening, and it disappears.

#One reason this might be important is when you're working with really large datasets. R actually has a whole suite of rules and functions to deal with memory isssues. And these will not get auto-triggered while just working in the console here. 

#5. Never rewrite code again. And this is mainly what we'll deal with today.
# R has two important tips. Vectorize things if you can, if you use a line of code more than once store it in an object or function.

# Lets break out and see if we can solve a problem and then we'll try to create a function from it
# One thing I get annoyed at is the base R doesn't have a function to calculating any part of a confidence interval. Including standard error.
############################
#I wanna make a function that calculates the 95% confidence interval no matter the input
#using these functions sd(), length(), qt()
# So here in a second I want y'all to write a short piece of code that takes this data, and calculates the confidence interval of a data set. So we'll start with this data
################variables###############
data<-c(1:10)   ###data as a vector
# And I want a 0.95% confidence interval.
# so whats the equation for confidence interval?
#
sd/sqrt(n) * tvalue (df) +/- mean    #heres what we need to do
# Now some people here may not be sure how to start doing this. And the important thing to remember
# Is to bring things into pieces and slowly go through it. Don't think of how to solve the whole problem
# Think of how to do the individual parts. If you run into an issue in the middle so be it.
# So split this equation in 3 parts (or more) and try to do them individualy.
#Take perhaps 15 minutes and see if you can write some code that converts the data into 3 numbers, the lower bound, the upper bound, and the mean. It doesnt matter how you decide to display the output


# 15 minute break

#In the middle mention that qt() measures quartile
qt(p=0.95,df=10)
# we should break it down into it's pieces and figure out how to do them one by one

sd.data<-sd(data)   #this function calculates the standard deviation

sem<-sd.data/sqrt(10)    # put them together. And there we go, we have sd/sqrt(n) also known as the standard error of the mean (sem)

#with some googling (or just using the help section) you can figure out that the function qt gives us the t value as long
# as we also give it the degrees of freedom (which is n-1) and the confidence interval (ci)
tvalue<-qt(0.95,(10-1))   ###with that we can write this  

mean.data<-mean(data)   ##the mean can be calculated with the mean function
sem*tvalue   ##gives us our interval
upper<-mean.data + sem*tvalue   ##putting it all together, this gives us our upper range

lower<-mean.data - sem*tvalue   ## our lower

##############################
# Ok now that we have all our pieces. How can we fit this into a function? 
# Well actually quite easy. 75% of it is just figuring out how to solve the problem in a code form. 

function(){
  sd.data<-sd(x)
  n<-length(x) 
  sem<-sd.data/10  
  tvalue<-qt(ci,(10-1))
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  
}

#And now I need to break down and explain how functions in R work. And remember this is how all functions in R work. Every single thing you've used in R is a function. 
#Functions are a set of instructions (code) that gets performed inside its own box. When you put the function(){}
#Around it its putting itself into its own box, also called an environment. You can imagine the code
#put inside the function as just a set of instructions, like we just bought something from IKEA.
#We have to give it the building pieces as well. So for now weve largely just grabbed an ikea box and threw a set of instructions inside. It will work but only for a single set of data. This is apparent when we clear the console. 

my.fun<-function(){
  sd.data<-sd(x)
  sem<-sd.data/10 
  tvalue<-qt(0.95,(10-1))
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  
  }

my.fun()
my.fun
#This doesnt do anything for a couple of reasons. But the obvious one is we never gave it our data. 
#This is where our arguments come in. And youre all very familiar with these. Lets open up a help file
?mean
#Arguments are how you put the building blocks inside our box so we can make something. 
#So in the mean code the important arguments are x, trim, and na.rm. When you see an equal sign 
#It means that is the default value, so if you never tell it what trim to do it will assume 0. If there
#is no equal sign then its a required variable and has no default. Like the x, which is the data. Now What the function does is run through its code and when it finds a variable the first place it looks is here in the arguments. And looks for where the function is. It then loads it into the environment.

#So lets give our function some arguments
#So what I really want this function to do is to calculate these things no matter what numbers I give it.
#So lets do that
# This variable name can be whatever we want, just whenever it sees that variable in the code it will attempt to load it from what we gave it. It looks in its own environment first, then our argument list, after that it looks in the global environment (VERY BAD)
my.fun<-function(x){
  sd.data<-sd(x)
  sem<-sd.data/10  
  tvalue<-qt(0.95,(10-1))
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  
}
my.fun(x=1:100)
#ok now that didn't do anything. Does anyone know why? I assure its not because it didnt do what we wanted it to do.

#It didnt work because we never told it do a bunch fo stuff, but never told it what to give us back.
#It did all of its stuff, deleted the products when it was done. which is effecient, but not every helpful.
#So all the end we have tell it to return something. If we dont tell it anything it will return the last thing
#printed to the console.
#You can either use return() or just tell it what to return

my.fun<-function(x){
  sd.data<-sd(x)
  sem<-sd.data/10  
  tvalue<-qt(0.95,9)
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  return(list(upper=upper,lower=lower,mean=mean.data))
}
my.fun(x=1:100)

#This is the same as just typing it. Return it will just return no matter what. So you can get in a good habitat of using it but its not always necessary.

#So our function works and it returns what we want it to no matter the numbers we give it. However theres a couple of things wrong with the function. Does anyone know one?
#Robust and reproducible
#Right this code doesnt actually calculate the confidence interval under every circumstance.
#Particularly the sem and tvalue part. Lets break up for like ten minutes and see if we can figure out how
#to have this calculate the n without us giving it to it. What I would suggest is not worrying about this in function form. I usually write it into a function at the very end so just paste this out.
#This is the first part of moving forward in R coding learning to problem solve and using R to help you.
#########33
# Now there are a couple of ways to do this, but in this particular case there's one kind of right answer.
# Making the code robust means it needs to calculate as much as possible without input
my.fun<-function(x){
  sd.data<-sd(x)
  n<-length(x) 
  sem<-sd.data/n  
  tvalue<-qt(0.95,(n-1))
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  return(list(upper=upper,lower=lower,mean=mean.data))
}
my.fun(x=1:1000)

#And thats it. Weve made our first official function. That takes whatever input we give it and gives us the right answer.

### break out and do something
# I want to create a function that converts farenheit measurements to celsius. Lets see if we can do that real quick
temp_fun<-function(temp_values){
  (temp_values-32)*5/9
}

#What if we want to add an extra layer of complexity. 0.95 percent confidence interval is great and somewhat standard, but what if we want to change that level? How can we do that in our function. You cant open a function an edit it everytime you want to change something small. So we can make probability another variable. And this is very easy. And we can give it a default value.

my.fun<-function(x,ci=0.95){
  sd.data<-sd(x)
  n<-length(x) 
  sem<-sd.data/n  
  tvalue<-qt(ci,(n-1))
  mean.data<-mean(x)
  sem*tvalue
  upper<-mean.data + sem*tvalue
  lower<-mean.data - sem*tvalue
  return(list(upper=upper,lower=lower,mean=mean.data))
}
my.fun(x=1:1000)

######
# What we start learning now are functions that are useful not just for our function writing but for your future codes in general.
#teach print
#As you get farther into programming youll find it useful to print out messages. Either something like 'Hey im starting', or 'oops i stopped', 'or you messed up!'. And if you get tired of your favorite R functions giving you illogical error messages you can even start writing in your own error messages. 

#The simple function to do this is print(). with fairly easy syntax
?print
print(i)

#Whatever you put in quotes will be printed to the console. This is useful because it will print out while code is going, including inside functions and for loops. Meaning you can put in status messages

#for example

for(i in 1:100000000){
  if(i==1) {print('im thinking')}
  # do some stuff
  if(i==100000000/2) {print('im thinking hard')}
  # do some more stuff
  if(i==100000000){print('im done')}
}

my.fun1<-function(){
  print('im thinking')
}
my.fun1()

#Theres a function that is very useful with the print function and gives a bit of an intro into more programming-esque functions. One of the first lessons is the If statement. This is part of the control structure of programming codes. They direct how codes are supposed to run. And are some of the fundamentals of programming.

#An If statement reads like this, IF something is true or false then DO something specific. So  write it out. If I am hungry then I will eat. so If something was true, I am hungry, then I should DO something. 

#In R it works like this. 
if(TRUE){print('I did it')}
if(FALSE){print('I did it')}
#So it splits out into two directions. And either does or doesnt do something. And this can be any statement as long as it makes a resulting statement that is either true or false
x<-1
if(x==1){print('I did it')}
x<-2

#And while currently it doesnt do anything if its not true, you can make it do something if the statement is not true.

if(TRUE){print('i did it')} else {'i didnt do it'}
if(FALSE){print('i did it')} else {'i didnt do it'}

#So lets return to our confidence interval function. A rule in writing good code is to make it robust. It must work under all conditions given. Currently our function breaks under a certain scenario. Can anyone tell me when?
#Robust and Reproducible
#Right it breaks if you put a confidence interval that is greater than 1. Because this is illogical. And so what well do is have tell us that we inputted an incorrect value and then ofcourse not do the resulting function


#So lets try that out in your code. to check the input of the confidence interval.

CI.fun<-function(data,ci){
  
  if(ci>1){print("You can't have a probability greater than 1 you goof!")}else{
    
    sd.data<-sd(data)
    n<-length(data)
    sem<-sd.data/n
    tvalue<-qt(ci,(length(data)-1))
    mean.data<-mean(data)
    upper<-mean.data + sem*tvalue
    lower<-mean.data - sem*tvalue
    
    list1<-list(upperbound=upper,lowerbound=lower,mean=mean.data)
    return(list1)
  }}

#
#maybe teach next and stop, given time.

#
#paste
#A very powerful tool in programming is the ability to create dynamic messages or outputs. For instance, in this code, what if we wanted to add to our error message and them them what Confidence interval they actual put in? You do this with paste. In other languages this is the same as concatenate. 

?paste
?paste0
x<-'matt'
paste0('hello','my','name','is',x)
paste('hello','my','name','is',x)
#How these work is they 'paste' together whatever things you give it, exactly how you tell it to

paste0('hello')
paste('hello','world')
paste0('hello','world')
paste0('hello',' world')
paste('hello','world',sep='-')
paste('hello','world','my name is',sep=' ')

#This becomes useful when you add in dynamic variables
i<-'the best'

paste0('i am a: ',i)

#Paste is most useful with two things. Reading/writing in files and in concert with the print function.
#For instance say you have a set of data but is seperated by state. You could input:
  
  state<-'AZ'
  paste0('myfiles_',state,'.csv')
  state<-c('AZ','TX')
  paste0('myfiles_',state,'.csv')
  read.csv(paste0('myfiles_',state,'.csv'))
  
#So you can use this in for loops to load multiple files at once, or decide what to load.
  
#Same with file saving
  
  extension<-'final1'
  write.csv(data,paste0('myfiles_',extension,'.csv'))
  
#So returning to our function. We can have it print out our values if wed like.
#If you noticed in our print function, you can only tell it a very static set of things to print.
#With the combination of paste you can have it say different things depending on the input.

##################################

################variables###############
data<-c(1:10)   ###data as a vector
ci<-.01         ###confidence
########################################
##start of code
CI.fun<-function(data,ci){
  
  if(ci>1){print(paste0("You can't have a probability greater than 1 you goof! You entered: ",ci)}else{
    
    sd.data<-sd(data)
    n<-length(data)
    sem<-sd.data/n
    tvalue<-qt(ci,(length(data)-1))
    mean.data<-mean(data)
    upper<-mean.data + sem*tvalue
    lower<-mean.data - sem*tvalue
    list(upperbound=upper,lowerbound=lower,mean=mean.data)
  }}   
   

#So I want us to break out and try something real quick. I want this function to not only return our list, but tell us (print) the upper, lower, and mean value. Can you do this?

CI.fun<-function(data,ci){
  
  if(ci>1){print(paste0("You can't have a probability greater than 1 you goof! You entered: ",ci))}else{
    
    sd.data<-sd(data)
    n<-length(data)
    sem<-sd.data/n
    tvalue<-qt(ci,(length(data)-1))
    mean.data<-mean(data)
    upper<-mean.data + sem*tvalue
    lower<-mean.data - sem*tvalue
print(paste0('your upper bound is: (',upper, ') and your lower bound is: (', lower,"), and your mean is: (", mean.data,')'))
list(upperbound=upper,lowerbound=lower,mean=mean.data)
  }}

#And weve basically created our function. And you can make these for anything you do often. To solve any problems you have.
# Not every situation is 
# Troubleshooting, its defintely an end game
# Changed inputs. 
# Sharing

##ggtheme
#As mentioned earlier you can create one to load your own ggtheme after youve spent tireless hours creating it.

mytheme<-function(){Need to do this}
  
ggplot() + mytheme()
######
#A final example maybe?
#
#Source and loading 
#And as you grow these lists of functions youll want to save them and use them again easily. One of the easy ways to do this is to save them all in one code, and load that code into R when you need it.

#For example I have a 'basics' sheet and it contains things I use all the time.
source('file:C:/Users/birde/Dropbox/r_packages/radar2/R/basics.R')
# Show link for how to make a package (not enough time to teach)
#The more advanced form of this is to create a package that contains all the functions you need, particularly if these functions are really long. THis is useful because you can hand packages over to collaborators, create help files, and even keep data sets in them.

#Let me show you
library(devtools)
?load.local
#Load will load the package like source, but wont load the help files
load_all('C:/Users/birde/Dropbox/r_packages/radar2')
#Once you install it will load the help files
install('C:/Users/birde/Dropbox/r_packages/radar2')
?geom
?beamht_std
geom

beamht_std

#This is the blog I used to get me started in package writing. And it hasnt really changed much since then.

#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


  
### More we definitely wont have time to cover
#############################################################
##                      for loops                          ##
#############################################################
##an iterative process that loops through a sequence

for(i in 1:10){ 
  print(i) 
}   ###what did this do?
i<-1
print(i)
i<-2
print(i)
i<-3
print(i)
# it wrapped through all values between 1 and 10...
for(i in 1:10){
  #stores the value as the variable i (or any variable name we told it to)
  i<-1
  #and then runs EVERYTYHING between the brackets with the variable as that value
  print(i) 
}

i<-2
print(i)

# it then does this for the 1st value, then the 2nd, and so on until all values have been looped through the entire sequence of whatever it is we give to it

# because all its doing is storing the value as the variable (again it can be named anything)
#we can treat it like a normal variable

for(i in 1:10){
  print(i+1)
}  ## in this case we want to add 1 to that value. So it took 1 and added 1 (equals 2), then took the next value (2) and added 1 (equals 3) so on and so on.

#####
# We can do this for characters, it doesnt matter. It does a simple thing but this simple thing makes endless complexities
for(i in c('Matt','Auriel')){
  print(i)
}

for(i in c('Matt','Auriel','Liz')){
  if(i=='Matt'){print('Matt is awesome')}
  if(i=="Auriel"){print('Auriel is not')}
}
## for loops are useful for refering to rows or columns in a data set if we just feed it sequential numbers
data<-matrix(1:100,nrow=10,ncol=10)
data

for(i in 1:10){
  print(data[i,1])
}    ### is going to loop through all the rows in column 1 (and print that value)

for(i in 1:10){
  print(data[i,2])
}    ###loops through all the rows in column 2 (and prints that value)

for(i in 1:10){
  print(data[i,1]+5)
}   ###loops through all the rows in column 1 and adds 5 to that value

data[,1] <- 100

for(i in 1:10){
  data[i,2]<-data[i,1]+10
}   ###does the same thing but stores the value in the correct row in column 2

data

data[,2]<- data[,1]+10  ###is the best way to do this particular task

#At this point you may be asking, well this is extremely useful or but wait Matt, that's not how you do that. 
# You're correct, for loops are one of the bases of all of programming. But, R has already written many of its functions with internal for loops, but in the C++ code, not in the R code.
#many functions can be vectorized, meaning r automatically loops things together
paste0('hd',1:10)   ### automatically adds the numbers 1 through 10 onto the word 'hd'
#All this is doing is basically this, but more effeciently:
for(i in 1:10){
  print(paste0('hd',i))
}

#I use the term vectorizing for refering to the concept that R functions have already written iterative proccesses in the function

#Trouble shooting for loops. For loops can be difficult to troubleshoot when they hit a snag so I wanted to give you a couple of tips on troubleshooting them.

#1. Put in a line at the top that allows you to read in a dummy variable

for(i in 1:10){
  #i<-1
  print(i)
  
}

#2. Print the outputs, or save middle outputs
for(i in c(1,2,NA,4)){
  print(i)
  sub.data<-i+1
  print("heres the middle data set")
  print(sub.data)
  print('now printing mean')
  mean(sub.data)
}

#3. Write the for loop last
for(i in 1:10){
  #i<-1
  mean(i)+2
  print(paste0('the answer is: ', i))}


##############################
# Summary of if and for loops
#####################################################################

##-'for' loops are best to do tasks that can not be vectorized or when memory is an issue
##- 'for' loops are ideal for looping through files or starter values
##-'If' statements should only be used if vectorizing was not an option
##-If you have to make more than one 'if' statement you can probably vectorize it
##-Neither is ideal for math, since R is very good at doing this
##-Exceptions are when a function can not be vectorized or when referencing previous values
##-Also for lowering memory usage 
#(If your files exceed 1.5gb looping or other packages may be requried)

#end of lesson

