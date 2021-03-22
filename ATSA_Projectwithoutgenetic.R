library(xts)
library(ts)
library(PerformanceAnalytics)
library(fTrading)
mydata=read.csv('indian.csv')
start_time <- Sys.time()
head(mydata)
tail(mydata)
mydata_ts=ts(data=mydata$Close)
typeof(mydata$Close)
plot(mydata_ts)
length(mydata$Date)
maximum_ts=max(mydata$Close)
minimum_ts=min(mydata$Close)



####state Ranges
#state1=(3548.21,13654.85)
#state2=(13654.85,23561.53)
#state3=(23561.53,33868.19)
#state4=(33868.19,43954.85)
#state5=(43954.85,54081.51)

#retrive row ,col mydata[1,2]
records=nrow(mydata)
#windowmaxrange=list()
#windowminrange=list()


windowminfunc<-function(ncw) {

#print("inside min functuion")
i=1
minimum_window=99999
wminrange=list()
count=1
while(count<=records)
{

  if(minimum_window > mydata[count,5]){
   minimum_window=mydata[count,5] 
  }
  
    
    if(count%%ncw==0)
    {
      #print(minimum_window)
      #print(maximum_window)
      wminrange[i]=minimum_window
      i=i+1
      minimum_window=99999
      
    }
  
  count=count+1;
  #print("Oustside count loop")
  #print(count)
  
}


if(minimum_window!=99999 &&  records%%ncw!=0  )
{
  #print("hello")
  wminrange[i]=minimum_window
  i=i+1
  minimum_window=99999
}

#print(wminrange)
return(wminrange)

}


windowmaxfunc<-function(ncw){

#windowmaxrange=list()
#windowminrange=list()
i=1
maximum_window=0
wmaxrange=list()
count=1
#ncw=20
while(count<=records)
{
  #print(count)
  #print(minimum_window)
  #print(mydata[count,5])
  
  
  if(maximum_window<mydata[count,5])
  {
    maximum_window=mydata[count,5]
  }
    
    if(count%%ncw==0)
    {
      #print(minimum_window)
      #print(maximum_window)
      wmaxrange[i]=maximum_window
      i=i+1
      maximum_window=0
      
    }
  
  count=count+1;
  
}


if( maximum_window!=0&&  records%%ncw!=0)
{
  wmaxrange[i]=maximum_window
  i=i+1
  maximum_window=0
}

#print(windowrange)
#mydata[1][0]

#print(wmaxrange)
return(wmaxrange)

}

#print(lik)
#nos=5
#nw=12
#ncw=20
likmatrix <- function (nos,ncw,nw,windowmaxrange,windowminrange){ 
  #nw=(records/ncw)+1
##lik matrix 
w=1
i=1
j=1
lik=matrix(nrow = nos,ncol = nos)
while(i<=nos)
{
  j=1;
  while(j<=nos)
  {
    lik[i,j]=0;
    j=j+1;
  }
  i=i+1;
}


while(w <=nw){
  range=(windowmaxrange[[w]]-windowminrange[[w]])/nos
  st<- rep(0, nos)
  for(k in 1:nos){
  	st[k]=windowminrange[[w]]+(k-1)*range
  }
  

  
  candle=1
  while(candle<ncw){
  	for(i in 1:(nos-1)){
    if(mydata[(w-1)*ncw+candle,5]>=st[i] &&  mydata[(w-1)*ncw+candle,5]<(st[i]+range) && (w-1)*ncw+candle<records){
    	for(j in 1:(nos-1)){
           if(mydata[(w-1)*ncw+candle+1,5]>=st[j] && mydata[(w-1)*ncw+candle+1,5]<(st[j]+range)){
            lik[i,j]=lik[i,j]+1;
            }
        }
        if(mydata[(w-1)*ncw+candle+1,5]>=st[nos] && mydata[(w-1)*ncw+candle+1,5]<=(st[nos]+range)){
            lik[i,nos]=lik[i,nos]+1;
            } 
    }
  	}
    if(mydata[(w-1)*ncw+candle,5]>=st[nos] &&  mydata[(w-1)*ncw+candle,5]<=(st[nos]+range) && (w-1)*ncw+candle<records){
      for(j in 1:(nos-1)){
        if(mydata[(w-1)*ncw+candle+1,5]>=st[j] && mydata[(w-1)*ncw+candle+1,5]<(st[j]+range)){
          lik[nos,j]=lik[nos,j]+1;
        }
      }
      if(mydata[(w-1)*ncw+candle+1,5]>=st[nos] && mydata[(w-1)*ncw+candle+1,5]<=(st[nos]+range)){
        lik[nos,nos]=lik[nos,nos]+1;
      } 
    }
    
    
    candle=candle+1;
 }

 w=w+1;
}
#}


#print(lik)
return(lik)

}

secondlaststate=function(nos,nw,windowmaxrange,windowminrange) {
	range=(windowmaxrange[[nw]]-windowminrange[[nw]])/nos
  	st<- rep(0, nos)
  	for(k in 1:nos){
  		st[k]=windowminrange[[nw]]+(k-1)*range
  	}

  	for(k in 1: nos){
  		if(mydata[records-1,5]>=st[k]&&mydata[records-1,5]<st[k]+range)
  			break;
  	}	
  
  	return(k);
}

laststate=function(nos,nw,windowmaxrange,windowminrange) {

	range=(windowmaxrange[[nw]]-windowminrange[[nw]])/nos
  	st<- rep(0, nos)
  	for(k in 1:nos){
  		st[k]=windowminrange[[nw]]+(k-1)*range
  	}

  	for(k in 1: nos){
  		if(mydata[records,5]>=st[k]&&mydata[records,5]<st[k]+range)
  			break;
  	}	
  
  	return(k);
}


lkstar<-function(nos,ncw,nw,windowmaxrange,windowminrange){

#lk matrix
#lk<-c(1,1,1,1,1)



lk<-rep(0,nos)
#print(lk[1])

w=1
while(w <=nw){
  
  range=(windowmaxrange[[w]]-windowminrange[[w]])/nos
  st<- rep(0, nos)
  for(k in 1:nos){
  	st[k]=windowminrange[[w]]+(k-1)*range
  }
  
#  print("s5")
 # print(st[nos])

ls=laststate(nos,nw,windowmaxrange,windowminrange)  
  candle=1
  while(candle<ncw){
     if(mydata[(w-1)*ncw+candle,5]>=st[ls] && mydata[(w-1)*ncw+candle,5]<(st[ls]+range)&& (w-1)*ncw+candle<records){
     for(m in 1:nos){
      
      if(m!=nos){
      if(mydata[(w-1)*ncw+candle+1,5]>=st[m] && mydata[(w-1)*ncw+candle+1,5]<(st[m]+range)){
        lk[m]=lk[m]+1;
      }
      }
       else{
         if(mydata[(w-1)*ncw+candle+1,5]>=st[m] && mydata[(w-1)*ncw+candle+1,5]<=(st[m]+range)){
           lk[m]=lk[m]+1;
         }
       }
    }
      
   }
    
    candle=candle+1
  }
  #print(lk)
  
  w=w+1;
}

print("lk* matrix is ")
print(lk)
#print(lk[2])

return(lk)
}


probablitymatrix<-function(nos,ncw,nw,lik,lk,windowmaxrange,windowminrange){
#pik matrix
pik=matrix(nrow = nos,ncol = nos)

i=1
j=1


while(i<=nos)
{
  j=1;
  sum=0
  while(j<=nos)
  {
    if(lk[i]!=0){
    pik[i,j]=lik[i,j]/lk[i];
    }
    else{
      pik[i,j]=lik[i,j]
    }
    sum=sum+pik[i,j]
    j=j+1;
  }
  
  j=1;
  while(j<=nos)
  {
    if(sum!=0){
    pik[i,j]=pik[i,j]/sum;
    }
    else{
      pik[i,j]=pik[i,j]
    }
    j=j+1;
  }
  
  i=i+1;
}

#print(pik)
return(pik)

}


#profit(5,20,12)
zfunction<-function(nos,ncw,nw,pik,windowmaxrange,windowminrange){
w=1
z=0;

while(w<=nw)
{
  
  range=(windowmaxrange[[w]]-windowminrange[[w]])/nos
  st<- rep(0, nos)
  for(k in 1:nos){
    st[k]=windowminrange[[w]]+(k-1)*range
  }
  #print("window is ")
  #print(w)
  
  candle=1
  while(candle<ncw)
  {
    #print("candle is ")
    #print(candle)
    
    for(i in 2:(nos-1)){
		if(mydata[(w-1)*ncw+candle,5]>=st[i] && mydata[(w-1)*ncw+candle,5]<(st[i]+range) && (w-1)*ncw+candle<records){
      		
		    if(pik[i,(i-1)]<pik[i,(i+1)]){
        		z=z+abs(mydata[(w-1)*ncw+candle+1,5]-mydata[(w-1)*ncw+candle,5])
      		}
      		
      		else{
        		z=z+abs(mydata[(w-1)*ncw+candle,5]-mydata[(w-1)*ncw+candle+1,5])        
      		}

    	}
    
    
  }
    candle=candle+1;
    #print(z)
  }
  
  w=w+1
}
print("z is" )
print(z)
return(z)
}


cprofit<-function(nos,ncw,nw,pik){
  w=1
  z=0;
  cp<- c()
  windowmaxrange= windowmaxfunc(ncw)
  windowminrange=windowminfunc(ncw)
  
  while(w<=nw)
  {
    
    range=(windowmaxrange[[w]]-windowminrange[[w]])/nos
    st<- rep(0, nos)
    for(k in 1:nos){
      st[k]=windowminrange[[w]]+(k-1)*range
    }
  
    
    candle=1
    while(candle<ncw)
    {
      #print("candle is ")
      #print(candle)
      
      for(i in 2:(nos-1)){
        if(mydata[(w-1)*ncw+candle,5]>=st[i] && mydata[(w-1)*ncw+candle,5]<(st[i]+range) && (w-1)*ncw+candle<records){
          
          if(pik[i,(i-1)]<pik[i,(i+1)]){
            z=z+mydata[(w-1)*ncw+candle+1,5]-mydata[(w-1)*ncw+candle,5]
            cp<-c(cp,z)
          }
          
          else{
            z=z+mydata[(w-1)*ncw+candle,5]-mydata[(w-1)*ncw+candle+1,5]  
            cp<-c(cp,z)
          }
          
        }
      }
      candle=candle+1;
    }
    
    w=w+1
  }
  print("**********************************************************")
  a<-unlist(cp, recursive = TRUE, use.names = TRUE)
  cp_ts=ts(data=a)
  plot(cp_ts)
  
}


profit<-function(nos, ncw, nw){
  
  windowmaxrange= windowmaxfunc(ncw)

  #print("Windowmaxrange[[1]]")
  #print(windowmaxrange)
  #print("windowmaxrange")
  #print(windowmaxrange)

  windowminrange=windowminfunc(ncw)
  #print("windowminrange")
  #print(windowminrange)
  
  
  lik1=likmatrix(nos,ncw,nw,windowmaxrange,windowminrange)
  print("lik matrix is ")
  print(lik1)
  
  lkarray=lkstar(nos,ncw,nw,windowmaxrange,windowminrange)
  print("lk* matrix is ")
  print(lkarray)

  pik1=probablitymatrix(nos,ncw,nw,lik1,lkarray,windowmaxrange,windowminrange)
  print("probability matrix is ")
  print(pik1)
  
  z1=zfunction(nos,ncw,nw,pik1,windowmaxrange,windowminrange)
  print("profit is ")
  print(z1)
  
  cprofit(nos,ncw,nw,pik1)
  MDD=maxDrawDown(mydata[,5])
  print("MDD is  ")
  print(MDD[[1]])
  CR=z1/MDD[[1]]
  print("Calmer ratio is")
  print(CR)
  
  return(z1)
}
#profit(8,7,177)
#windowminfunc(3,14)
finals=0;
finalc=0;
maxz=0;

#s 3:20, candle 8:100
for(s in 3:20){
  print("states are")
  print(s)
  for(ncw in 8:100){
    print("no of candles ")
    print(ncw)
    #print(ceiling(nrow(mydata)/10));
    nw=ceiling((nrow(mydata)/ncw));
    print("no of windows ")
    print(nw)
    receivez=profit(s,ncw,nw)
    if(receivez>maxz){
      maxz=receivez
      finals=s;
      finalc=ncw;
    }
  }
}
print(finals)
print(finalc)
end_time <- Sys.time()
print("time taken")
print(end_time - start_time)
#cprofit(8,89,14)
