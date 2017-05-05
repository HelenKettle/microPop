getMolarYields=function(microbeNames,numPaths,resData,resNames,molStoi,keyRes,Rtype){

    #molStoi is an array [gname,rname,pname]
    #keyRes is a list of vectors. The elements in the list are the group names
    #The keyRes vectors contain the name (string) of the key resource on each path. They have pathnames (e.g. path1, path2)
    #i.e keyRes$gname['pathname']
    #Rtype is an array [gname,rname,pname]

    molYield=NA*molStoi
    
     for (gname in microbeNames){

 
         pathnames=paste('path',seq(1,numPaths[gname]),sep='')

         for (pname in pathnames){

             key=keyRes[[gname]][pname]
 
             if (!is.na(key)){
 
                 if (key=='Hex' & !key%in%resNames){#i.e. substitutable resources
                     resType=Rtype[gname,,pname]
                     keyStoi=mean(molStoi[gname,resType=='S',pname],na.rm=TRUE)
                     #print(keyStoi)
                 }else{
                     keyStoi=molStoi[gname,key,pname]
                 }

                 molYield[gname,,pname]=molStoi[gname,,pname]/keyStoi
             }
         }
     }
    
    return(molYield)

}
    
