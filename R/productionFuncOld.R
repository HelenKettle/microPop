productionFuncOld=function(rname,ess,subst,all.substrates,keyResName,stoichiom,products,bio.products,uptake,growthRate.p,yield){
    
    if (growthRate.p>0){
        
        if (length(ess)==length(all.substrates)){
                                        #all substrates are essential
            uptakeKeyRes=growthRate.p/yield[keyResName]
            
            if (length(bio.products)>0){#biomass is in stoichiom so don't remove
                v=stoichiom[rname]/stoichiom[keyResName]*uptakeKeyRes
            }else{#need to remove bac growth as not in stoichiom
                prod.mass=sum(stoichiom[products])*uptakeKeyRes/stoichiom[keyResName]
                scale=(prod.mass-growthRate.p)/prod.mass
                v=scale*stoichiom[rname]/stoichiom[keyResName]*uptakeKeyRes
            }
            
        }else{
            if (length(bio.products)>0){stop('MICROPOP ERROR: All resources must be essential if biomass is included as a product in the stoichiometry')}
                      #remove bac growth
            v=(stoichiom[rname]/sum(stoichiom[products]))*(sum(uptake[all.substrates])-growthRate.p)
        }
        
    }else{
        v=0}
    
    return(max(v,0))
}
