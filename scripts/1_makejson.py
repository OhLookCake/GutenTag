# -*- coding: utf-8 -*-
"""
Created on Tue Jun 16 20:00:02 2013

@author: EeshanMalhotra
"""
#import json

datapath="C:/etc/Projects/Data/MovieTaglines/data/taglines2.list"

ctr=0
taglines={}

ins = open(datapath, "r" )

for line in ins:
    ctr+=1
    if ctr%1000==0:
        print ctr
    
    #if ctr>=50:
        #break
    

    if len(line)<=1:
        continue
    if line[0]=="#":
    #It's a title
        movie=line[2:].encode("utf-8")
        if movie[-5:-1]=='(TV)':
            movie=movie[:-6]
        if movie[-4:-1]=='(V)' or movie[-5:-1]=='(VG)' or movie[-2:-1]=='}':
            movie="(V/VG/SUS)"
        else:
            if movie[-1:]=='\n':
                movie=movie[:-1]
#       not removing the year.
        movie=movie.replace("\t"," ")
    else:
    #it's a tagline
        tline=line[1:-1].encode("utf-8")
            
        if movie in taglines:
            taglines[movie]=taglines[movie]+" "+tline
        else:
            taglines[movie]=tline
    


print("Generated")
#print taglines
outfile=open('C:/etc/Projects/Data/MovieTaglines/data/taglines.tsv', 'w')

for i in taglines:
    outfile.write(i+"\t"+taglines[i]+'\n')

print ("End")








