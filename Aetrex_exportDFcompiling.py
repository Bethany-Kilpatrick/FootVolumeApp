# -*- coding: utf-8 -*-
"""
Created on Thu Jun  9 09:57:37 2022

@author: Ife.Olawore
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 
import csv
import itertools

# This 
fPath = "C:/Users/bethany.kilpatrick/Downloads/"
fileExt = r".csv"
fName = 'datamanagementBOA Technology2025-05-16 09_20_25.csv'


dat = pd.read_csv(fPath + fName, delimiter =',', header = 0) 
df = pd.DataFrame(dat)



# Function to normalize some of the given  metrics
def divide_ten(number):
    return number / 10

df[["Left Dorsal Height","Right Dorsal Height", "Left Arch Height",
"Right Arch Height",  "Left Girth", "Right Girth","Left Instep Width", "Right Instep Width","Left Ball Height", "Right Ball Height","Left Ball Width", "Right Ball Width", 
"Left Ball Girth", "Right Ball Girth","Left Heel Width", "Right Heel Width","Left Short Heel Girth", "Right Short Heel Girth",
"Left Long Heel Girth", "Right Long Heel Girth", "Left Ankle Girth", "Right Ankle Girth","Left Length To First Met Head", 
"Right Length To First Met Head", "Left Length To Fifth Met Head", "Right Length To Fifth Met Head", "Left Max Toe Height", "Right Max Toe Height"]]= df[["Left Dorsal Height","Right Dorsal Height", "Left Arch Height",
"Right Arch Height", "Left Girth", "Right Girth","Left Instep Width", "Right Instep Width", "Left Ball Height", "Right Ball Height","Left Ball Width", "Right Ball Width", 
"Left Ball Girth", "Right Ball Girth","Left Heel Width", "Right Heel Width","Left Short Heel Girth", "Right Short Heel Girth",
"Left Long Heel Girth", "Right Long Heel Girth", "Left Ankle Girth", "Right Ankle Girth","Left Length To First Met Head", 
"Right Length To First Met Head", "Left Length To Fifth Met Head", "Right Length To Fifth Met Head", "Left Max Toe Height", "Right Max Toe Height"

]] .apply(divide_ten)

                                                         
# Replacing values under some columns with the required 
df['Gender'] = df['Gender'].replace(['Male','Female'], ['M', 'F']) 
df['Right Arch Type'] = df['Right Arch Type'].replace(['H','L','M','F'], ['High','Low','Medium', 'Flat'])
df['Left Arch Type'] = df['Left Arch Type'].replace(['H','L','M','F'], ['High','Low','Medium', 'Flat'])

# extracting the date from date-time and duplicating
date = df['Date'].str.split("T", n=11,expand = True)[:][0]
date = pd.DataFrame([date[value//2] for value in range(len(date)*2)])
date.columns = ['Date']

#duplicating some coulumn values
Gender = df['Gender'].iloc[df.index.repeat(2)].reset_index()
size = df['Size'].iloc[df.index.repeat(2)].reset_index(drop=True)

# inserting replications of the side
df.insert(4, 'Left', 'L')
df.insert(5, 'Right', 'R') 


#duplicating some coulumn values
side = df[['Left','Right']].stack().reset_index(drop=True).to_frame('Side')



# extracting the name from email and duplicating

email  = (df['Email'] )
email  = pd.DataFrame([email[values//2] for values in range(len(email)*2)])


subject = (df['Email'].str.split("@",n=1,expand = True)[:][0])
subject = pd.DataFrame([subject[values//2] for values in range(len(subject)*2)])



# subject = subject.replace('dan.feeney', 'Dan Feeney') # Change individual names from username to real name



subject.columns = ['Subject'] 


# separating internal from external
placement = (df['Email'].str.split("@",n=1,expand = True)[:][1]) 

placement = pd.DataFrame([placement[values//2] for values in range(len(placement)*2)])
placement = placement.replace('boatechnology.com', 'Internal')
placement.columns = ['Internal or External']
placement[placement != 'Internal'] = 'External'


# Transposing left and right values for each subject 
sex = df[['Gender']].stack().reset_index(drop=True).to_frame('Sex')

archtype = df[['Left Arch Type','Right Arch Type']].stack().reset_index(drop=True).to_frame('ArchType')
length = df[['Left Length','Right Length']].stack().reset_index(drop=True).to_frame('Length')
width = df[["Left Width","Right Width"]].stack().reset_index(drop=True).to_frame('Width')
instep = df[["Left Dorsal Height","Right Dorsal Height"]].stack().reset_index(drop=True).to_frame('Instep')
archht = df[["Left Arch Height", "Right Arch Height"]].stack().reset_index(drop=True).to_frame('ArchHt')
girth = df[["Left Girth", "Right Girth"]].stack().reset_index(drop=True).to_frame('Girth')
totalarea = df[["Left Total Area", "Right Total Area"]].stack().reset_index(drop=True).to_frame('TotalArea')
archdepth = df[["Left Arch Depth", "Right Arch Depth"]].stack().reset_index(drop=True).to_frame('ArchDepth')

instepWidth = df[["Left Instep Width", "Right Instep Width"]].stack().reset_index(drop=True).to_frame('InstepWidth') 
ballHt = df[["Left Ball Height", "Right Ball Height"]].stack().reset_index(drop=True).to_frame('BallHt') 
ballWidth = df[["Left Ball Width", "Right Ball Width"]].stack().reset_index(drop=True).to_frame('BallWidth') 
ballGirth = df[["Left Ball Girth", "Right Ball Girth"]].stack().reset_index(drop=True).to_frame('BallGirth')
heelWidth = df[["Left Heel Width", "Right Heel Width"]].stack().reset_index(drop=True).to_frame('HeelWidth')
shortHeelGirth = df[["Left Short Heel Girth", "Right Short Heel Girth"]].stack().reset_index(drop=True).to_frame('ShortHeelGirth')
longHeelGirth = df[["Left Long Heel Girth", "Right Long Heel Girth"]].stack().reset_index(drop=True).to_frame('LongHeelGirth') 
ankleGirth = df[["Left Ankle Girth", "Right Ankle Girth"]].stack().reset_index(drop=True).to_frame('AnkleGirth')  
lengthFirstMetHead = df[["Left Length To First Met Head", "Right Length To First Met Head"]].stack().reset_index(drop=True).to_frame('LengthToFirstMetHead') 
lengthFifthMetHead = df[["Left Length To Fifth Met Head", "Right Length To Fifth Met Head"]].stack().reset_index(drop=True).to_frame('LengthToFifthMetHead') 
maxToeHt = df[["Left Max Toe Height", "Right Max Toe Height"]].stack().reset_index(drop=True).to_frame('MaxToeHeight')   

repShoeSize = df[[]].stack().reset_index(drop=True).to_frame('ReportedShoeSize')  
Sub_numb = df[[]].stack().reset_index(drop=True).to_frame('Sub_numb') 
CadFileMade = df[[]].stack().reset_index(drop=True).to_frame('CadFileMade') 
Subject = df[[]].stack().reset_index(drop=True).to_frame('Subject') 



euSize= df[['European Shoe Size']].stack().reset_index(drop=True).to_frame('EUsize') 
euSize= df['European Shoe Size'].iloc[df.index.repeat(2)].reset_index(drop=True)

# compute width_length
width_length = []
for val1 in range(0, len(length.index)):
    for val2 in range(0, len(length.columns)):
       width_length.append(width.values[val1,val2]/length.values[val1, val2])
width_length = pd.DataFrame(width_length)
width_length.columns = ['Width_Length ']

# merging the columns to produce final output
merge  = [date,placement,subject,email,CadFileMade,Gender,side,archtype,length,width,instep,archht,girth,totalarea,archdepth,
          size,repShoeSize, euSize, width_length, 
          instepWidth,ballHt, ballWidth, ballGirth,heelWidth, shortHeelGirth,longHeelGirth,ankleGirth, lengthFirstMetHead, 
          lengthFifthMetHead,maxToeHt]  



data = pd.concat(merge,axis = 1)  # trouble maker line, making it unable to keep the line above it
data.set_index('index', inplace = True)  

data = data.sort_index(axis=0, ascending=False)



data.to_csv('C://Users//bethany.kilpatrick//Downloads//Aetrex_export_May16.csv')
