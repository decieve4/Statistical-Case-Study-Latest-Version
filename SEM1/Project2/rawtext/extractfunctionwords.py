import os
from os import listdir
import os.path
import re
import string
import collections
import math
from collections import Counter


frequentwordsfile = "/Users/rosss/essays/wordfile.txt" #function word file to use

#uncomment this to do the human essays
inputdirectory = "/Users/rosss/essays/rawtext/humanessays/"
outputdir =   "/Users/rosss/essays/humanfunctionwords//"

#uncomment this to do the chatGPT essays
#inputdirectory = "/Users/rosss/essays/rawtext/GPTessays/"
#outputdir =   "/Users/rosss/essays/GPTfunctionwords//"

frequentwords = []

fin = open(frequentwordsfile,'r')
for line in fin:
	frequentwords.append(line.split(',')[0].strip())


def only_printable_ascii(string):
    ''' Returns the string without non-ASCII characters and non-printable ASCII'''
    stripped = (c for c in string if 31 < ord(c) < 127)
    return ''.join(stripped)
    

#takes a chunk of words and returns vectors containing the counts of word lenghts, function words, common words etc
def createvector(wordlist):
	wordcounter = Counter()
	numwords = 0
	for word in wordlist:	
		if len(word) > 0:	
			wordcounter[word] += 1
			numwords+=1
		else:
			print('Error: zero length words')		
		
	frequentwordsvec = []
	frequentwordscount = 0
	
	for word in frequentwords:
		frequentwordsvec.append(wordcounter[word])
		frequentwordscount = frequentwordscount+wordcounter[word]
		
	frequentwordsvec.append(numwords-frequentwordscount) #number of nonfunction words
	return (frequentwordsvec)



print("Processing files...")
if not os.path.exists(outputdir):
	os.makedirs(outputdir)
		
directories = listdir(inputdirectory)
for dirname in directories:
	if (dirname == ".DS_Store"):
		continue

	print(dirname)
	if not os.path.exists(outputdir+dirname):
		os.makedirs(outputdir+dirname)
	mydir = inputdirectory+dirname+"/"
	files = listdir(mydir)
	files.sort() #puts unknown at the end
		
	for text in files:
		outputfile = open(outputdir+dirname+'/'+text +" --- functionwords.txt", 'w')
		
		with open(inputdirectory + dirname + '/' +  text, 'r') as myfile:
			
			allwords = []
			
			for line in myfile:
				line = only_printable_ascii(line)											#strips other non-english characters
				line =  re.sub('[,\.]',' ',line)											#replace . and , with spaces
				line =  line.translate(string.maketrans("",""), string.punctuation) 		#delete all remaining punctuation										
				line = line.replace('\n', ' ').replace('\r', '')							#replaces new line with space									
				line = ' '.join(line.split())												#replace multiple spaces with a single one
				line = line.lower()		
			
				words = line.split(" ")													#break the line down into individual words by splitting on spaces
				for word in words:		
					word = word.replace(" ", "")										
					word = word.translate(None, string.whitespace)
					if word=='' or len(word)<1: 										#if we are left with an empty word after stripping out bad characrers, then throw it away
						continue		
					allwords.append(word)

			frequentwordsvec = createvector(allwords)		
			outputfile.write(str(frequentwordsvec).strip('[]')  + "\n")

    
	
	
	