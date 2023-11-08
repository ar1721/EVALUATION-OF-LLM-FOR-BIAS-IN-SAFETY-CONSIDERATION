from os import listdir

from os.path import isfile, join

mypath="D:\RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 990 and 350 Dataset\Code_Run"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]

print(onlyfiles[0])
for rfiles in onlyfiles:
    filep="D:/RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 990 and 350 Dataset\Code_Run/"+rfiles
    f = open(filep,'r')
    fw=open(rfiles,'w')
    for line in f:
        if line.__contains__("tidybayes"):
            fw.write("#library(tidybayes)\n")
        elif(line.__contains__("formula") and line.__contains__("rater_raw_race")):
            line=line.replace("rater_raw_race","rater_ethinicity")
            print(line)
            fw.write(line)
        else:
            fw.write(line)
            

    # Close opend file
    f.close()
    fw.close()
