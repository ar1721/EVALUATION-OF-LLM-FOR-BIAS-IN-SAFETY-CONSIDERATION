from os import listdir

from os.path import isfile, join

mypath="D:\RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 990 and 350 Dataset\Code_Run"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.__contains__("Unfair")]
# onlyfiles=onlyfiles[0:1]
linno=1
print(onlyfiles)
for rfiles in onlyfiles:
    filep="D:/RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 990 and 350 Dataset\Code_Run/"+rfiles
    f = open(filep,'r')
    fw=open(rfiles,'w')
    linno=0
    for line in f:
        if linno>=38 and linno <=55:
            print(linno)
            print(line)
            linno=linno+1
        elif line.__contains__("colnm3<-c(\"rater_id\","):
            print("change")
            fw.write("\ncolnm3<-c(\"rater_id\",\"rater_gender\",\"rater_race\",\"rater_raw_race\",\"rater_age\",\"phase\",\"rater_education\",\"item_id\",\"degree_of_harm\",\"Q3_bias_overall\")")
            linno=linno+1
        elif line.__contains__("colnm2<-c(\"rater_id\",\"rater_gender\","):   
            print("change")
            fw.write("\ncolnm2<-c(\"rater_id\",\"rater_gender\",\"rater_race\",\"rater_raw_race\",\"rater_age\",\"phase\",\"rater_education\",\"item_id\",\"degree_of_harm\",\"Q3_unfair_bias_overall\")")
            linno=linno+1
        else:
            fw.write(line)
            linno=linno+1
            

    # Close opend file
    f.close()
    fw.close()