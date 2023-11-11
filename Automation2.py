from os import listdir


from os.path import isfile, join

mypath="D:\RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 350 Dataset\\"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]


files=[i for i in onlyfiles if i.__contains__("Extended") or i.__contains__("Replication")]


# m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
# gr=m$data   
# gr1=gr[gr$effect2__=="No",]   
# k="rater_gender"
#gr1 <- subset(gr1, select = -cond__)
# gr2=distinct(gr1)
# plotb=plottingBar(gr1,modelname,k)

for f in files:
    fp="D:\RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 350 Dataset\\"+f
    fr=open(fp,"r")
    fw=open(f,"w")
    k=0
    for line in fr:
        if "m=plot(mod_plot)[[1]]+facet_wrap"in line or "m=plot(mod_plot)[[1]]+ facet_wrap" in line  or "m=plot(mod_plot)[[1]] + facet_wrap" in line or "m=plot(mod_plot)[[1]] +facet_wrap" in line:
            k=k+1
            print(k)
            fw.write(line)
            fw.write("\n")
            fw.write("gr=m$data")
            fw.write("\n")
            fw.write("gr1=gr[gr$effect2__==\"No\",]")
        elif "plotb=plottingBar(" in line:
            link=line.replace("plotb=plottingBar(m,modelname,\"","")
            link=link.replace("\")","")
            link=link.strip()
            fw.write("\n")
            fw.write("k=\""+link+"\"")
            fw.write("\n")
            # fw.write("#nrc=length(unique(dices$rater_gender))*length(unique(dices[[k]]))")
            fw.write("\n")
            fw.write("gr1 <- subset(gr1, select = -cond__) \n gr2=distinct(gr1)")
            fw.write("\n")
            fw.write("plotb=plottingBar(gr2,modelname,k)")
            fw.write("\n")
        else:
            fw.write(line)
    fr.close()
    fw.close()  
    
print(files)

