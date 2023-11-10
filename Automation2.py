from os import listdir


from os.path import isfile, join

mypath="D:\RIT Study\Master Project\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\With 350 Dataset\\"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]


files=[i for i in onlyfiles if i.__contains__("Extended") or i.__contains__("Replication")]


# m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
# gr=m$data   
# gr1=gr[gr$effect2__=="No",]   
# k="rater_gender"
# nrc=length(unique(dices$rater_gender))*length(unique(dices[[k]]))  
# gr1=gr[1:nrc,] 
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
            fw.write("ud1=length(unique(dices$rater_gender)) \n ud2=length(unique(dices[[k]])) \n selected_rows_indices <- c() \n i=1 \n for (lo in range(ud1)){ \n hel=c(seq(i, nrow(gr1), by = ud1*ud1)) \n selected_rows_indices <- c(selected_rows_indices, hel) \n i=i+1 \n }")
            fw.write("\n")
            fw.write("gr2=gr1[selected_rows_indices,]")
            fw.write("\n")
            fw.write("plotb=plottingBar(gr2,modelname,k)")
            fw.write("\n")
        else:
            fw.write(line)
    fr.close()
    fw.close()  
    
print(files)

