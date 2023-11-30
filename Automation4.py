import os
from PIL import Image

# path ="D:\\RIT Study\\Master Project\\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\\With 990 and 350 Dataset\\Overall"
jpegfiles=[]
# for root, dirs, files in os.walk(path):
# 	for file in files:
# 		if(file.endswith(".jpeg")):
# 			jpegfiles.append(os.path.join(root,file))
                    
# path ="D:\\RIT Study\\Master Project\\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\\With 990 and 350 Dataset\\HarmfullContent"

# for root, dirs, files in os.walk(path):
# 	for file in files:
# 		if(file.endswith(".jpeg")):
# 			jpegfiles.append(os.path.join(root,file))

path ="D:\\RIT Study\\Master Project\\EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION\\With 990 and 350 Dataset\\UnfairBias"

for root, dirs, files in os.walk(path):
	for file in files:
		if(file.endswith(".jpeg")):
			jpegfiles.append(os.path.join(root,file))
                    

for i in jpegfiles:
    print(os.getcwd())
    image = Image.open(i)

    image.save(i,
                    "JPEG",
                    optimize = True,
                    quality = 10)
