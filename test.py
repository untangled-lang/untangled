import os

os.system("make")
os.system("./toplevel.native < ./test/integer.txt > ./result/integer.out")
