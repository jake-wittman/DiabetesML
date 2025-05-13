import pyautogui
import time
import sys
from datetime import datetime
from random import randint
pyautogui.FAILSAFE = False
numMin = None
if ((len(sys.argv)<2) or sys.argv[1].isalpha() or int(sys.argv[1])<1):
    numMin = 3
else:
    numMin = int(sys.argv[1])
while(True):
    numMin = randint(3, 8)
    x=0
    while(x<numMin):
        time.sleep(60)
        x+=1
    # for i in range(0,1):
    #     pyautogui.moveTo(0,i*4)
     
    for i in range(0,15):
        #print(i)
        #pyautogui.press("shift")
        pyautogui.press('f15')
        pyautogui.press('volumeup')
        pyautogui.press('volumedown')
    #print("Movement made at {}".format(datetime.now().time()))
