import numpy as np
import pandas as pd

# Convert IP into city name
# Base on the city name convert into North(1), Middel(2), South(3), East(4), and others(0) 
north = ['Keelung','Keelung City','Zhubei','Taipei','New Taipei City',' Taipei County','Taoyuan District','Hsinchu','Yilan County','Yilan']
middle = ['Miaoli','Miaoli City','Yuanlin','Toufen Township','Taichung','Taichung City','Huwei','Nantou City','Puli Town','Douliu','Chang-hua','Yunlin County']
south = ['Chiayi City','Tainan City','Kaohsiung City','Pingtung City']
east = ['Hualien City','Hualien County','Taitung County','Taitung City']

file_name = './3_5_content.json' # 1. Choose the file. Must run "IP_to_Location5" FIRST!!
ptt = pd.read_json(file_name)
city_list = list(ptt['City'])

area_code = []
for city in city_list:
    if city in north:
        area_code.append(1)
    elif city in middle:
        area_code.append(2)
    elif city in south:
        area_code.append(3)
    elif city in east:
        area_code.append(4)
    else:
        area_code.append(0)

ptt['Area'] = area_code

with open('3_5_test_content.json'.format(file_name), 'w', encoding='utf-8') as file: # Change the output file name
    ptt.to_json(file, force_ascii=False, orient='records')
print('Finished.')
