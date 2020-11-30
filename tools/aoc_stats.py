
import re
import requests
import os.path
from datetime import datetime
import time

file_name = 'aoc_stats.csv'

if not os.path.isfile(file_name):
    with open(file_name,'x') as fd:
        fd.write('timestamp,' + ','.join([f'day_{day}_gold,day_{day}_silver' for day in range(1, 26)]) + '\n')

while True:
    response = requests.get('https://adventofcode.com/2019/stats')
    if response.status_code == 200:
        stats = reversed(re.findall('>\s*(\d+)\s*<.*stats-both">\s*(\d+)\s*<.*firstonly">\s*(\d+)\s*<', response.text))
        stats = map(lambda x: (int(x[0]), int(x[1]), int(x[2])), stats)
        #data_str = datetime.now().strftime('%Y-%m-%d_%H:%M:%S')
        data_str = time.ctime()
        for data_column in stats:
            data_str += f',{data_column[2]},{data_column[1]}'
        print(data_str)
        with open(file_name,'a') as fd:
            fd.write(f'{data_str}\n')
    else:
        print(f'{data_str} Network error: {response.status_code}')
    time.sleep(60)