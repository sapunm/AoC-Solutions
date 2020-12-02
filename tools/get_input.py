
import requests
import os.path
from datetime import datetime
from apscheduler.schedulers.blocking import BlockingScheduler

session = requests.Session() 
session.cookies.set('session', '53616c7465645f5fa78d43585e156ce83eae432eb656c3fc4da1d2978e4b504ec6f4730f2d630b1f7dd8884bc21ec49f')

def get_input(path: str, year: int, day: int, time: int, **kwargs):
    file_path = path.format(day=day)
    response = session.get(f'https://adventofcode.com/{year}/day/{day}/input')
    if response.status_code == 200:
        with open(file_path, 'w+') as fd:
            fd.write(response.text)
            print(f'Fetched day {day} input to {file_path}')
    else:
        print(f'Network error: {response.status_code}')


if __name__ == '__main__':
    import argparse

    today = datetime.today()

    parser = argparse.ArgumentParser(description="Get input for AoC puzzle and save to file")    
    parser.add_argument('-p', '--path', type=str, default="input.txt", help='Path to save file in')
    parser.add_argument('-y', '--year', type=int, default=today.year, help='AoC edition year')
    parser.add_argument('-d', '--day', type=int, default=today.day, help='Calendar day')
    parser.add_argument('-t', '--time', type=int, default=6, help='Local time when puzzles are released')
    parser.add_argument('-w', '--watch', default=False, action='store_true', help='Wait in a loop and fetch file')

    args = parser.parse_args()
    
    if not args.watch:
        get_input(**vars(args))
    else:
        sched = BlockingScheduler()
        job = sched.add_job(get_input, 'date', run_date=datetime(today.year, today.month, today.day, args.time, 0, 5), kwargs=vars(args))        
        sched.start()
