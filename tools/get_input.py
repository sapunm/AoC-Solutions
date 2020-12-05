from os import environ
from datetime import datetime

from apscheduler.schedulers.blocking import BlockingScheduler
import requests

sched = BlockingScheduler()
session = requests.Session() 
session.cookies.set('session', environ['AOC_SESSION'])

rust_templ = '''
pub type DataType = Vec<_>;
pub type ResultType = usize;

lazy_static! {
    pub static ref DATA: DataType = vec![
        content
    ];
}
'''

def format_content(ext, content: str) -> str:
    if ext == 'rs':
        return rust_templ.replace('content', content.replace('\n', '\n        '))

    return content

def get_input(path: str, year: int, day: int, time: int, **kwargs):
    response = session.get(f'https://adventofcode.com/{year}/day/{day}/input')

    if response.status_code == 200:
        file_path = path.format(day=day)
        file_ext = file_path.split('.')[-1]
        file_content = format_content(file_ext, response.text)

        with open(file_path, 'w+') as fd:
            fd.write(file_content)
            print(f'Fetched day {day} input to {file_path}')
    else:
        print(f'Network error: {response.status_code}')

    if sched.running: 
        sched.shutdown()

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
        job = sched.add_job(get_input, 'date', run_date=datetime(today.year, today.month, today.day, args.time, 0, 5), kwargs=vars(args))        
        sched.start()
