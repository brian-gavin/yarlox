#!/usr/bin/env python3

import subprocess
import stat
import os


def run_every_test(path):
    for test in os.listdir(path):
        test_stat = os.stat(os.path.join(path, test))
        if stat.S_ISDIR(test_stat.st_mode):
            run_every_test(os.path.join(path, test))
        else:
            run_test(os.path.join(path, test))


def run_test(path):
    if not path.endswith('.lox'):
        return
    print(f'Running test for {path}')
    try:
        completed = subprocess.run(['cargo', 'run', path], check=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf8')
        report(path, completed)
    except subprocess.CalledProcessError as e:
        print('Error: test', path, 'failed.')
        report(path, e)


def report(path, completed_process):
    with open(f'{path}.report.txt', 'w') as outfile:
        print('=== STDOUT ===', file=outfile)
        print(completed_process.stdout, file=outfile)
        print('=== STDERR ===', file=outfile)
        print(completed_process.stderr, file=outfile)


run_every_test('lox_test')
