#! /usr/bin/env python

import collections
import os
import subprocess


SimpleTest = collections.namedtuple('SimpleTest', 'name')
ParamTest = collections.namedtuple('ParamTest', 'name runs')
TestRun = collections.namedtuple('TestRun', 'name args')


TESTS = (
    SimpleTest('test_empty'),
    SimpleTest('test_once_kill'),
    SimpleTest('test_twice_kill'),
    SimpleTest('test_stop_resume'),
    ParamTest('test_complete', [
        TestRun('0', ['0']),
        TestRun('1', ['1']),
        TestRun('2', ['2']),
    ]),
)


def run_test(args, ref_file):
    # Run tests under Valgrind
    base_args = args
    args = [
        'valgrind', '-q', '--leak-check=full', '--show-leak-kinds=all',
    ] + base_args

    with open(ref_file, 'rb') as f:
        ref_bytes = f.read()

    with open(os.devnull, 'rb') as devnull:
        with open('tmp.out', 'wb') as temp_file:
            subprocess.check_call(
                args,
                stdin=devnull, stdout=temp_file, stderr=temp_file
            )

    with open('tmp.out', 'rb') as f:
        out_bytes = f.read()

    if out_bytes != ref_bytes:
        print('\x1b[31mDIFF\x1b[0m: {}'.format(' '.join(base_args)))
    else:
        print('\x1b[32mOK\x1b[0m:   {}'.format(' '.join(base_args)))


for test in TESTS:
    base_args = [
        os.path.join('exe', test.name)
    ]
    def get_ref_file(test_name, run_name=None):
        if run_name:
            test_name = '{}_{}'.format(test_name, run_name)
        return os.path.join('ref', test_name)

    if isinstance(test, SimpleTest):
        run_test(base_args, get_ref_file(test.name))
    elif isinstance(test, ParamTest):
        for run in test.runs:
            run_test(
                base_args + run.args,
                get_ref_file(test.name, run.name)
            )
    else:
        raise ValueError()
