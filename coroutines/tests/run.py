#! /usr/bin/env python

import argparse
import collections
import os
import subprocess


SimpleTest = collections.namedtuple('SimpleTest', 'name')
ParamTest = collections.namedtuple('ParamTest', 'name runs')
TestRun = collections.namedtuple('TestRun', 'name args')


TESTS = (
    SimpleTest('test_empty'),

    SimpleTest('test_kill_main'),
    SimpleTest('test_kill_twice'),
    SimpleTest('test_foreign_kill'),

    SimpleTest('test_delegate_nonlocal_ref'),

    SimpleTest('test_spawn_twice'),
    SimpleTest('test_spawn_exit'),
    SimpleTest('test_spawn_kill'),
    SimpleTest('test_spawn_switch_exit'),
    SimpleTest('test_spawn_switch_kill'),

    SimpleTest('test_switch_dead'),
    SimpleTest('test_switch_self'),

    SimpleTest('test_resume_simple'),
    SimpleTest('test_resume_chained'),
    SimpleTest('test_resume_parent_dead'),

    SimpleTest('test_reference_loop'),
    SimpleTest('test_secondary_stack'),
)


parser = argparse.ArgumentParser()
parser.add_argument(
    '-v', '--valgrind', action='store_true',
    help='Enable memory checks with Valgrind'
)
parser.add_argument(
    'pattern', default=None, nargs='?',
    help='Specify what tests to run'
)


def run_test(args, base_args, ref_file):
    if args.valgrind:
        args = [
            'valgrind', '-q', '--leak-check=full', '--show-leak-kinds=all',
        ] + base_args
    else:
        args = list(base_args)

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


def main(args):
    for test in TESTS:
        if args.pattern and args.pattern not in test.name:
            continue

        base_args = [
            os.path.join('exe', test.name)
        ]
        def get_ref_file(test_name, run_name=None):
            if run_name:
                test_name = '{}_{}'.format(test_name, run_name)
            return os.path.join('ref', test_name)

        if isinstance(test, SimpleTest):
            run_test(args, base_args, get_ref_file(test.name))
        elif isinstance(test, ParamTest):
            for run in test.runs:
                run_test(
                    args,
                    base_args + run.args,
                    get_ref_file(test.name, run.name)
                )
        else:
            raise ValueError()


if __name__ == '__main__':
    args = parser.parse_args()
    main(args)
