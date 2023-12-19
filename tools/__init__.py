from pathlib import Path
import os
import logging
import subprocess
import glob
from functools import wraps


targets = []
target_by_name = {}
target_by_product = {}

class Target:
    def __init__(self, name):
        self.name = name
        self.sources      = []
        self.products     = []
        self.dependencies = []
        self.actions      = []

        global targets
        assert self.name not in target_by_name,\
            "Duplicate Target Name: " + name
        target_by_name[name] = self
        targets.append(self)

    def add_product(self, path):
        path = Path(path)
        assert path not in target_by_product,\
            "Duplicate Product Path: " + str(path)
        target_by_product[path] = self
        self.products.append(path)

    def add_source(self, path):
        path = Path(path)

        if path in self.sources:
            return

        self.sources.append(path)

        if str(path).startswith("build"):
            assert path in target_by_product,\
                f"No target known to produce {path}"
            self.dependencies.append(target_by_product[path])

    def execute(self, force=0):
        for dep in self.dependencies:
            dep.execute(force - 1)

        doit = False
        for p in self.products:
            if not p.exists():
                doit = True

        if not doit and force == 0:
            return

        logging.info(f"Target {self.name}: execute {len(self.actions)} commands")
        for action in self.actions:
            # Execute all steps of the action
            for step in action:
                pass

    def __rshift__(self, op):
        op(self)
        return self

    @staticmethod
    def operation(func):
        @wraps(func)
        def wrapit(*args, **kwargs):
            def doit(target):
                ret = func(target, *args, **kwargs)
                if ret:
                    next(ret)
                    target.actions.append(ret)
            return doit
        return wrapit

@Target.operation
def depends(target, other_target):
    print(f"depends {target.name} -> {other_target.name}")
    target.dependencies.append(other_target)
    yield
    print("depend execute")

@Target.operation
def mkdir(target, *directories):
    directories = [Path(d) for d in directories]
    for d in directories:
        target.add_product(d)

    yield

    for d in directories:
        logging.debug(f"mkdir: {d}")
        d.mkdir(parents=True, exist_ok=True)


def rsync(source, destination, recursive=False):
    def doit(target):
        if not recursive:
            target.add_source(source)
            target.add_product(destination)
            target.actions.append(["rsynv", source, destination])
        else:
            src_path = Path(source)
            dst_path = Path(destination)
            for f in src_path.glob("**/*"):
                target.add_source(f)
                target.add_product(dst_path / f.relative_to(src_path))
            target.actions.append(["rsync", "-av", source, destination])


    return doit

def main(argv):
    import argparse
    parser = argparse.ArgumentParser(description='Build a Lecture',
                                     formatter_class=argparse.RawTextHelpFormatter)
    target_help = sorted([f" - {t.name}" for t in targets])
    parser.add_argument('target', help="Which target to build? Choices:\n"
                        + "\n".join(target_help), nargs="*",
                        metavar='TARGET', default="all",
                        choices=[t.name for t in targets])
    parser.add_argument('-f', "--force",
                        help="Force build", default=0, type=int)
    parser.add_argument('-v', "--verbose", help="Verbose", default=False, action="store_true")
    opts = parser.parse_args(argv)

    if opts.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)


    target_by_name[opts.target].execute(
        force=opts.force
    )
